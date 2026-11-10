;;; efrit-tool-decisions.el --- The decide tool and the efrit-decide-lines command -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.17.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; Two faces of `efrit-decisions':
;;
;; - The `decide' tool: the model hands over a list of inputs and the
;;   questions, gets one row of answers per input.  Labeling fifty
;;   headlines costs fifty fast calls to the decision model instead of
;;   fifty rounds of its own guessing; the answers are numbers it can
;;   sort and threshold.  `exec' class, so the reviewer sees the
;;   questions; the sandbox asks for the endpoint's host under `net'
;;   (efrit's own gateways are expected).
;;
;; - `efrit-decide-lines': the user's command over the region, one
;;   input per line, questions from a questions file or typed once;
;;   the answers come as a `tabulated-list' with the inputs, sortable
;;   by any column, `w' copies the table as TSV.
;;
;; The tool registers only when a decision backend exists.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'efrit-decisions)
(require 'efrit-tool-registry)
(require 'efrit-log)
(require 'efrit-config)

(declare-function efrit-sandbox-check "efrit-sandbox" (cap target &optional tool detail))
(defvar efrit-sandbox-enabled)

;;;; Shared

(defun efrit-tool-decisions--host (backend)
  (or (url-host (url-generic-parse-url (efrit-decisions-backend-url backend))) "unknown-host"))

(defun efrit-tool-decisions--check (backend count)
  "Ask the sandbox for BACKEND's host on behalf of the decide tool over COUNT inputs."
  (when (bound-and-true-p efrit-sandbox-enabled)
    (require 'efrit-sandbox)
    (efrit-sandbox-check 'net (cons 'host (efrit-tool-decisions--host backend)) "decide"
                         (format "decide with %s over %d input%s" (efrit-decisions-backend-name backend)
                                 count (if (= count 1) "" "s")))))

(defun efrit-tool-decisions--list (value)
  "VALUE (a vector, list or hash table from the tool input) as a list."
  (cond ((vectorp value) (append value nil))
        ((hash-table-p value) (let (out) (maphash (lambda (k v) (push (cons k v) out)) value) (nreverse out)))
        ((listp value) value)
        (t (list value))))

(defun efrit-tool-decisions--get (obj key)
  "Member KEY of OBJ, a hash table or an alist with string keys."
  (cond ((hash-table-p obj) (gethash key obj))
        ((listp obj) (alist-get key obj nil nil #'equal))
        (t nil)))

(defun efrit-tool-decisions--option (item key)
  "ITEM, a string or an object with KEY and \"description\", as a labelled pair."
  (if (stringp item)
      item
    (cons (format "%s" (efrit-tool-decisions--get item key))
          (or (efrit-tool-decisions--get item "description") ""))))

(defun efrit-tool-decisions-questions-from-input (value)
  "The question plists for VALUE, the tool's \"questions\" array."
  (mapcar (lambda (q)
            (let ((type (intern (or (efrit-tool-decisions--get q "type") "predicate"))))
              (append
               (list :type type
                     :name (efrit-tool-decisions--get q "name")
                     :instructions (efrit-tool-decisions--get q "instructions"))
               (pcase type
                 ('choice (list :choices (mapcar (lambda (c) (efrit-tool-decisions--option c "value"))
                                                 (efrit-tool-decisions--list (efrit-tool-decisions--get q "choices")))))
                 ('score (list :levels (mapcar (lambda (c) (efrit-tool-decisions--option c "label"))
                                               (efrit-tool-decisions--list (efrit-tool-decisions--get q "levels")))))
                 (_ nil)))))
          (efrit-tool-decisions--list value)))

(defun efrit-tool-decisions--row (input answers err)
  "One result line for the tool: INPUT then the answers, or the error."
  (format "%s\t%s"
          (truncate-string-to-width (replace-regexp-in-string "[\n\t]+" " " input) 120 nil nil "…")
          (if err
              (format "ERROR: %s" err)
            (mapconcat (lambda (a) (format "%s=%s" (alist-get 'name a) (efrit-decisions-format-answer a)))
                       answers "\t"))))

(defun efrit-tool-decisions-decide (input)
  "The `decide' tool over INPUT: inputs[], questions[], optional backend."
  (let* ((inputs (mapcar (lambda (x) (format "%s" x))
                         (efrit-tool-decisions--list (alist-get "inputs" input nil nil #'equal))))
         (questions (efrit-tool-decisions-questions-from-input (alist-get "questions" input nil nil #'equal)))
         (backend (efrit-decisions-resolve-backend (alist-get "backend" input nil nil #'equal))))
    (when (null inputs) (signal 'user-error (list "decide: inputs is required (an array of texts)")))
    (efrit-decisions-check-questions questions)
    (efrit-tool-decisions--check backend (length inputs))
    (let* ((started (float-time))
           (results (efrit-decide-many-sync inputs questions :backend backend))
           (failed (cl-count-if (lambda (r) (nth 2 r)) results)))
      (concat
       (format "%d input%s, %d question%s, %s, %.1f s%s\n"
               (length inputs) (if (= 1 (length inputs)) "" "s")
               (length questions) (if (= 1 (length questions)) "" "s")
               (efrit-decisions-backend-name backend) (- (float-time) started)
               (if (> failed 0) (format ", %d failed" failed) ""))
       "Columns: input, then NAME=ANSWER per question. predicate: yes/no with probability; choice: the value with its probability; score: the nearest level with the probability-weighted index (0 = first level).\n"
       (mapconcat (lambda (r) (apply #'efrit-tool-decisions--row r)) results "\n")))))

(defun efrit-tool-decisions--schema ()
  `(("type" . "object")
    ("properties"
     . (("inputs" . (("type" . "array") ("items" . (("type" . "string")))
                     ("description" . "The texts to decide about, one answer row each. Up to a few hundred.")))
        ("questions"
         . (("type" . "array")
            ("description" . "The questions, asked of every input. Each: type (predicate | choice | score), name, instructions; choice adds choices [{value, description}]; score adds levels [{label, description}] low to high.")
            ("items" . (("type" . "object")
                        ("properties" . (("type" . (("type" . "string") ("enum" . ["predicate" "choice" "score"])))
                                         ("name" . (("type" . "string")))
                                         ("instructions" . (("type" . "string")))
                                         ("choices" . (("type" . "array")))
                                         ("levels" . (("type" . "array")))))
                        ("required" . ["type" "name" "instructions"])))))
        ("backend" . (("type" . "string")
                      ("description" . ,(format "Which decision model: one of %s. Default: the first."
                                                (mapconcat #'efrit-decisions-backend-name efrit-decisions-backends ", ")))))))
    ("required" . ["inputs" "questions"])))

(defun efrit-tool-decisions--description ()
  (format "Ask a decision model fixed questions about many texts at once and get numbers back: a probability for a yes/no question, a choice with probabilities, or a score on a labelled scale. Fast (well under a second per text) and consistent; use it to label, sort, filter or triage a list (headlines, messages, log lines, items) instead of judging each yourself. Models: %s."
          (mapconcat #'efrit-decisions-backend-name efrit-decisions-backends ", ")))

;;;###autoload
(defun efrit-decisions-ensure-tools ()
  "Register the decide tool when a decision backend exists; withdraw it when none.  Idempotent."
  (interactive)
  (if efrit-decisions-backends
      (efrit-register-tool "decide" :description (efrit-tool-decisions--description)
                           :input-schema (efrit-tool-decisions--schema)
                           :function #'efrit-tool-decisions-decide :class 'exec :package 'efrit-decisions)
    (efrit-unregister-tool "decide"))
  (when (called-interactively-p 'any)
    (message "efrit-decisions: %d backend(s); decide tool %s" (length efrit-decisions-backends)
             (if (efrit-registered-tool-get "decide") "registered" "withdrawn"))))

(advice-add 'efrit-decisions-add-backend :after (lambda (&rest _) (efrit-decisions-ensure-tools))
            '((name . efrit-tool-decisions--after-add)))
(advice-add 'efrit-decisions-remove-backend :after (lambda (&rest _) (efrit-decisions-ensure-tools))
            '((name . efrit-tool-decisions--after-remove)))

(efrit-decisions-ensure-tools)

;;;; The command: lines of the region, a table of answers

(defcustom efrit-decisions-questions-file
  (expand-file-name "decisions.el" efrit-data-directory)
  "A file of named question sets for `efrit-decide-lines'.
Its contents are read as one Lisp form: an alist of (NAME . QUESTIONS),
QUESTIONS a list of question plists (see `efrit-decisions-question').
`efrit-decide-lines' offers the names, or a question typed once."
  :type 'file
  :group 'efrit-decisions)

(defvar efrit-decisions-questions-history nil)

(defun efrit-decisions-question-sets ()
  "The named question sets from `efrit-decisions-questions-file', or nil."
  (when (file-readable-p efrit-decisions-questions-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents efrit-decisions-questions-file)
          (read (current-buffer)))
      (error (efrit-log 'warn "decisions: cannot read %s: %s" efrit-decisions-questions-file
                        (error-message-string err))
             nil))))

(defun efrit-decisions-read-questions ()
  "Ask for a question set: a named one from the file, or one question typed."
  (let* ((sets (efrit-decisions-question-sets))
         (choice (completing-read "Questions (a set name, or type a yes/no question): "
                                  (mapcar #'car sets) nil nil nil 'efrit-decisions-questions-history)))
    (or (alist-get choice sets nil nil #'equal)
        (progn
          (when (string-empty-p (string-trim choice)) (user-error "No question"))
          (list (efrit-decisions-question 'predicate "answer" choice))))))

(defvar efrit-decisions-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "w") #'efrit-decisions-results-copy)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode efrit-decisions-results-mode tabulated-list-mode "Decisions"
  "Answers of a decision model, one input per row.
\\{efrit-decisions-results-mode-map}"
  (setq tabulated-list-padding 1))

(defun efrit-decisions-results-copy ()
  "Copy the table as tab-separated text."
  (interactive)
  (let ((lines (list (mapconcat (lambda (c) (car c)) tabulated-list-format "\t"))))
    (dolist (e tabulated-list-entries)
      (push (mapconcat #'substring-no-properties (cadr e) "\t") lines))
    (kill-new (string-join (nreverse lines) "\n"))
    (message "Copied %d rows" (length tabulated-list-entries))))

(defun efrit-decisions--cell (answer)
  "ANSWER for a table cell: the number alone sorts; the label helps the eye."
  (pcase (alist-get 'type answer)
    ('predicate (format "%.2f" (alist-get 'probability answer)))
    ('choice (format "%s %.2f" (alist-get 'choice answer) (or (alist-get 'probability answer) 0)))
    ('score (format "%.2f %s" (alist-get 'score answer) (alist-get 'level answer)))
    (_ "?")))

(defun efrit-decisions-show-results (results questions &optional title)
  "Show RESULTS ((INPUT ANSWERS ERROR) ...) for QUESTIONS in a table buffer."
  (let ((buf (get-buffer-create (or title "*efrit decisions*"))))
    (with-current-buffer buf
      (efrit-decisions-results-mode)
      (setq tabulated-list-format
            (vconcat (list (list "Input" 60 t))
                     (mapcar (lambda (q) (list (plist-get q :name) 16 t)) questions)))
      (setq tabulated-list-entries
            (cl-loop for r in results for i from 0
                     collect (pcase-let ((`(,input ,answers ,err) r))
                               (list i (vconcat
                                        (list (replace-regexp-in-string "[\n\t]+" " " input))
                                        (if err
                                            (cons (concat "ERROR: " err)
                                                  (make-list (max 0 (1- (length questions))) ""))
                                          (mapcar #'efrit-decisions--cell answers)))))))
      (tabulated-list-init-header)
      (tabulated-list-print t))
    (pop-to-buffer buf)
    buf))

;;;###autoload
(defun efrit-decide-lines (start end questions &optional backend)
  "Ask QUESTIONS about every non-blank line of START..END; show a table.
Interactively the region (else the buffer), the questions from
`efrit-decisions-questions-file' or one typed, and with a prefix
argument the backend."
  (interactive
   (let ((bounds (if (use-region-p) (cons (region-beginning) (region-end)) (cons (point-min) (point-max)))))
     (list (car bounds) (cdr bounds)
           (efrit-decisions-read-questions)
           (and current-prefix-arg
                (completing-read "Decision backend: "
                                 (mapcar #'efrit-decisions-backend-name efrit-decisions-backends) nil t)))))
  (let ((lines (seq-remove #'string-empty-p
                           (mapcar #'string-trim (split-string (buffer-substring-no-properties start end) "\n")))))
    (when (null lines) (user-error "No lines to decide about"))
    (message "efrit-decisions: asking about %d line%s…" (length lines) (if (= 1 (length lines)) "" "s"))
    (let ((results (efrit-decide-many-sync lines questions :backend backend
                                           :progress (lambda (d n) (when (zerop (% d 10)) (message "efrit-decisions: %d/%d" d n))))))
      (efrit-decisions-show-results results questions)
      (message "efrit-decisions: %d line%s decided" (length lines) (if (= 1 (length lines)) "" "s")))))

(provide 'efrit-tool-decisions)

;;; efrit-tool-decisions.el ends here
