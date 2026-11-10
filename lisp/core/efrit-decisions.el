;;; efrit-decisions.el --- Structured decisions from a decision model -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.17.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; A decision model answers fixed questions about a text and nothing
;; else: is this true (a probability), which of these (a choice with
;; probabilities), how much on this scale (a score).  One request, one
;; input, several questions, an answer per question, in a few hundred
;; milliseconds.  Where efrit would otherwise ask the chat model for
;; a label and parse prose, this gives a number.
;;
;; `efrit-decide' sends INPUT and QUESTIONS to a backend and returns
;; the answers; `efrit-decide-async' does the same with a callback;
;; `efrit-decide-many' runs one request per input, in parallel up to a
;; limit, and collects.  Questions are plists:
;;
;;   (:type predicate :name "urgent"
;;    :instructions "Does this need an answer today?")
;;   (:type choice :name "kind" :instructions "What is it?"
;;    :choices (("bug" . "something is broken") ("ask" . "a request") "other"))
;;   (:type score :name "mood" :instructions "How does the writer feel?"
;;    :levels ("angry" "neutral" "happy"))
;;
;; An answer is an alist with the question's name and type, and:
;; `probability' for a predicate; `choice' and `probabilities' (an
;; alist value -> p) for a choice; `score' (the probability-weighted
;; level index, so 0.0 .. N-1 continuous) and `level' (the nearest
;; label) for a score.  Raw fields the endpoint adds are kept under
;; `raw'.
;;
;; Backends.  `efrit-decisions-add-backend' names an endpoint and a
;; model; the first is the default.  One protocol ships,
;; `decisions-v1': POST {"model","input","questions"} and read
;; {"answers":[...]} in the questions' order.  A package adds a
;; protocol with `efrit-decisions-request' and `efrit-decisions-parse'
;; methods.  Auth is efrit's API key by default, or an auth-source
;; host.  No defaults for URLs or models: they belong to whoever runs
;; the gateway.
;;
;; Not used by the sandbox or the reviewer, by design: that path stays
;; an independent general model (tzz, 2026-11-10).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'efrit-log)
(require 'efrit-http)

(defgroup efrit-decisions nil
  "Structured decisions from a decision model."
  :group 'efrit
  :prefix "efrit-decisions-")

(defcustom efrit-decisions-backends nil
  "The decision backends, as `efrit-decisions-backend' structs.
Add one with `efrit-decisions-add-backend'; for example

  (efrit-decisions-add-backend
   :name \"fast\" :protocol \\='decisions-v1
   :url \"https://gateway.example.com/v1/decisions\"
   :model \"azure/Example-Decision-1\")

The first backend is the default for `efrit-decide'."
  :type '(repeat sexp))

(defcustom efrit-decisions-timeout 60
  "Seconds one decision request may take."
  :type 'integer)

(defcustom efrit-decisions-parallel 4
  "Requests `efrit-decide-many' keeps in flight at once."
  :type 'integer)

(cl-defstruct (efrit-decisions-backend (:constructor efrit-decisions-backend--create))
  "How to reach one decision model."
  name        ; string
  protocol    ; symbol: decisions-v1, ...
  url         ; string: the endpoint
  model       ; string: the model name sent in the body
  auth        ; nil: efrit's API key; a string: an auth-source host; a list or function: headers
  headers     ; function of no arguments returning extra headers, or nil
  options)    ; alist merged into the request body

(cl-defun efrit-decisions-add-backend (&key name protocol url model auth headers options)
  "Add or replace the decision backend NAME in `efrit-decisions-backends'."
  (unless (and (stringp name) (not (string-empty-p name)))
    (error "efrit-decisions: a backend needs a name"))
  (unless (and (symbolp protocol) protocol)
    (error "efrit-decisions: %s: protocol must be a symbol" name))
  (unless (and (stringp url) (string-match-p "\\`https?://" url))
    (error "efrit-decisions: %s: url must be an http(s) URL" name))
  (unless (and (stringp model) (not (string-empty-p model)))
    (error "efrit-decisions: %s: model is required" name))
  (let ((backend (efrit-decisions-backend--create
                  :name name :protocol protocol :url url :model model
                  :auth auth :headers headers :options options)))
    (setq efrit-decisions-backends
          (append (cl-remove name efrit-decisions-backends
                             :key #'efrit-decisions-backend-name :test #'equal)
                  (list backend)))
    backend))

(defun efrit-decisions-remove-backend (name)
  "Remove the decision backend NAME.  Returns non-nil when there was one."
  (prog1 (efrit-decisions-backend name)
    (setq efrit-decisions-backends
          (cl-remove name efrit-decisions-backends :key #'efrit-decisions-backend-name :test #'equal))))

(defun efrit-decisions-backend (name)
  "The decision backend called NAME, or nil."
  (seq-find (lambda (b) (equal (efrit-decisions-backend-name b) name)) efrit-decisions-backends))

(defun efrit-decisions-available-p ()
  "Non-nil when a decision backend is configured."
  (and efrit-decisions-backends t))

(defun efrit-decisions-resolve-backend (&optional name)
  "The backend NAME, or the default; a `user-error' names the fix when none."
  (cond
   ((efrit-decisions-backend-p name) name)
   ((and name (not (string-empty-p name)))
    (or (efrit-decisions-backend name)
        (user-error "efrit-decisions: no backend named %s (known: %s)" name
                    (or (mapconcat #'efrit-decisions-backend-name efrit-decisions-backends ", ") "none"))))
   ((car efrit-decisions-backends))
   (t (user-error "efrit-decisions: no decision backend configured; see `efrit-decisions-backends'"))))

;;;; Questions

(defconst efrit-decisions-question-types '(predicate choice score)
  "The kinds of question a decision model answers.")

(defun efrit-decisions-question (type name instructions &rest args)
  "A question plist of TYPE with NAME and INSTRUCTIONS.
ARGS: `:choices' for a choice, each a string or (VALUE . DESCRIPTION);
`:levels' for a score, each a string or (LABEL . DESCRIPTION), low
to high."
  (unless (memq type efrit-decisions-question-types)
    (error "efrit-decisions: unknown question type %S" type))
  (append (list :type type :name name :instructions instructions) args))

(defun efrit-decisions--labelled (item)
  "ITEM, a string or (TEXT . DESCRIPTION), as (TEXT . DESCRIPTION)."
  (cond ((consp item) (cons (format "%s" (car item)) (or (cdr item) "")))
        (t (cons (format "%s" item) ""))))

(defun efrit-decisions-check-questions (questions)
  "Signal a readable error unless QUESTIONS are well-formed; return them."
  (unless (and questions (listp questions))
    (error "efrit-decisions: at least one question is needed"))
  (let ((names nil))
    (dolist (q questions)
      (let ((type (plist-get q :type)) (name (plist-get q :name)))
        (unless (memq type efrit-decisions-question-types)
          (error "efrit-decisions: question %S has type %S; one of %s" name type
                 (mapconcat #'symbol-name efrit-decisions-question-types ", ")))
        (unless (and (stringp name) (not (string-empty-p name)))
          (error "efrit-decisions: a question needs a name"))
        (when (member name names)
          (error "efrit-decisions: two questions named %s" name))
        (push name names)
        (unless (stringp (plist-get q :instructions))
          (error "efrit-decisions: question %s needs instructions" name))
        (when (and (eq type 'choice) (< (length (plist-get q :choices)) 2))
          (error "efrit-decisions: choice %s needs at least two choices" name))
        (when (and (eq type 'score) (< (length (plist-get q :levels)) 2))
          (error "efrit-decisions: score %s needs at least two levels" name)))))
  questions)

;;;; Protocols

(cl-defgeneric efrit-decisions-request (protocol backend input questions options)
  "The request body (an alist) asking BACKEND the QUESTIONS about INPUT.
OPTIONS is an alist from the caller, laid over the backend's own.")

(cl-defgeneric efrit-decisions-parse (protocol questions text)
  "The answers in response TEXT, one alist per question in QUESTIONS' order.
Signals an error with a readable message when the response has none.")

(defun efrit-decisions--merge-options (backend options)
  (let ((merged (copy-alist (efrit-decisions-backend-options backend))))
    (dolist (o options)
      (setf (alist-get (car o) merged nil nil #'equal) (cdr o)))
    merged))

(defun efrit-decisions--question-json (q)
  "Question plist Q in the decisions-v1 wire shape."
  (let ((type (plist-get q :type)))
    (append
     `(("type" . ,(symbol-name type))
       ("name" . ,(plist-get q :name))
       ("instructions" . ,(plist-get q :instructions)))
     (pcase type
       ('choice `(("choices" . ,(vconcat (mapcar (lambda (c)
                                                   (let ((l (efrit-decisions--labelled c)))
                                                     `(("value" . ,(car l)) ("description" . ,(cdr l)))))
                                                 (plist-get q :choices))))))
       ('score `(("levels" . ,(vconcat (mapcar (lambda (c)
                                                 (let ((l (efrit-decisions--labelled c)))
                                                   `(("label" . ,(car l)) ("description" . ,(cdr l)))))
                                               (plist-get q :levels))))))
       (_ nil)))))

(cl-defmethod efrit-decisions-request ((_protocol (eql 'decisions-v1)) backend input questions options)
  (append `(("model" . ,(efrit-decisions-backend-model backend))
            ("input" . ,input)
            ("questions" . ,(vconcat (mapcar #'efrit-decisions--question-json questions))))
          (efrit-decisions--merge-options backend options)))

(defun efrit-decisions--json-parse (text)
  (json-parse-string text :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun efrit-decisions--get (obj key)
  "Member KEY (a string) of alist OBJ with symbol keys."
  (alist-get (intern key) obj))

(defun efrit-decisions--number (value)
  "VALUE as a float, or nil."
  (cond ((numberp value) (float value))
        ((and (stringp value) (string-match-p "\\`-?[0-9.]+\\'" value)) (string-to-number value))
        (t nil)))

(defun efrit-decisions--probabilities (raw key labels)
  "The per-option probabilities of RAW under KEY as ((LABEL . P) ...), or nil.
The endpoint sends them as an object keyed by option or as a list in
LABELS' order; both are read."
  (let ((v (efrit-decisions--get raw key)))
    (cond
     ((null v) nil)
     ((and (listp v) (consp (car v)) (symbolp (caar v)))
      (mapcar (lambda (cell) (cons (symbol-name (car cell)) (efrit-decisions--number (cdr cell)))) v))
     ((listp v)
      (cl-loop for p in v for l in labels collect (cons l (efrit-decisions--number p))))
     (t nil))))

(defun efrit-decisions--normalize (q raw)
  "The answer alist for question Q from its RAW answer object."
  (let* ((type (plist-get q :type))
         (name (plist-get q :name))
         (base `((name . ,name) (type . ,type) (raw . ,raw))))
    (pcase type
      ('predicate
       (let ((p (or (efrit-decisions--number (efrit-decisions--get raw "probability"))
                    (efrit-decisions--number (efrit-decisions--get raw "value")))))
         (unless p (error "efrit-decisions: no probability in the answer to %s" name))
         (append `((probability . ,p) (value . ,(>= p 0.5))) base)))
      ('choice
       (let* ((labels (mapcar (lambda (c) (car (efrit-decisions--labelled c))) (plist-get q :choices)))
              (probs (efrit-decisions--probabilities raw "probabilities" labels))
              (choice (or (efrit-decisions--get raw "choice")
                          (efrit-decisions--get raw "value")
                          (car (car (sort (copy-sequence probs) (lambda (a b) (> (or (cdr a) 0) (or (cdr b) 0)))))))))
         (unless choice (error "efrit-decisions: no choice in the answer to %s" name))
         (append `((choice . ,(format "%s" choice))
                   (probability . ,(or (efrit-decisions--number (efrit-decisions--get raw "probability"))
                                       (alist-get (format "%s" choice) probs nil nil #'equal)))
                   (probabilities . ,probs))
                 base)))
      ('score
       (let* ((labels (mapcar (lambda (c) (car (efrit-decisions--labelled c))) (plist-get q :levels)))
              (score (or (efrit-decisions--number (efrit-decisions--get raw "score"))
                         (efrit-decisions--number (efrit-decisions--get raw "value"))))
              (probs (efrit-decisions--probabilities raw "probabilities" labels)))
         (unless score (error "efrit-decisions: no score in the answer to %s" name))
         (append `((score . ,score)
                   (level . ,(or (efrit-decisions--get raw "level")
                                 (nth (max 0 (min (1- (length labels)) (round score))) labels)))
                   (probabilities . ,probs))
                 base))))))

(cl-defmethod efrit-decisions-parse ((_protocol (eql 'decisions-v1)) questions text)
  (let* ((obj (efrit-decisions--json-parse text))
         (answers (efrit-decisions--get obj "answers")))
    (unless (and (listp answers) (= (length answers) (length questions)))
      (error "efrit-decisions: expected %d answers, got %s%s" (length questions)
             (if (listp answers) (length answers) "none")
             (if-let* ((m (efrit-decisions--get (efrit-decisions--get obj "error") "message")))
                 (format ": %s" m) "")))
    (cl-mapcar #'efrit-decisions--normalize questions answers)))

;;;; Deciding

(cl-defun efrit-decide-async (input questions callback &key backend options)
  "Ask BACKEND (default the first) the QUESTIONS about INPUT; CALLBACK gets the answers.
CALLBACK is (lambda (ANSWERS ERROR)).  OPTIONS is laid over the
backend's.  Returns the `efrit-http-request', for `efrit-http-cancel'."
  (efrit-decisions-check-questions questions)
  (let* ((backend (efrit-decisions-resolve-backend backend))
         (protocol (efrit-decisions-backend-protocol backend))
         (body (efrit-decisions-request protocol backend input questions options)))
    (efrit-http-post-json
     (efrit-decisions-backend-url backend) body
     (lambda (text err)
       (if err
           (funcall callback nil (if (equal err "cancelled") err
                                   (format "%s: %s" (efrit-decisions-backend-name backend) err)))
         (condition-case e
             (funcall callback (efrit-decisions-parse protocol questions text) nil)
           (error (funcall callback nil (error-message-string e))))))
     :auth (efrit-decisions-backend-auth backend)
     :headers (when-let* ((fn (efrit-decisions-backend-headers backend))) (funcall fn))
     :label (format "decide (%s)" (efrit-decisions-backend-name backend))
     :timeout efrit-decisions-timeout)))

(cl-defun efrit-decide (input questions &key backend options)
  "Ask the QUESTIONS about INPUT and return the answers; signal on failure.
Waits without blocking Emacs; C-g cancels.  See `efrit-decide-async'."
  (let ((answers nil) (err nil))
    (efrit-http-wait
     (efrit-decide-async input questions (lambda (a e) (setq answers a err e))
                         :backend backend :options options))
    (if err (error "%s" err) answers)))

(defun efrit-decisions-answer (answers name)
  "The answer named NAME among ANSWERS, or nil."
  (seq-find (lambda (a) (equal (alist-get 'name a) name)) answers))

(cl-defun efrit-decide-many (inputs questions callback &key backend options progress)
  "Ask the QUESTIONS about each of INPUTS; CALLBACK gets the results.
At most `efrit-decisions-parallel' requests run at once.  CALLBACK is
(lambda (RESULTS)) with one (INPUT ANSWERS ERROR) per input, in order.
PROGRESS, when given, is called with (DONE TOTAL) after each answer.
Returns a function that cancels what is left."
  (efrit-decisions-check-questions questions)
  (let* ((backend (efrit-decisions-resolve-backend backend))
         (total (length inputs))
         (results (make-vector total nil))
         (pending (cl-loop for i from 0 for in in inputs collect (cons i in)))
         (done 0) (inflight nil) (cancelled nil))
    (cl-labels
        ((finish-one (i input answers err)
           (aset results i (list input answers err))
           (cl-incf done)
           (when progress (funcall progress done total))
           (when (and (= done total) (not cancelled))
             (funcall callback (append results nil))))
         (start-next ()
           (while (and pending (not cancelled)
                       (< (length inflight) (max 1 efrit-decisions-parallel)))
             (pcase-let ((`(,i . ,input) (pop pending)))
               (let (req)
                 (setq req (efrit-decide-async
                            input questions
                            (lambda (answers err)
                              (setq inflight (delq req inflight))
                              (finish-one i input answers err)
                              (start-next))
                            :backend backend :options options))
                 (push req inflight))))))
      (if (zerop total)
          (progn (funcall callback nil) #'ignore)
        (start-next)
        (lambda ()
          (setq cancelled t pending nil)
          (dolist (r inflight) (efrit-http-cancel r))
          (length inflight))))))

(cl-defun efrit-decide-many-sync (inputs questions &rest args)
  "Like `efrit-decide-many' but wait; return the results list.
ARGS are its keyword arguments.  Emacs keeps processing; C-g cancels
what is left and returns what came back (missing ones carry the error
\"cancelled\")."
  (let* ((results nil) (done nil)
         (cancel (apply #'efrit-decide-many inputs questions
                        (lambda (r) (setq results r done t))
                        args)))
    (unwind-protect
        (while (not done)
          (accept-process-output nil 0.1))
      (unless done
        (funcall cancel)
        (setq results (mapcar (lambda (in) (list in nil "cancelled")) inputs))))
    results))

;;;; Showing answers

(defun efrit-decisions-format-answer (answer)
  "ANSWER as a short string: `yes 0.93', `deal 0.81', `positive 3.07'."
  (pcase (alist-get 'type answer)
    ('predicate (format "%s %.2f" (if (alist-get 'value answer) "yes" "no") (alist-get 'probability answer)))
    ('choice (format "%s%s" (alist-get 'choice answer)
                     (if-let* ((p (alist-get 'probability answer))) (format " %.2f" p) "")))
    ('score (format "%s %.2f" (alist-get 'level answer) (alist-get 'score answer)))
    (_ (format "%S" answer))))

(provide 'efrit-decisions)

;;; efrit-decisions.el ends here
