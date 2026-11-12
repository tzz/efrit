;;; efrit-tool-lisp-dev.el --- Load project Lisp and run ERT in this Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.12.33
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; After editing a Lisp file the model wants to load it and run its
;; tests.  `load' is refused inside eval_sexp (it is the way to run
;; arbitrary code past the static inspection), and a batch Emacs is
;; refused too, so the model had no route left (2026-10-20: three
;; rejected `emacs --batch' attempts for three new ERT tests).  Two
;; tools give it the in-process route, which is also the one tzz
;; wants: "everything it needs is in the running Emacs".
;;
;; `load_project_file' loads one Lisp file that lives under the
;; project root into this Emacs: `load' with the file name handler
;; and the sandbox's read check, the compiled file ignored so the
;; edit is what runs.  Reviewed as a write (it changes the live
;; session) and flagged by efrit-review-flags like an eval of the
;; file's contents.
;;
;; `run_ert' runs ERT tests by selector in this Emacs and returns the
;; counts and the failing tests' messages, nothing more.  It never
;; prompts; `ert-run-tests-batch' writes to a temporary buffer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ert)
(require 'efrit-tool-utils)
(require 'efrit-sandbox)
(require 'efrit-tool-last-error)
(require 'efrit-sandbox-eval)
(declare-function efrit-tools--call-with-input-blocked "efrit-tools" (thunk))

(declare-function efrit-review-flags--text-effects "efrit-review-flags")

(defgroup efrit-tool-lisp-dev nil
  "Loading project Lisp and running ERT from the agent."
  :group 'efrit-tool-utils)

(defcustom efrit-tool-run-ert-max-failures 10
  "Most failing tests whose messages `run_ert' returns."
  :type 'integer
  :group 'efrit-tool-lisp-dev)

(defcustom efrit-tool-run-ert-timeout 120
  "Seconds `run_ert' lets a selector run before it is stopped."
  :type 'integer
  :group 'efrit-tool-lisp-dev)

;;;; load_project_file

(defun efrit-tool-load-project-file (args)
  "Load the Lisp file ARGS's `path' names, under the project root, into this Emacs.
The source is loaded, never a stale .elc.  Returns the features the
load provided and the top-level definitions the file holds."
  (efrit-tool-execute load_project_file args
    (let* ((path-input (or (alist-get 'path args) ""))
           (info (efrit-resolve-path path-input 'read "load_project_file"))
           (path (plist-get info :path))
           ;; the resolved :path has symlinks followed; the root must
           ;; too, or a project reached through a symlink (~/autodist
           ;; -> ~/ai_workspace/source/autodist) refuses its own files
           ;; (2026-11-11: four refusals of one load)
           (root (efrit-sandbox-canonical (efrit-tool--get-project-root))))
      (when (string-empty-p path-input) (signal 'user-error (list "path is required")))
      (unless (file-exists-p path) (signal 'file-error (list "File not found" path)))
      (unless (string-match-p "\\.el\\'" path)
        (signal 'user-error (list "Only Emacs Lisp source files (.el) can be loaded" path)))
      (unless (string-prefix-p root (efrit-sandbox-canonical path))
        (signal 'user-error (list (format "%s is outside the project (%s); load_project_file loads project files only"
                                          (plist-get info :path-relative) (abbreviate-file-name root)))))
      (let ((before (copy-sequence features))
            (load-prefer-newer t)
            (messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest a) (when fmt (push (apply #'format fmt a) messages)) nil)))
          (load path nil t t))
        (efrit-tool-success
         `((path . ,path)
           (path_relative . ,(plist-get info :path-relative))
           (features_provided . ,(vconcat (mapcar #'symbol-name (cl-set-difference features before))))
           (messages . ,(vconcat (nreverse messages)))))))))

;;;; run_ert

(defun efrit-tool-run-ert--selector (raw)
  "The ERT selector for RAW: a string is a regexp, \"t\" is everything, a vector is a list of names."
  (cond
   ((null raw) t)
   ((equal raw "t") t)
   ((vectorp raw) (cons 'member (mapcar #'intern (append raw nil))))
   ((stringp raw) raw)
   (t (signal 'user-error (list "selector must be a regexp string or a list of test names")))))

(defun efrit-tool-run-ert (args)
  "Run ERT tests matching ARGS's `selector' in this Emacs; return counts and failures.
`selector' is a regexp on test names (default: every test), or a list
of test names.  The tests run under `ert-run-tests-batch' with its
output captured; failures come back with their messages, capped at
`efrit-tool-run-ert-max-failures'."
  (efrit-tool-execute run_ert args
    (let* ((selector (efrit-tool-run-ert--selector (alist-get 'selector args)))
           (matching (ert-select-tests selector t))
           (start (float-time))
           (failures nil)
           (stats nil))
      (when (null matching)
        (signal 'user-error (list (format "No ERT tests match %S; load the test file first (load_project_file)"
                                          (alist-get 'selector args)))))
      (with-temp-buffer
        (let ((standard-output (current-buffer))
              (inhibit-message t))
          (with-timeout (efrit-tool-run-ert-timeout
                         (signal 'user-error (list (format "run_ert stopped after %ds" efrit-tool-run-ert-timeout))))
            (setq stats (ert-run-tests-batch selector)))))
      (dotimes (i (length (ert--stats-tests stats)))
        (let* ((test (aref (ert--stats-tests stats) i))
               (result (aref (ert--stats-test-results stats) i)))
          (when (and result (not (ert-test-result-expected-p test result)))
            (push `((name . ,(symbol-name (ert-test-name test)))
                    (status . ,(if (ert-test-result-type-p result :failed) "failed"
                                 (if (ert-test-result-type-p result :passed) "unexpected-pass" "error")))
                    (message . ,(if (ert-test-result-with-condition-p result)
                                    (truncate-string-to-width
                                     (format "%S" (ert-test-result-with-condition-condition result))
                                     2000 nil nil "…")
                                  "")))
                  failures))))
      (setq failures (nreverse failures))
      (efrit-tool-success
       `((selector . ,(format "%S" selector))
         (total . ,(ert-stats-total stats))
         (passed . ,(ert-stats-completed-expected stats))
         (failed . ,(ert-stats-completed-unexpected stats))
         (skipped . ,(ert-stats-skipped stats))
         (seconds . ,(/ (round (* 100 (- (float-time) start))) 100.0))
         (failures . ,(vconcat (seq-take failures efrit-tool-run-ert-max-failures))))
       (and (> (length failures) efrit-tool-run-ert-max-failures)
            (list (format "%d more failure(s) not shown" (- (length failures) efrit-tool-run-ert-max-failures))))))))

;;;; reproduce_command

(declare-function efrit-sandbox-eval-inspect "efrit-sandbox-eval" (form))
(declare-function efrit-sandbox-check-buffer "efrit-sandbox" (buffer &optional tool detail))
(declare-function efrit-context-target-buffer "efrit-context-sources" ())
(defvar efrit-sandbox-enabled)
(defvar efrit-tools-input-allowed)

(defcustom efrit-tool-reproduce-max-frames 40
  "Backtrace frames returned by reproduce_command."
  :type 'integer
  :group 'efrit)

(defun efrit-tool-reproduce--backtrace (skip)
  "The current backtrace as strings, innermost first, SKIP frames dropped.
Frames of the capture machinery itself are dropped too."
  (let ((out nil) (n 0) (seen 0))
    (mapbacktrace
     (lambda (evald func args _flags)
       (cl-incf seen)
       (when (and (> seen skip) (< n efrit-tool-reproduce-max-frames)
                  (not (memq func '(efrit-tool-reproduce--backtrace mapbacktrace signal))))
         (cl-incf n)
         (push (if (and evald args)
                   (truncate-string-to-width (format "%S" (cons func args)) 200 nil nil "…")
                 (format "%S" (if (symbolp func) func (type-of func))))
               out))))
    (nreverse out)))

(defun efrit-tool-reproduce-command (args)
  "Run ARGS's `command' (or `form') in `buffer' and return its error with a backtrace.
What `toggle-debug-on-error' plus pressing the key would show, without
the debugger: the backtrace is captured at the signal point with
`handler-bind', so the frames are the real ones, not a summary.
`command' is called with `call-interactively' in the user's buffer
\(the one `buffer' names, or the target buffer), so its `interactive'
spec runs.  Returns the error, its condition and the frames, or the
value when nothing failed.  Commands that read from the minibuffer
cannot be driven here; use `form' with the arguments spelled out."
  (efrit-tool-execute reproduce_command args
    (let* ((command (alist-get 'command args))
           (form-text (alist-get 'form args))
           (buf (let ((name (alist-get 'buffer args)))
                  (cond
                   ((and name (not (string-empty-p name)))
                    (or (get-buffer name) (signal 'user-error (list (format "No buffer named %s" name)))))
                   ((and (require 'efrit-context-sources nil t) (fboundp 'efrit-context-target-buffer)
                         (efrit-context-target-buffer)))
                   (t (current-buffer)))))
           (thunk nil) (label nil))
      (cond
       ((and command (not (string-empty-p command)))
        (let ((sym (intern-soft command)))
          (unless (and sym (commandp sym))
            (signal 'user-error (list (format "%s is not an interactive command" command))))
          (setq label command
                thunk (lambda () (let ((this-command sym) (last-command this-command))
                                   (call-interactively sym))))))
       ((and form-text (not (string-empty-p form-text)))
        (let ((form (car (read-from-string form-text))))
          (when (bound-and-true-p efrit-sandbox-enabled)
            (when-let* ((why (and (fboundp 'efrit-sandbox-eval-inspect) (efrit-sandbox-eval-inspect form))))
              (signal 'user-error (list why))))
          (setq label form-text
                thunk (lambda () (eval form t)))))
       (t (signal 'user-error (list "command or form is required"))))
      (efrit-sandbox-check-buffer buf "reproduce_command")
      (let ((frames nil) (outcome nil))
        (with-current-buffer buf
          ;; The backtrace is taken at the signal, through the debugger
          ;; hook: that is what `toggle-debug-on-error' arms, and it
          ;; exists on every Emacs (`handler-bind' is 30+).  The
          ;; `debug' condition below asks Emacs to call the debugger
          ;; before unwinding into this handler; the hook records the
          ;; frames and returns, then the handler runs as usual.
          (let ((debugger (lambda (&rest _)
                            (unless frames
                              (setq frames (efrit-tool-reproduce--backtrace 4)))
                            nil))
                (debug-on-error t)
                (debug-ignored-errors nil)
                (inhibit-quit nil)
                (efrit-tools-input-allowed nil))
            (condition-case err
                (setq outcome (list 'value (funcall thunk)))
              (quit (setq outcome (list 'quit nil)))
              ((debug error) (setq outcome (list 'error err))))))
        (pcase (car outcome)
          ('error
           (let ((err (cadr outcome)))
             (efrit-tool-success
              `((ran . ,label) (buffer . ,(buffer-name buf)) (failed . t)
                (error . ,(error-message-string err))
                (condition . ,(format "%S" (car err)))
                (data . ,(truncate-string-to-width (format "%S" (cdr err)) 600 nil nil "…"))
                (backtrace . ,(vconcat frames))))))
          ('quit
           (efrit-tool-success `((ran . ,label) (buffer . ,(buffer-name buf)) (failed . :json-false)
                                 (note . "the command quit (C-g or a minibuffer read it could not do here)"))))
          (_
           (efrit-tool-success
            `((ran . ,label) (buffer . ,(buffer-name buf)) (failed . :json-false)
              (value . ,(truncate-string-to-width (format "%S" (cadr outcome)) 600 nil nil "…"))
              (note . "no error this time; if the user still sees one, the trigger differs (buffer, point, region, a prefix arg) — ask which")))))))))

;;;; probe_command

(defcustom efrit-tool-probe-max-text 4000
  "Longest buffer text `probe_command' returns after the run."
  :type 'integer
  :group 'efrit-tool-utils)

(defun efrit-tool-probe--parse-stubs (stubs)
  "STUBS from the tool input as a list of (SYMBOL . LAMBDA-FORM).
Each entry is an alist or hash with `symbol' and `lambda' (source
text of a lambda).  A stub on one of efrit's own symbols is refused
by `efrit-eval-with-function' when the probe runs."
  (mapcar (lambda (stub)
            (let* ((get (lambda (k) (cond ((hash-table-p stub) (gethash k stub))
                                          ((listp stub) (or (alist-get (intern k) stub)
                                                            (alist-get k stub nil nil #'equal))))))
                   (name (funcall get "symbol"))
                   (text (funcall get "lambda")))
              (unless (and (stringp name) (not (string-empty-p name)))
                (signal 'user-error (list "each stub needs a symbol")))
              (unless (and (stringp text) (not (string-empty-p text)))
                (signal 'user-error (list (format "stub for %s needs a lambda" name))))
              (let ((form (condition-case err (car (read-from-string text))
                            (error (signal 'user-error (list (format "stub for %s: %s" name (error-message-string err))))))))
                (unless (and (consp form) (memq (car form) '(lambda function closure)))
                  (signal 'user-error (list (format "stub for %s must be a lambda, got %s" name (car-safe form)))))
                (cons (intern name) form))))
          (if (vectorp stubs) (append stubs nil) stubs)))

(defun efrit-tool-probe--form (command args stubs calls-var)
  "The form the probe evaluates: COMMAND run with ARGS under STUBS.
Every stub is wrapped so its calls are recorded in CALLS-VAR (a
symbol bound by the caller) before the stub's own body runs; the
wrapping goes through `efrit-eval-with-function', the same path a
`cl-letf' in eval_sexp takes, so the sandbox's refusal of efrit's own
symbols applies and the definitions come back however the run ends."
  (let ((call (if args
                  `(apply #',command (list ,@args))
                `(let ((this-command ',command) (last-command this-command))
                   (call-interactively #',command)))))
    (dolist (stub (reverse stubs))
      (let ((sym (car stub)) (fn (cdr stub)))
        (setq call
              `(efrit-eval-with-function
                ',sym
                (lambda (&rest probe-args)
                  (push (cons ',sym probe-args) ,calls-var)
                  (apply ,fn probe-args))
                (lambda () ,call)))))
    call))

(defun efrit-tool-probe-command (args)
  "Run a command in a throwaway buffer, with collaborators stubbed, and report what it did.
ARGS: command (required, an interactive command or any function);
text (the buffer's initial contents); point and mark (positions in
that text, 1-based; a mark activates the region); mode (a major mode
to turn on, default `fundamental-mode'); args (a list of Lisp source
strings: with it the function is applied to those values instead of
run through `call-interactively'); stubs (a list of {symbol, lambda}:
each symbol's function cell is replaced for the run, every call to it
is recorded).  Returns the value or error, the calls each stub
received, the messages written, the buffer's text, point and mark
after, and the names of other buffers whose text changed.  The
buffer is killed afterwards.  Reviewed as eval_sexp is; synchronous
user input is blocked, so a command that prompts fails rather than
hangs; use `args' to pass what the prompt would have read."
  (efrit-tool-execute probe_command args
    (let* ((name (or (alist-get 'command args) ""))
           (command (intern-soft name))
           (text (or (alist-get 'text args) ""))
           (point-at (alist-get 'point args))
           (mark-at (alist-get 'mark args))
           (mode (let ((m (alist-get 'mode args))) (and m (not (string-empty-p m)) (intern-soft m))))
           (call-args (let ((a (alist-get 'args args)))
                        (mapcar (lambda (src) (car (read-from-string src)))
                                (if (vectorp a) (append a nil) a))))
           (stubs (efrit-tool-probe--parse-stubs (alist-get 'stubs args)))
           (calls-var (make-symbol "probe-calls"))
           (buf nil))
      (when (string-empty-p name) (signal 'user-error (list "command is required")))
      (unless (and command (fboundp command))
        (signal 'user-error (list (format "%s is not a function in this Emacs" name))))
      (when (and (null call-args) (not (commandp command)))
        (signal 'user-error (list (format "%s is not interactive; pass args to call it" name))))
      (when (and (alist-get 'mode args) (not (and mode (fboundp mode))))
        (signal 'user-error (list (format "no such mode: %s" (alist-get 'mode args)))))
      ;; the buffer is made after the argument checks, so a refused call
      ;; leaves nothing behind
      (setq buf (generate-new-buffer " *efrit-probe*"))
      (unwind-protect
          (let ((form `(let ((,calls-var nil))
                         (cons (condition-case err
                                   (list 'value ,(efrit-tool-probe--form command call-args stubs calls-var))
                                 (quit (list 'quit nil))
                                 (error (list 'error err)))
                               ,calls-var)))
                (outcome nil) (observed nil))
            (with-current-buffer buf
              (when mode (delay-mode-hooks (funcall mode)))
              (insert text)
              (goto-char (if (integerp point-at) (min (max 1 point-at) (point-max)) (point-min)))
              (when (integerp mark-at)
                (set-mark (min (max 1 mark-at) (point-max)))
                (activate-mark))
              (setq observed
                    (efrit-eval-observe
                     (lambda ()
                       (setq outcome
                             (if (bound-and-true-p efrit-sandbox-enabled)
                                 (efrit-sandbox-eval-form
                                  form
                                  (lambda (f) (efrit-tools--call-with-input-blocked (lambda () (eval f t)))))
                               (efrit-tools--call-with-input-blocked (lambda () (eval form t))))))))
              (let* ((result (car outcome))
                     (calls (reverse (cdr outcome)))
                     (after (buffer-substring-no-properties (point-min) (point-max))))
                (efrit-tool-success
                 `((command . ,name)
                   ,@(pcase (car result)
                       ('value `((failed . :json-false)
                                 (value . ,(truncate-string-to-width (format "%S" (cadr result)) 600 nil nil "…"))))
                       ('quit `((failed . t) (error . "quit: the command read input it could not get here; pass args")))
                       ('error `((failed . t)
                                 (error . ,(error-message-string (cadr result)))
                                 (condition . ,(format "%S" (car (cadr result)))))))
                   (stub_calls . ,(vconcat (mapcar (lambda (c)
                                                     `((symbol . ,(symbol-name (car c)))
                                                       (args . ,(vconcat (mapcar (lambda (a) (truncate-string-to-width (format "%S" a) 300 nil nil "…"))
                                                                                 (cdr c))))))
                                                   calls)))
                   (messages . ,(vconcat (nth 1 observed)))
                   (buffer_after . ,(if (> (length after) efrit-tool-probe-max-text)
                                        (concat (substring after 0 efrit-tool-probe-max-text)
                                                (format "… [%d more chars]" (- (length after) efrit-tool-probe-max-text)))
                                      after))
                   (point_after . ,(point))
                   (mark_after . ,(or (mark t) :json-null))
                   (other_buffers_changed . ,(vconcat (cl-remove (buffer-name buf) (nth 2 observed) :test #'equal))))))))
        (when (and buf (buffer-live-p buf)) (kill-buffer buf))))))

(provide 'efrit-tool-lisp-dev)

;;; efrit-tool-lisp-dev.el ends here
