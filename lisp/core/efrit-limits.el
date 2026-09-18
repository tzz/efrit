;;; efrit-limits.el --- Per-project loop limits, raised on request -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; The iteration cap (`efrit-repl-loop-max-iterations',
;; `efrit-do-async-max-iterations') exists to stop a runaway loop.  A
;; long refactor is not a runaway loop, but it hits the same cap, and
;; until now the turn ended with "iteration limit" and the work was
;; lost mid-flight.
;;
;; Now, when a session reaches its cap, the loop asks:
;;
;;   continue this turn (N more calls, once)
;;   raise the limit for this Emacs session
;;   raise it for this project (saved)
;;   stop here
;;
;; Session and project overrides are kept here, per project root, and
;; the loop reads the *effective* limit through `efrit-limits-effective'
;; so both loops and any future cap share one mechanism.  Project
;; overrides persist as JSON in <project>/.efrit/settings.json,
;; beside the sandbox's grants and with the same reasoning: data, not
;; code, validated on load, written 0600.
;;
;; The prompt is a transient menu when one can be shown (same
;; recursive-edit pattern as efrit-sandbox-ui), an echo-area choice
;; otherwise; with no way to ask (batch, or `efrit-limits-ask' nil)
;; the answer is "stop", which is the old behaviour.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'efrit-log)
(require 'efrit-tool-utils)   ; efrit-tool--get-project-root

(defvar transient-post-exit-hook)

(defgroup efrit-limits nil
  "Loop limits and how they are raised."
  :group 'efrit
  :prefix "efrit-limits-")

(defcustom efrit-limits-ask t
  "When non-nil, hitting an iteration cap asks whether to continue.
nil restores the old behaviour: the turn ends at the cap."
  :type 'boolean
  :group 'efrit-limits)

(defcustom efrit-limits-continue-step 50
  "How many more API calls \"continue once\" allows before asking again."
  :type 'integer
  :group 'efrit-limits)

(defconst efrit-limits--file-name "settings.json")
(defconst efrit-limits--dir ".efrit")
(defconst efrit-limits--version 1)

(defconst efrit-limits-known
  '(max-iterations max-tool-calls)
  "Limit names this module manages.  A name maps to an integer.
`max-iterations' is API calls per turn (the loop engine);
`max-tool-calls' is tool calls per turn (the circuit breaker).")

;;; State
;;
;; Overrides are keyed by (root . name): root -> alist of (name . value).

(defvar efrit-limits--session (make-hash-table :test 'equal)
  "Project root -> alist of session overrides.")

(defvar efrit-limits--project (make-hash-table :test 'equal)
  "Project root -> alist of project overrides loaded from settings.json.")

(defvar efrit-limits--loaded (make-hash-table :test 'equal)
  "Project roots whose settings file has been read this session.")

(defvar efrit-limits--once (make-hash-table :test 'equal)
  "(root . name) -> one-shot raised value that applies until the next reset.
A \"continue once\" answer sets it; `efrit-limits-reset-once' clears it
at the end of the turn.")

(defun efrit-limits-project-root ()
  "The project root overrides are keyed on."
  (file-name-as-directory (expand-file-name (efrit-tool--get-project-root))))

;;; Persistence

(defun efrit-limits-file (root)
  "Path of ROOT's settings file."
  (expand-file-name efrit-limits--file-name (expand-file-name efrit-limits--dir root)))

(defun efrit-limits--valid-overrides (obj)
  "The valid limit alist in a parsed settings OBJ (a hash table), or nil."
  (when (hash-table-p obj)
    (let ((limits (gethash "limits" obj)) (out nil))
      (when (hash-table-p limits)
        (maphash (lambda (k v)
                   (let ((name (and (stringp k) (intern k))))
                     (when (and (memq name efrit-limits-known) (integerp v) (>= v 0))
                       (push (cons name v) out))))
                 limits))
      out)))

(defun efrit-limits-load (root)
  "Read ROOT's project overrides from disk; never signals."
  (let ((file (efrit-limits-file root)))
    (puthash root
             (and (file-readable-p file)
                  (condition-case err
                      (efrit-limits--valid-overrides
                       (with-temp-buffer
                         (insert-file-contents file)
                         (json-parse-buffer :object-type 'hash-table)))
                    (error
                     (efrit-log 'warn "limits: %s unreadable (%s), ignoring"
                                file (error-message-string err))
                     nil)))
             efrit-limits--project)
    (puthash root t efrit-limits--loaded)))

(defun efrit-limits--ensure-loaded (root)
  (unless (gethash root efrit-limits--loaded)
    (efrit-limits-load root)))

(defun efrit-limits-save (root)
  "Write ROOT's project overrides (mode 0600).  efrit's own state, outside the sandbox."
  (let* ((file (efrit-limits-file root))
         (limits (gethash root efrit-limits--project))
         (json (json-encode
                `((version . ,efrit-limits--version)
                  (limits . ,(or (mapcar (lambda (c) (cons (symbol-name (car c)) (cdr c))) limits)
                                 (make-hash-table)))))))
    (make-directory (file-name-directory file) t)
    (with-file-modes #o600
      (with-temp-file file (insert json "\n")))
    (efrit-log 'info "limits: saved %s" file)
    file))

;;; Lookup and set

(defun efrit-limits-effective (name default &optional root)
  "The limit NAME in force for ROOT: once > session > project > DEFAULT.
DEFAULT is the customization value the caller would otherwise use."
  (let ((root (or root (efrit-limits-project-root))))
    (efrit-limits--ensure-loaded root)
    (or (gethash (cons root name) efrit-limits--once)
        (alist-get name (gethash root efrit-limits--session))
        (alist-get name (gethash root efrit-limits--project))
        default)))

(defun efrit-limits-set (name value scope &optional root)
  "Set limit NAME to VALUE at SCOPE (`once', `session', `project') for ROOT."
  (let ((root (or root (efrit-limits-project-root))))
    (pcase scope
      ('once (puthash (cons root name) value efrit-limits--once))
      ('session (setf (alist-get name (gethash root efrit-limits--session)) value))
      ('project
       (efrit-limits--ensure-loaded root)
       (setf (alist-get name (gethash root efrit-limits--project)) value)
       (efrit-limits-save root))
      (_ (error "Unknown limit scope %S" scope)))
    (efrit-log 'info "limits: %s = %s (%s) for %s" name value scope root)
    value))

(defun efrit-limits-reset-once (&optional root)
  "Drop one-shot raises for ROOT (default all).  Call at the end of a turn."
  (if root
      (maphash (lambda (k _) (when (equal (car k) root) (remhash k efrit-limits--once)))
               efrit-limits--once)
    (clrhash efrit-limits--once)))

(defun efrit-limits-reset-session ()
  "Forget session overrides and one-shot raises for every project."
  (interactive)
  (clrhash efrit-limits--session)
  (clrhash efrit-limits--once))

;;; Asking

(defvar efrit-limits--answer 'pending)
(defvar efrit-limits--depth nil)
(defvar efrit-limits--context nil
  "Plist (:name :current :step :root) for the open prompt.")

(defun efrit-limits--choose (answer)
  (setq efrit-limits--answer answer))

(defun efrit-limits--exit-recursive-edit ()
  (when (and efrit-limits--depth (= (recursion-depth) efrit-limits--depth))
    (exit-recursive-edit)))

(defun efrit-limits--unit (name)
  "What limit NAME counts, for the prompt."
  (pcase name
    ('max-iterations "API calls")
    ('max-tool-calls "tool calls")
    (_ (symbol-name name))))

(defun efrit-limits--menu-description ()
  (let ((c efrit-limits--context))
    (format "This turn reached %d %s, the limit for %s."
            (plist-get c :current)
            (efrit-limits--unit (plist-get c :name))
            (abbreviate-file-name (directory-file-name (plist-get c :root))))))

(defun efrit-limits--label-continue ()
  (format "continue: %d more %s, then ask again"
          (plist-get efrit-limits--context :step)
          (efrit-limits--unit (plist-get efrit-limits--context :name))))
(defun efrit-limits--label-session ()
  (format "raise the limit to %d for this Emacs session"
          (efrit-limits--raised)))
(defun efrit-limits--label-project ()
  (format "raise it to %d for this project (saved)"
          (efrit-limits--raised)))

(defun efrit-limits--raised ()
  "The new limit a session/project raise sets: current plus the step, rounded up."
  (let* ((c efrit-limits--context)
         (n (+ (plist-get c :current) (plist-get c :step))))
    (* 50 (ceiling n 50.0))))

(defun efrit-limits--define-menu ()
  (when (require 'transient nil t)
    (unless (fboundp 'efrit-limits-menu)
      (eval
       '(transient-define-prefix efrit-limits-menu ()
          "Continue past the iteration limit?"
          [:description efrit-limits--menu-description
           ["Continue"
            ("c" (lambda () (interactive) (efrit-limits--choose 'once))
             :description efrit-limits--label-continue)
            ("s" (lambda () (interactive) (efrit-limits--choose 'session))
             :description efrit-limits--label-session)
            ("p" (lambda () (interactive) (efrit-limits--choose 'project))
             :description efrit-limits--label-project)]
           ["Stop"
            ("n" "stop here; the conversation stays open"
             (lambda () (interactive) (efrit-limits--choose nil)))]])
       t))
    (fboundp 'efrit-limits-menu)))

(defun efrit-limits--ask-with-menu ()
  (setq efrit-limits--answer 'pending)
  (let ((efrit-limits--depth (1+ (recursion-depth))))
    (unwind-protect
        (progn
          (add-hook 'transient-post-exit-hook #'efrit-limits--exit-recursive-edit)
          (run-at-time 0 nil (lambda () (call-interactively #'efrit-limits-menu)))
          (condition-case nil (recursive-edit) (quit nil)))
      (remove-hook 'transient-post-exit-hook #'efrit-limits--exit-recursive-edit)))
  (if (eq efrit-limits--answer 'pending) nil efrit-limits--answer))

(defun efrit-limits--ask-in-echo-area ()
  (pcase (read-char-choice
          (format "%s  [c]ontinue %d more  [s]ession  [p]roject  [n]o "
                  (efrit-limits--menu-description)
                  (plist-get efrit-limits--context :step))
          '(?c ?s ?p ?n))
    (?c 'once) (?s 'session) (?p 'project) (_ nil)))

(defun efrit-limits-ask-to-raise (name current &optional root)
  "Ask whether to go past limit NAME, currently CURRENT, for ROOT.
Applies the chosen raise and returns the new effective limit, or nil
when the user stops (or nothing can ask).  Never signals."
  (let* ((root (or root (efrit-limits-project-root)))
         (efrit-limits--context (list :name name :current current
                                      :step efrit-limits-continue-step :root root))
         (answer (and efrit-limits-ask
                      (not noninteractive)
                      (condition-case err
                          (if (efrit-limits--define-menu)
                              (efrit-limits--ask-with-menu)
                            (efrit-limits--ask-in-echo-area))
                        (quit nil)
                        (error (efrit-log 'warn "limits prompt: %s" (error-message-string err)) nil)))))
    (pcase answer
      ('once (efrit-limits-set name (+ current efrit-limits-continue-step) 'once root))
      ((or 'session 'project) (efrit-limits-set name (efrit-limits--raised) answer root))
      (_ nil))))

(provide 'efrit-limits)

;;; efrit-limits.el ends here
