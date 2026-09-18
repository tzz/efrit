;;; efrit-reload.el --- Reload every loaded efrit library from source -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; `M-x efrit-reload' re-`load's every efrit feature that is currently
;; loaded, in the order they were first loaded (dependencies before
;; dependents), so a checkout edited in place takes effect without
;; restarting Emacs.
;;
;; What it keeps: your customizations.  `defvar'/`defcustom' do not
;; reassign a bound variable, so settings and buffer-local state
;; survive; `defconst' and `defface' do get their new values.
;; Timers, hooks and advice added with `add-hook'/`advice-add' are
;; idempotent and stay in place.
;;
;; What it cannot do: a struct whose slots changed keeps existing
;; instances in the old shape until they are recreated (an agent
;; buffer's session, for one), and an `eval-after-load' body runs
;; again.  When something looks stale after a reload, kill the agent
;; buffer and start it fresh; when things look wrong, restart Emacs.
;;
;; The source file is preferred over a stale .elc: `load' with the
;; extension elided picks the newer of the two, as `load-prefer-newer'
;; would, so an edited .el wins over the .elc compiled before it.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defconst efrit-reload--feature-prefix "efrit"
  "Features whose names start with this are reloaded.")

(defconst efrit-reload--never
  '(efrit-reload)
  "Features left alone: reloading this file while it runs is pointless.")

(defun efrit-reload-features ()
  "The loaded efrit features, oldest first (the order to reload them in).
`features' lists the newest first."
  (reverse
   (seq-filter (lambda (f)
                 (and (string-prefix-p efrit-reload--feature-prefix (symbol-name f))
                      (not (memq f efrit-reload--never))))
               features)))

(defun efrit-reload--library-file (feature)
  "The file FEATURE was loaded from, sans extension, or nil."
  (let ((file (or (locate-library (symbol-name feature))
                  (symbol-file feature 'provide))))
    (and file (file-name-sans-extension file))))

;;;###autoload
(defun efrit-reload (&optional verbose)
  "Reload every loaded efrit library from its source, dependencies first.
With VERBOSE (a prefix argument) list each file as it loads.  Files
that fail to load are reported at the end; the rest still load."
  (interactive "P")
  (let ((load-prefer-newer t)
        (loaded 0)
        (failed nil)
        (start (float-time)))
    (dolist (feature (efrit-reload-features))
      (let ((file (efrit-reload--library-file feature)))
        (if (null file)
            (push (cons feature "no file found") failed)
          (condition-case err
              (progn
                (load file nil (not verbose))
                (cl-incf loaded))
            (error
             (push (cons feature (error-message-string err)) failed))))))
    (let ((summary (format "efrit: reloaded %d librar%s in %.1fs%s"
                           loaded (if (= loaded 1) "y" "ies")
                           (- (float-time) start)
                           (if failed
                               (format "; %d failed: %s" (length failed)
                                       (mapconcat (lambda (f) (format "%s (%s)" (car f) (cdr f)))
                                                  (nreverse failed) ", "))
                             ""))))
      (when (fboundp 'efrit-log) (funcall 'efrit-log 'info "%s" summary))
      (message "%s" summary)
      loaded)))

(provide 'efrit-reload)

;;; efrit-reload.el ends here
