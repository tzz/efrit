;;; efrit-session-diff.el --- The whole change of a session, in one diff -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.20.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; The model edits in fragments and the reviewer judges fragments.
;; Neither sees the change as a whole: by the thirteenth edit of a
;; turn (2026-11-11, two files) nobody had the diff of the file in
;; view, and the agent and the reviewer re-derived the state of the
;; file from memory, call after call.
;;
;; This keeps, per session, the files a write tool touched, and
;; produces one unified diff of all of them against where they
;; started: the VC HEAD when the file is under version control, else
;; a snapshot of the text taken before the session's first write (a
;; new file diffs against nothing).  The diff goes two places:
;;
;;   - the `session-diff' context source: the editor-context block of
;;     each user turn carries it, so the model starts a request with
;;     its own change in front of it, not a memory of it;
;;   - the reviewer's message: a CHANGES THIS SESSION section, so an
;;     edit is judged against the change it continues.
;;
;; Both are budgeted (`efrit-session-diff-max-chars'); over budget,
;; whole files drop from the end, last touched first kept, and the
;; omitted ones are named.  Nothing here is sent until a write tool
;; has touched a file: a read-only session carries no diff.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-log)
(require 'efrit-vcs)

(defgroup efrit-session-diff nil
  "The session's change as one diff, for the model and the reviewer."
  :group 'efrit)

(defcustom efrit-session-diff-enabled t
  "Non-nil sends the session's diff with each request and each review."
  :type 'boolean
  :group 'efrit-session-diff)

(defcustom efrit-session-diff-max-chars 12000
  "Longest session diff sent in one place.
Files past the budget are named, not shown; the most recently touched
files are kept."
  :type 'integer
  :group 'efrit-session-diff)

(defconst efrit-session-diff-write-tools
  '("edit_file" "create_file" "write_file" "edit_buffer" "format_file" "undo_edit")
  "Tools whose target file is tracked after they run.")

(defvar efrit-session-diff--files (make-hash-table :test 'equal)
  "SESSION-ID -> list of (TRUENAME . ORIGIN), most recently touched first.
ORIGIN is `vc' when the file diffs against its VC HEAD, else the
text the file had before the session's first write (a string), or
`new' for a file that did not exist then.")

(defvar efrit-current-session-id)
(declare-function efrit-resolve-path-simple "efrit-tool-utils")

;;;; Tracking

(defun efrit-session-diff--file-of (tool input)
  "The file TOOL's INPUT writes, absolute, or nil."
  (when (hash-table-p input)
    (let ((path (or (gethash "path" input) (gethash "file" input) (gethash "file_path" input))))
      (cond
       ((and (stringp path) (not (string-empty-p path)))
        (if (fboundp 'efrit-resolve-path-simple)
            (ignore-errors (efrit-resolve-path-simple path))
          (expand-file-name path)))
       ((equal tool "edit_buffer")
        (when-let* ((name (gethash "buffer" input)) (buf (get-buffer name)))
          (buffer-file-name buf)))))))

(defun efrit-session-diff--origin (file)
  "How FILE will be diffed: `vc' under version control, `new' when absent, else its text."
  (cond
   ((and (file-exists-p file)
         (ignore-errors (efrit-vcs-root (file-name-directory file)))
         (ignore-errors (vc-backend file)))
    'vc)
   ((not (file-exists-p file)) 'new)
   (t (with-temp-buffer (insert-file-contents file) (buffer-string)))))

(defun efrit-session-diff-before-tool (tool input &optional session-id)
  "Before TOOL writes INPUT's file: remember where the file starts from.
The origin is taken once per file per session, before the first write;
later writes only move the file to the front."
  (when (and efrit-session-diff-enabled (member tool efrit-session-diff-write-tools))
    (condition-case err
        (when-let* ((file (efrit-session-diff--file-of tool input)))
          (let* ((id (or session-id (bound-and-true-p efrit-current-session-id)))
                 (key (if (file-exists-p file) (file-truename file) (expand-file-name file)))
                 (files (gethash id efrit-session-diff--files))
                 (entry (assoc key files)))
            (unless entry
              (setq entry (cons key (efrit-session-diff--origin file))))
            (puthash id (cons entry (cl-remove key files :key #'car :test #'equal))
                     efrit-session-diff--files)))
      (error (efrit-log 'warn "session diff: %s" (error-message-string err))))))

(defun efrit-session-diff-files (&optional session-id)
  "The files SESSION-ID touched, most recent first, as absolute names."
  (mapcar #'car (gethash (or session-id (bound-and-true-p efrit-current-session-id))
                         efrit-session-diff--files)))

(defun efrit-session-diff-forget (&optional session-id)
  "Drop SESSION-ID's tracked files: a new conversation starts clean."
  (remhash (or session-id (bound-and-true-p efrit-current-session-id)) efrit-session-diff--files))

;;;; The diff

(defun efrit-session-diff--current-text (file)
  "FILE's text as it is now: the visiting buffer's when there is one (unsaved
edits included, that is what edit_buffer leaves), else the file's."
  (cond
   ((find-buffer-visiting file)
    (with-current-buffer (find-buffer-visiting file)
      (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max)))))
   ((file-exists-p file)
    (with-temp-buffer (insert-file-contents file) (buffer-string)))
   (t "")))

(defun efrit-session-diff--one (file origin)
  "The unified diff of FILE from ORIGIN to now, or nil when nothing changed."
  (let* ((label (abbreviate-file-name file))
         (text
          (condition-case err
              (pcase origin
                ('vc
                 (let ((buf (find-buffer-visiting file)))
                   ;; a buffer edit not yet saved is not in the working
                   ;; tree: diff the buffer's text against HEAD by hand
                   (if (and buf (buffer-modified-p buf))
                       (efrit-vcs-diff-strings
                        (or (ignore-errors
                              (with-temp-buffer
                                (vc-call-backend (vc-backend file) 'find-revision file nil (current-buffer))
                                (buffer-string)))
                            "")
                        (efrit-session-diff--current-text file)
                        (concat "a/" label) (concat "b/" label))
                     (efrit-vcs-diff (list file) nil nil (file-name-directory file)))))
                ('new
                 (let ((now (efrit-session-diff--current-text file)))
                   (unless (string-empty-p now)
                     (efrit-vcs-diff-strings "" now "/dev/null" (concat "b/" label)))))
                ((pred stringp)
                 (efrit-vcs-diff-strings origin (efrit-session-diff--current-text file)
                                         (concat "a/" label) (concat "b/" label))))
            (error (efrit-log 'warn "session diff of %s: %s" label (error-message-string err))
                   nil))))
    (when (and (stringp text) (string-match-p "^[-+]" text))
      (string-trim-right text))))

(defun efrit-session-diff-text (&optional session-id budget)
  "SESSION-ID's change as one unified diff within BUDGET chars, or nil.
Files are diffed most recently touched first; those past the budget
are named in a trailing line.  Nil when no tracked file differs from
its origin."
  (when efrit-session-diff-enabled
    (let* ((id (or session-id (bound-and-true-p efrit-current-session-id)))
           (budget (or budget efrit-session-diff-max-chars))
           (parts nil) (omitted nil) (used 0))
      (dolist (entry (gethash id efrit-session-diff--files))
        (when-let* ((diff (efrit-session-diff--one (car entry) (cdr entry))))
          (if (or omitted (> (+ used (length diff) 1) budget))
              (push (abbreviate-file-name (car entry)) omitted)
            (push diff parts)
            (cl-incf used (1+ (length diff))))))
      (when (or parts omitted)
        (concat (mapconcat #'identity (nreverse parts) "\n")
                (when omitted
                  (format "%s[%d more changed file%s not shown for size: %s]"
                          (if parts "\n" "") (length omitted) (if (cdr omitted) "s" "")
                          (mapconcat #'identity (nreverse omitted) ", "))))))))

;;;; Context source

(defun efrit-session-diff-context-source (_buf)
  "The `session-diff' entry of `efrit-context-sources'."
  (when-let* ((diff (efrit-session-diff-text)))
    (concat "Your changes so far this session (unified diff of every file you wrote, "
            "against where it started; the state you continue from):\n"
            "```diff\n" diff "\n```")))

;;;; Showing the user

(defun efrit-session-diff-show (&optional session-id)
  "Show SESSION-ID's diff (the current session's) in a popup."
  (interactive)
  (let ((diff (efrit-session-diff-text session-id efrit-session-diff-max-chars)))
    (if (not diff)
        (message "Efrit: this session wrote no file yet")
      (require 'efrit-ui-helpers)
      (efrit-show-popup "*efrit session diff*" diff 'diff-mode))))

(declare-function efrit-show-popup "efrit-ui-helpers" (name text &optional mode))

(provide 'efrit-session-diff)

;;; efrit-session-diff.el ends here
