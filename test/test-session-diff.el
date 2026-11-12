;;; test-session-diff.el --- the session's change as one diff -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-session-diff)
(require 'efrit-context-sources)
(require 'efrit-review)

(defun test-sd--input (&rest kv)
  (let ((h (make-hash-table :test 'equal)))
    (while kv (puthash (pop kv) (pop kv) h))
    h))

(defmacro test-sd--with-tree (&rest body)
  "BODY with ROOT a fresh git repository (tracked.el committed) and
PLAIN a directory outside any repository (loose.el)."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "efrit-sd-repo-" t)))
          (plain (file-name-as-directory (make-temp-file "efrit-sd-plain-" t)))
          (efrit-session-diff--files (make-hash-table :test 'equal))
          (efrit-session-diff-enabled t)
          (efrit-current-session-id "sd-test"))
     (skip-unless (executable-find "git"))
     (unwind-protect
         (let ((default-directory root))
           (call-process "git" nil nil nil "init" "-q")
           (call-process "git" nil nil nil "config" "user.email" "t@example.invalid")
           (call-process "git" nil nil nil "config" "user.name" "t")
           (with-temp-file "tracked.el" (insert "(defun sd-a () 1)\n(defun sd-b () 2)\n"))
           (call-process "git" nil nil nil "add" ".")
           (call-process "git" nil nil nil "commit" "-q" "-m" "first")
           (with-temp-file (expand-file-name "loose.el" plain) (insert "(setq loose 1)\n"))
           ,@body)
       (dolist (f (list (expand-file-name "tracked.el" root) (expand-file-name "loose.el" plain)
                        (expand-file-name "fresh.el" root)))
         (when-let* ((b (find-buffer-visiting f)))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory root t)
       (delete-directory plain t))))

(ert-deftest test-session-diff-tracks-writes-and-diffs-from-where-each-file-started ()
  "A tracked file diffs against HEAD, a file outside VC against the text it
had before the session's first write, a new file against nothing.  Files
come most recently touched first; an untouched session has no diff; a
reset forgets."
  (test-sd--with-tree
    (let ((tracked (expand-file-name "tracked.el" root))
          (loose (expand-file-name "loose.el" plain))
          (fresh (expand-file-name "fresh.el" root)))
      (should-not (efrit-session-diff-text))
      ;; a read tool tracks nothing
      (efrit-session-diff-before-tool "read_file" (test-sd--input "path" tracked))
      (should-not (efrit-session-diff-files))
      ;; the first write records the origin; the edit then happens
      (efrit-session-diff-before-tool "edit_file" (test-sd--input "path" tracked "old_str" "1" "new_str" "11"))
      (with-temp-file tracked (insert "(defun sd-a () 11)\n(defun sd-b () 2)\n"))
      (efrit-session-diff-before-tool "edit_file" (test-sd--input "path" loose))
      (with-temp-file loose (insert "(setq loose 2)\n"))
      (efrit-session-diff-before-tool "create_file" (test-sd--input "path" fresh "content" "x"))
      (with-temp-file fresh (insert "(provide 'fresh)\n"))
      (should (equal (list (file-truename fresh) (file-truename loose) (file-truename tracked))
                     (mapcar #'file-truename (efrit-session-diff-files))))
      (let ((diff (efrit-session-diff-text)))
        (should diff)
        ;; the new file, from nothing
        (should (string-match-p "^\\+(provide 'fresh)" diff))
        (should (string-match-p "/dev/null" diff))
        ;; the loose file, against its pre-session text
        (should (string-match-p "^-(setq loose 1)" diff))
        (should (string-match-p "^\\+(setq loose 2)" diff))
        ;; the tracked file, against HEAD
        (should (string-match-p "^-(defun sd-a () 1)" diff))
        (should (string-match-p "^\\+(defun sd-a () 11)" diff))
        ;; most recent first
        (should (< (string-search "fresh" diff) (string-search "loose" diff)
                   (string-search "tracked" diff))))
      ;; a second write to the same file keeps the first origin
      (efrit-session-diff-before-tool "edit_file" (test-sd--input "path" loose))
      (with-temp-file loose (insert "(setq loose 3)\n"))
      (should (string-match-p "^-(setq loose 1)\n\\+(setq loose 3)" (efrit-session-diff-text)))
      ;; a file edited back to its origin drops out
      (with-temp-file loose (insert "(setq loose 1)\n"))
      (should-not (string-match-p "loose" (efrit-session-diff-text)))
      ;; budget: whole files past it are named, the most recent kept
      (let* ((first (efrit-session-diff--one (file-truename fresh) 'new))
             (short (efrit-session-diff-text nil (+ 20 (length first)))))
        (should (string-match-p "fresh" short))
        (should (string-match-p "\\[1 more changed file not shown for size: .*tracked.el\\]" short))
        (should-not (string-match-p "^-(defun sd-a" short)))
      ;; the unsaved buffer text counts, not the file on disk
      (with-current-buffer (find-file-noselect tracked)
        (goto-char (point-max)) (insert "(defun sd-c () 3)\n")
        (should (string-match-p "^\\+(defun sd-c () 3)" (efrit-session-diff-text)))
        (set-buffer-modified-p nil))
      (efrit-session-diff-forget)
      (should-not (efrit-session-diff-files))
      (should-not (efrit-session-diff-text)))))

(ert-deftest test-session-diff-reaches-the-model-and-the-reviewer ()
  "The `session-diff' context source fences the diff; the reviewer's
message carries it under CHANGES THIS SESSION; neither says anything
when the session wrote nothing."
  (test-sd--with-tree
    (let ((tracked (expand-file-name "tracked.el" root)))
      (with-temp-buffer
        (should-not (efrit-context--source-session-diff (current-buffer)))
        (should-not (string-match-p "CHANGES THIS SESSION"
                                    (efrit-review--user-message "req" nil "1. edit_file {}" nil nil
                                                                (efrit-review--session-diff "sd-test"))))
        (efrit-session-diff-before-tool "edit_file" (test-sd--input "path" tracked))
        (with-temp-file tracked (insert "(defun sd-a () 1)\n"))
        (let ((src (efrit-context--source-session-diff (current-buffer))))
          (should (string-match-p "\\`Your changes so far this session" src))
          (should (string-match-p "```diff\n" src))
          (should (string-match-p "^-(defun sd-b () 2)" src)))
        (let ((msg (efrit-review--user-message "req" nil "1. edit_file {}" nil nil
                                               (efrit-review--session-diff "sd-test"))))
          (should (string-match-p "CHANGES THIS SESSION" msg))
          (should (string-match-p "^-(defun sd-b () 2)" msg))
          ;; the order the reviewer reads: request, changes, then the calls
          (should (< (string-search "USER REQUEST" msg) (string-search "CHANGES THIS SESSION" msg)
                     (string-search "PROPOSED TOOL CALLS" msg))))
        ;; the source respects the switch
        (let ((efrit-session-diff-enabled nil))
          (should-not (efrit-context--source-session-diff (current-buffer))))))))

(provide 'test-session-diff)
;;; test-session-diff.el ends here
