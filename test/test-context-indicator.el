;;; test-context-indicator.el --- context describe/dismiss, range mentions -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'efrit-context-sources)
(require 'efrit-agent-mentions)

(ert-deftest test-context-describe-and-dismiss ()
  "The label names file:line, adds the region size, and says dismissed;
dismissal removes the file-bound sources and ends on a region."
  (let ((efrit-context--dismissed nil)
        (efrit-context-sources '(buffer position region project))
        (file (make-temp-file "efrit-ctx-" nil ".txt" "one\ntwo\nthree\n")))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-min)) (forward-line 1)
          (let ((name (file-name-nondirectory file)))
            (should (equal (format "⧉ %s:2" name) (efrit-context-describe (current-buffer))))
            (transient-mark-mode 1)
            (set-mark (point-min)) (goto-char (point-max)) (activate-mark)
            (should (equal (format "⧉ %s:1, 3 lines" name) (efrit-context-describe (current-buffer))))
            (deactivate-mark)
            (cl-letf (((symbol-function 'efrit-context-target-buffer) (lambda (&rest _) (current-buffer))))
              (efrit-context-dismiss)
              (should (efrit-context-dismissed-p (current-buffer)))
              (should (equal (format "⧉ %s (dismissed)" name) (efrit-context-describe (current-buffer))))
              (should (equal '(project) (efrit-context-active-sources (current-buffer))))
              (let ((snap (efrit-context-snapshot (current-buffer))))
                (should-not (and snap (string-match-p "Point: line" snap))))
              ;; a region brings it back
              (set-mark (point-min)) (goto-char (point-max)) (activate-mark)
              (should-not (efrit-context-dismissed-p (current-buffer)))
              (deactivate-mark)
              (efrit-context-restore)
              (should-not (efrit-context-dismissed-p (current-buffer))))))
      (when-let* ((b (find-buffer-visiting file))) (kill-buffer b))
      (delete-file file))))

(ert-deftest test-mention-range-expands-only-those-lines ()
  "@path#L2-L3 inlines lines 2-3 labelled; @path#L2 one line; plain @path the file."
  (let* ((root (file-name-as-directory (make-temp-file "efrit-mr-" t)))
         (efrit-project-root root))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "f.txt" root) (insert "l1\nl2\nl3\nl4\n"))
          (should (equal '("f.txt" 2 3) (efrit-agent-mention-split "f.txt#L2-L3")))
          (should (equal '("f.txt" 2 2) (efrit-agent-mention-split "f.txt#L2")))
          (should (equal '("f.txt" nil nil) (efrit-agent-mention-split "f.txt")))
          (let ((out (efrit-agent-mentions-expand "see @f.txt#L2-L3 there")))
            (should (string-match-p "lines 2-3 of 4" out))
            (should (string-match-p "\nl2\nl3\n" out))
            (should-not (string-match-p "l1" out))
            (should-not (string-match-p "l4\n" out)))
          (let ((out (efrit-agent-mentions-expand "@f.txt")))
            (should (string-match-p "l1\nl2\nl3\nl4" out))))
      (delete-directory root t))))

(ert-deftest test-mention-range-command-inserts-into-the-input ()
  (require 'efrit-agent)
  (let* ((root (file-name-as-directory (make-temp-file "efrit-mr-" t)))
         (efrit-project-root root)
         (file (expand-file-name "g.el" root))
         (efrit-agent-auto-show nil)
         (agent (get-buffer-create " *mr-agent*")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "a\nb\nc\nd\n"))
          (with-current-buffer agent
            (efrit-agent-mode) (efrit-agent--init-regions) (efrit-agent--setup-regions))
          (with-current-buffer (find-file-noselect file)
            (cl-letf (((symbol-function 'efrit-agent-target-buffer) (lambda (&rest _) agent))
                      ((symbol-function 'efrit-agent-display) #'ignore))
              (efrit-agent-mention-range 3 (point-max))))
          (with-current-buffer agent
            (should (string-match-p "@g.el#L2-L4 $"
                                    (buffer-substring-no-properties efrit-agent--input-start (point-max))))))
      (when-let* ((b (find-buffer-visiting file))) (kill-buffer b))
      (kill-buffer agent)
      (delete-directory root t))))

(provide 'test-context-indicator)
;;; test-context-indicator.el ends here
