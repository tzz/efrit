;;; test-review-flags.el --- what a Lisp change does, for the reviewer -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-review-flags)
(require 'efrit-review)

(defun test-rf--use (name &rest kv)
  (let ((input (make-hash-table :test 'equal)))
    (while kv (puthash (pop kv) (pop kv) input))
    (list "id" name input)))

(ert-deftest test-review-flags-effects-of-forms ()
  "Keys, hooks, advice, core variables, :vc, shadowing, loads and side effects are found;
plain defuns under the file's own prefix and quoted data are not."
  (let ((eff (efrit-review-flags--text-effects
              "(global-set-key (kbd \"C-c x\") #'foo)
               (add-hook 'after-save-hook #'bar)
               (advice-add 'find-file :around #'baz)
               (setq load-path (cons \"/x\" load-path))
               (use-package magit :vc (:url \"https://example.invalid/m.git\" :rev \"abc\") :init (setq m 1))
               (defmacro transient-describe-global-set-key (&rest _) nil)
               (defun tzz-fine () 1)
               (require 'dash)
               (delete-file \"/tmp/x\")
               '(global-set-key \"not real\")"
              '("tzz"))))
    (should (assq 'key eff))
    (should (string-match-p "global" (cdr (assq 'key eff))))
    (should (string-match-p "after-save-hook" (cdr (assq 'hook eff))))
    (should (string-match-p "find-file" (cdr (assq 'advice eff))))
    (should (string-match-p "load-path" (cdr (assq 'core-var eff))))
    (should (string-match-p "example.invalid" (cdr (assq 'vc eff))))
    (should (assq 'init eff))
    (should (string-match-p "transient-describe-global-set-key" (cdr (assq 'shadow eff))))
    (should (string-match-p "dash" (cdr (assq 'load eff))))
    (should (string-match-p "delete-file" (cdr (assq 'effect eff))))
    ;; one key flag only: the quoted one is data
    (should (= 1 (cl-count 'key eff :key #'car)))
    ;; tzz-fine is the file's own
    (should-not (cl-some (lambda (e) (string-match-p "tzz-fine" (cdr e))) eff))))

(ert-deftest test-review-flags-while-let-that-never-advances ()
  "A while-let whose body setq's one of its own spec variables to end
the loop is flagged as infinite (the spec rebinds it each time); a
while-let that advances through state outside its spec is not."
  (let ((bad (efrit-review-flags--text-effects
              "(let (r) (while-let ((p (overlay-properties o))) (setq r (car p)) (setq p nil)) r)" nil))
        (ok (efrit-review-flags--text-effects
             "(let ((i 0)) (while-let ((x (nth i xs))) (push x out) (setq i (1+ i))))" nil)))
    (should (assq 'loop bad))
    (should (string-match-p "infinite" (cdr (assq 'loop bad))))
    (should (string-match-p "\\bp\\b" (cdr (assq 'loop bad))))
    (should-not (assq 'loop ok))))

(ert-deftest test-review-flags-edit-reports-only-what-changes ()
  "An edit_file to an .el file is flagged for the effects the new text adds,
not for what the old text already did; a non-Lisp file is not analysed."
  (let ((flags (efrit-review-flags-for-use
                (test-rf--use "edit_file"
                              "path" "/home/u/autodist/emacs/tzz.emacs.libraries.el"
                              "old_str" "(use-package expand-region :bind (\"C-=\" . er/expand-region))"
                              "new_str" "(use-package expand-region :bind (\"C-=\" . er/expand-region))
                                         (use-package repeat :ensure nil :config (repeat-mode 1))
                                         (advice-add 'save-buffer :before #'tzz-note)"))))
    (should (cl-some (lambda (f) (string-match-p "advice-add save-buffer" f)) flags))
    (should-not (cl-some (lambda (f) (string-match-p "expand-region" f)) flags)))
  (should-not (efrit-review-flags-for-use
               (test-rf--use "edit_file" "path" "/home/u/notes.txt" "old_str" "a" "new_str" "(advice-add 'x :around #'y)")))
  ;; eval: the form itself
  (should (cl-some (lambda (f) (string-match-p "shadow" f))
                   (efrit-review-flags-for-use
                    (test-rf--use "eval_sexp" "expr" "(defmacro transient-describe-global-set-key (&rest _) nil)")))))

(ert-deftest test-review-flags-reach-the-reviewer-text ()
  "The batch text the reviewer reads carries the flags under the call, and
the system prompt explains the kinds."
  (let* ((input (make-hash-table :test 'equal))
         (use (let ((u (make-hash-table :test 'equal)))
                (puthash "path" "/x/init.el" input)
                (puthash "content" "(use-package evil :vc (:url \"https://example.invalid/evil\"))" input)
                (puthash "type" "tool_use" u) (puthash "id" "t1" u)
                (puthash "name" "create_file" u) (puthash "input" input u) u))
         (batch (efrit-review-describe-batch (vector use))))
    (should (string-match-p "\\[FLAG vc: use-package evil :vc" batch))
    (should (string-match-p "vc = a use-package" efrit-review--system-prompt))))

(ert-deftest test-review-flags-eval-shelling-out-to-what-emacs-does ()
  "An eval that runs `shell-command-to-string \"git …\"' is flagged with the
facility the sandbox will name when it refuses (2026-10-19: approved
without a flag); a real program only gets the plain effect flag."
  (require 'efrit-sandbox)
  (let* ((flags (lambda (expr)
                  (let ((input (make-hash-table :test 'equal)))
                    (puthash "expr" expr input)
                    (efrit-review-flags-for-use (list "t1" "eval_sexp" input))))))
    (let ((git (funcall flags "(shell-command-to-string \"git status --short x.el\")")))
      (should (cl-some (lambda (f) (string-match-p "\\[FLAG shell: Emacs does this natively: git -> vcs_status" f)) git)))
    (let ((tmux (funcall flags "(call-process \"tmux\" nil nil nil \"ls\")")))
      (should (cl-some (lambda (f) (string-match-p "\\[FLAG effect: call-process" f)) tmux))
      (should-not (cl-some (lambda (f) (string-match-p "Emacs does this natively" f)) tmux)))))

(ert-deftest test-review-flags-eval-reading-files-by-hand ()
  "An eval that does insert-file-contents + re-search-forward on a literal
path is flagged `tool' with read_file / search_content named; a form that
computes nothing file-like gets no such flag."
  (let* ((flags (lambda (expr)
                  (let ((input (make-hash-table :test 'equal)))
                    (puthash "expr" expr input)
                    (efrit-review-flags-for-use (list "t1" "eval_sexp" input))))))
    (let ((f (funcall flags "(with-temp-buffer (insert-file-contents (expand-file-name \"~/x/a.el\")) (goto-char (point-min)) (re-search-forward \"foo\" nil t))")))
      (should (cl-some (lambda (x) (string-match-p "\\[FLAG tool: insert-file-contents by hand: efrit has read_file" x)) f))
      (should (cl-some (lambda (x) (string-match-p "re-search-forward by hand: efrit has search_content" x)) f)))
    (should-not (cl-some (lambda (x) (string-match-p "FLAG tool" x))
                         (funcall flags "(progn (global-completion-preview-mode -1) (list major-mode))")))))

;;;; The defun after the edit

(defun test-rf--batch-use (name &rest kv)
  "A tool_use block for `efrit-review-describe-batch' with NAME and KV input."
  (let ((input (make-hash-table :test 'equal)) (use (make-hash-table :test 'equal)))
    (while kv (puthash (pop kv) (pop kv) input))
    (puthash "type" "tool_use" use) (puthash "id" "t1" use)
    (puthash "name" name use) (puthash "input" input use)
    use))

(defun test-rf--after-callee () "Fixture." 1)
(defun test-rf--after-caller () "Fixture." (test-rf--after-callee))

(ert-deftest test-review-flags-reviewer-sees-the-defun-after-an-edit-file ()
  "An edit_file on a Lisp file shows the reviewer the whole top-level form
the replacement leaves, named, with the loaded functions that call it.
An old text found twice (without replace_all) shows nothing: edit_file
will refuse it.  A non-Lisp file shows nothing."
  (let ((file (make-temp-file "efrit-rf-" nil ".el"))
        (txt (make-temp-file "efrit-rf-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; -*- lexical-binding: t; -*-\n"
                    "(defvar test-rf--after-x 1)\n\n"
                    "(defun test-rf--after-callee ()\n  \"Fixture.\"\n  (let ((a 1))\n    (+ a 1)))\n\n"
                    "(defun test-rf--after-other () 2)\n"))
          (copy-file file txt t)
          (let* ((use (test-rf--batch-use "edit_file" "path" file
                                          "old_str" "    (+ a 1)))" "new_str" "    (* a 2)))"))
                 (lines (efrit-review-after-edit (list "t1" "edit_file" (gethash "input" use))))
                 (batch (efrit-review-describe-batch (vector use))))
            (should (= 2 (length lines)))
            (should (string-match-p "\\`\\[AFTER THE EDIT, test-rf--after-callee:\n(defun test-rf--after-callee ()" (car lines)))
            ;; the new text is in, the old is out, the rest of the form is shown
            (should (string-match-p "(\\* a 2)" (car lines)))
            (should-not (string-match-p "(\\+ a 1)" (car lines)))
            (should (string-match-p "\"Fixture.\"" (car lines)))
            ;; neighbours are not
            (should-not (string-match-p "test-rf--after-other" (car lines)))
            (should (string-match-p "\\`\\[CALLED BY: .*test-rf--after-caller" (cadr lines)))
            (should (string-match-p "AFTER THE EDIT, test-rf--after-callee" batch))
            (should (string-match-p "CALLED BY" batch)))
          ;; a top-level form that is not a defun: shown, unnamed, no callers
          (let ((lines (efrit-review-after-edit
                        (list "t1" "edit_file"
                              (gethash "input" (test-rf--batch-use "edit_file" "path" file
                                                                   "old_str" "after-x 1" "new_str" "after-x 2"))))))
            (should (= 1 (length lines)))
            (should (string-match-p "\\`\\[AFTER THE EDIT, test-rf--after-x:\n(defvar test-rf--after-x 2)\\]\\'" (car lines))))
          ;; ambiguous old text: nothing (the tool refuses it)
          (should-not (efrit-review-after-edit
                       (list "t1" "edit_file"
                             (gethash "input" (test-rf--batch-use "edit_file" "path" file
                                                                  "old_str" "test-rf--after" "new_str" "x")))))
          ;; replace_all: the form around the first hit, after all replacements
          (let ((lines (efrit-review-after-edit
                        (list "t1" "edit_file"
                              (gethash "input" (test-rf--batch-use "edit_file" "path" file "replace_all" t
                                                                   "old_str" "a 1" "new_str" "a 7"))))))
            (should (string-match-p "(let ((a 7))\n    (\\+ a 7)" (car lines))))
          ;; not Lisp: nothing
          (should-not (efrit-review-after-edit
                       (list "t1" "edit_file"
                             (gethash "input" (test-rf--batch-use "edit_file" "path" txt
                                                                  "old_str" "(+ a 1)" "new_str" "(* a 2)")))))
          ;; the text comes from the visiting buffer when there is one,
          ;; unsaved changes included
          (let ((buf (find-file-noselect file)))
            (unwind-protect
                (progn
                  (with-current-buffer buf
                    (goto-char (point-max)) (insert "(defun test-rf--after-new () (list 1 2))\n"))
                  (let ((lines (efrit-review-after-edit
                                (list "t1" "edit_file"
                                      (gethash "input" (test-rf--batch-use "edit_file" "path" file
                                                                           "old_str" "(list 1 2)" "new_str" "(list 1 2 3)"))))))
                    (should (string-match-p "test-rf--after-new () (list 1 2 3))\\]" (car lines)))))
              (with-current-buffer buf (set-buffer-modified-p nil))
              (kill-buffer buf))))
      (delete-file file) (delete-file txt))))

(ert-deftest test-review-flags-reviewer-sees-the-defun-after-an-edit-buffer ()
  "An edit_buffer replacement or insertion in a Lisp buffer shows the form it
lands in; a non-Lisp buffer shows nothing."
  (with-temp-buffer
    (rename-buffer " *efrit-rf-buffer*" t)
    (insert "(defun test-rf--after-b (x)\n  x)\n")
    (delay-mode-hooks (emacs-lisp-mode))
    (let* ((name (buffer-name))
           (from (save-excursion (goto-char (point-min)) (search-forward "  x)") (- (point) 2)))
           (lines (efrit-review-after-edit
                   (list "t1" "edit_buffer"
                         (gethash "input" (test-rf--batch-use "edit_buffer" "buffer" name "replace" t
                                                              "from-pos" from "to-pos" (1+ from) "text" "(1+ x)"))))))
      (should (= 1 (length lines)))
      (should (string-match-p "\\`\\[AFTER THE EDIT, test-rf--after-b:\n(defun test-rf--after-b (x)\n  (1\\+ x))\\]\\'" (car lines)))
      ;; an insertion at a position inside the form
      (let ((lines (efrit-review-after-edit
                    (list "t1" "edit_buffer"
                          (gethash "input" (test-rf--batch-use "edit_buffer" "buffer" name
                                                               "position" from "text" "(ignore) "))))))
        (should (string-match-p "(ignore) x)" (car lines))))
      ;; appended at the end: a new form, shown on its own
      (let ((lines (efrit-review-after-edit
                    (list "t1" "edit_buffer"
                          (gethash "input" (test-rf--batch-use "edit_buffer" "buffer" name
                                                               "text" "(defun test-rf--after-c () 3)\n"))))))
        (should (string-match-p "\\`\\[AFTER THE EDIT, test-rf--after-c:\n(defun test-rf--after-c () 3)\\]\\'" (car lines))))
      (fundamental-mode)
      (should-not (efrit-review-after-edit
                   (list "t1" "edit_buffer"
                         (gethash "input" (test-rf--batch-use "edit_buffer" "buffer" name "text" "x"))))))))

(provide 'test-review-flags)
;;; test-review-flags.el ends here
