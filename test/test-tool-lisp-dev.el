;;; test-tool-lisp-dev.el --- load_project_file and run_ert -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-test-sandbox-helpers)
(require 'efrit-tool-lisp-dev)
(require 'efrit-do-handlers)

(ert-deftest test-lisp-dev-load-then-run-ert-in-process ()
  "A project .el file loads into this Emacs (source, not .elc); its tests
run by selector with counts and failure messages; a file outside the
project is refused; an unknown selector says to load first."
  (let* ((root (file-name-as-directory (make-temp-file "efrit-ld-" t)))
         (efrit-project-root root)
         (default-directory root)
         (outside (make-temp-file "efrit-ld-out-" nil ".el" "(defun ld-out () 1)\n")))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "ld-lib.el" root)
            (insert ";;; ld-lib.el -*- lexical-binding: t; -*-\n(defun ld-lib-add (a b) (+ a b))\n(provide 'ld-lib)\n"))
          (with-temp-file (expand-file-name "test-ld-lib.el" root)
            (insert ";;; -*- lexical-binding: t; -*-\n(require 'ert)\n(require 'ld-lib)\n"
                    "(ert-deftest ld-lib-passes () (should (= 3 (ld-lib-add 1 2))))\n"
                    "(ert-deftest ld-lib-fails () (should (= 4 (ld-lib-add 1 2))))\n"))
          ;; a stale .elc must not win
          (with-temp-file (expand-file-name "ld-lib.elc" root) (insert "garbage"))
          (let ((r (efrit-tool-load-project-file '((path . "ld-lib.el")))))
            (should (eq t (alist-get 'success r)))
            (should (member "ld-lib" (append (alist-get 'features_provided (alist-get 'result r)) nil)))
            (should (fboundp 'ld-lib-add)))
          (let ((load-path (cons root load-path)))
            (efrit-tool-load-project-file '((path . "test-ld-lib.el"))))
          (let* ((r (efrit-tool-run-ert '((selector . "\\`ld-lib-"))))
                 (res (alist-get 'result r)))
            (should (eq t (alist-get 'success r)))
            (should (= 2 (alist-get 'total res)))
            (should (= 1 (alist-get 'passed res)))
            (should (= 1 (alist-get 'failed res)))
            (let ((f (aref (alist-get 'failures res) 0)))
              (should (equal "ld-lib-fails" (alist-get 'name f)))
              (should (string-match-p "ert-test-failed" (alist-get 'message f)))))
          (should-not (eq t (alist-get 'success (efrit-tool-load-project-file `((path . ,outside))))))
          (should (string-match-p "load the test file first"
                                  (format "%s" (efrit-tool-run-ert '((selector . "\\`no-such-test-"))))))
          ;; the handlers are wired
          (let ((input (make-hash-table :test 'equal)))
            (puthash "selector" "\\`ld-lib-passes\\'" input)
            (should (string-match-p "\"passed\":1" (efrit-do--handle-run-ert input)))))
      (dolist (sym '(ld-lib-add)) (fmakunbound sym))
      (dolist (test '(ld-lib-passes ld-lib-fails)) (ert-delete-test test))
      (setq features (delq 'ld-lib features))
      (ignore-errors (delete-file outside))
      (delete-directory root t))))

(ert-deftest test-lisp-dev-load-through-a-symlinked-root ()
  "A project root reached through a symlink loads its own files.
The resolved path has the symlink followed; the root is compared in
the same form.  2026-11-11: ~/autodist -> ~/ai_workspace/source/autodist
refused every load_project_file of its files, four times in a row."
  (let* ((real (file-name-as-directory (make-temp-file "efrit-ld-real-" t)))
         (link (concat (make-temp-file "efrit-ld-link-") "-l"))
         (efrit-project-root (file-name-as-directory link))
         (default-directory efrit-project-root))
    (make-symbolic-link (directory-file-name real) link)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "ld-sym.el" real)
            (insert ";;; ld-sym.el -*- lexical-binding: t; -*-\n(defun ld-sym-one () 1)\n(provide 'ld-sym)\n"))
          ;; through the link, by relative name, and by the real path
          (dolist (path (list (expand-file-name "ld-sym.el" link) "ld-sym.el" (expand-file-name "ld-sym.el" real)))
            (let ((r (efrit-tool-load-project-file `((path . ,path)))))
              (should (eq t (alist-get 'success r)))))
          (should (fboundp 'ld-sym-one)))
      (fmakunbound 'ld-sym-one)
      (setq features (delq 'ld-sym features))
      (ignore-errors (delete-file link))
      (delete-directory real t))))

(defun test-lisp-dev--boom ()
  (interactive)
  (test-lisp-dev--inner (list 1 2)))
(defun test-lisp-dev--inner (x)
  (car (cdr (cdr (cdr x)))))

(ert-deftest test-lisp-dev-reproduce-command-returns-error-and-frames ()
  "reproduce_command runs the command in the named buffer and returns the
error with the frames from the signal point; a form that succeeds
returns its value; an unknown command is refused."
  (require 'efrit-tool-lisp-dev)
  (let ((buf (generate-new-buffer "reproduce")))
    (unwind-protect
        (cl-letf (((symbol-function 'efrit-sandbox-check-buffer) #'ignore))
          (fset 'test-lisp-dev--inner (lambda (x) (signal 'wrong-type-argument (list 'listp (nth 1 x)))))
          (let* ((r (efrit-tool-reproduce-command `((command . "test-lisp-dev--boom") (buffer . ,(buffer-name buf)))))
                 (res (alist-get 'result r)))
            (should (eq t (alist-get 'success r)))
            (should (eq t (alist-get 'failed res)))
            (should (equal "wrong-type-argument" (alist-get 'condition res)))
            (should (string-match-p "listp" (alist-get 'error res)))
            (should (equal (buffer-name buf) (alist-get 'buffer res)))
            (should (cl-some (lambda (f) (string-match-p "test-lisp-dev--boom" f))
                             (append (alist-get 'backtrace res) nil))))
          (let* ((r (efrit-tool-reproduce-command '((form . "(+ 1 2)"))))
                 (res (alist-get 'result r)))
            (should (eq :json-false (alist-get 'failed res)))
            (should (equal "3" (alist-get 'value res))))
          (let ((r (efrit-tool-reproduce-command '((command . "no-such-command-xyz")))))
            (should-not (eq t (alist-get 'success r)))))
      (kill-buffer buf))))

(provide 'test-tool-lisp-dev)

;;; test-tool-lisp-dev.el ends here
