;;; test-tool-contract.el --- library_contract -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-tool-contract)

(defvar test-contract--option 1)
(put 'test-contract--option 'standard-value '(1))
(put 'test-contract--option 'custom-type 'integer)
(defvar test-contract--plain 2)
(defvar test-contract--written nil)
(defvar test-contract--done-hook nil)

(defun test-contract--fixture-file ()
  "Write the fixture library to a temp file and load it; return the file."
  (let ((file (make-temp-file "efrit-contract-" nil ".el")))
    (with-temp-file file
      (insert ";;; -*- lexical-binding: t; -*-\n"
              "(defun test-contract--helper (x) (* x 2))\n"
              "(defun test-contract--cmd (arg)\n"
              "  \"Do a thing at point in the current buffer.\\n\\nSecond paragraph.\"\n"
              "  (declare (interactive-only test-contract--helper))\n"
              "  (interactive \"sName: \")\n"
              "  (let ((local test-contract--option))\n"
              "    (setq test-contract--written (test-contract--helper (+ local test-contract--plain)))\n"
              "    (setq local nil)\n"
              "    (run-hook-with-args 'test-contract--done-hook arg)\n"
              "    (run-hooks 'test-contract--done-hook 'post-command-hook)\n"
              "    test-contract--written))\n"
              "(defun test-contract--caller () (test-contract--cmd \"x\"))\n"
              "(define-obsolete-function-alias 'test-contract--old #'test-contract--cmd \"9.9\")\n"
              "(provide 'test-contract-fixture)\n"))
    (load file nil t)
    file))

(ert-deftest test-contract-reports-the-contract-from-loaded-code ()
  "Kind, signature, interactive spec, the interactive-only declaration
with its replacement, the obsolete alias, the options and variables
read (a `!' on the ones set, locals excluded), the hooks run, the
positional dependencies the docstring admits, the first paragraph
only, and the callers from the elisp xref backend."
  (let ((file (test-contract--fixture-file)))
    (unwind-protect
        (progn
          (let* ((r (efrit-tool-library-contract '((symbol . "test-contract--cmd"))))
                 (res (alist-get 'result r)))
            (should (eq t (alist-get 'success r)))
            (should (equal "command" (alist-get 'kind res)))
            (should (equal "(arg)" (alist-get 'signature res)))
            (should (equal "\"sName: \"" (alist-get 'interactive_spec res)))
            (should (equal "test-contract--helper" (alist-get 'interactive_only (alist-get 'declarations res))))
            (should (equal "Do a thing at point in the current buffer." (alist-get 'doc res)))
            (should (member "at point" (append (alist-get 'depends_on res) nil)))
            (should (member "current buffer" (append (alist-get 'depends_on res) nil)))
            (should (equal ["test-contract--option"] (alist-get 'reads_options res)))
            (let ((vars (append (alist-get 'reads_variables res) nil)))
              (should (member "test-contract--plain" vars))
              (should (member "test-contract--written!" vars))
              ;; the let-bound local, set or read, is not a global
              (should-not (cl-some (lambda (v) (string-prefix-p "local" v)) vars))
              ;; hook variables read by the runners are hooks, not reads
              (should-not (member "post-command-hook" vars)))
            (should (equal '("test-contract--done-hook" "post-command-hook")
                           (append (alist-get 'runs_hooks res) nil)))
            (should (equal [] (alist-get 'advice res)))
            (should (string-suffix-p (file-name-nondirectory file) (file-name-nondirectory (alist-get 'file res))))
            ;; the caller is found in the loaded code, by name; the
            ;; obsolete alias is not a caller
            (let ((callers (append (alist-get 'callers res) nil)))
              (should (cl-some (lambda (c) (string-prefix-p "test-contract--caller in " c)) callers))
              (should-not (cl-some (lambda (c) (string-prefix-p "test-contract--old " c)) callers))))
          ;; the obsolete alias: kind alias, of what, since when
          (let ((res (alist-get 'result (efrit-tool-library-contract '((symbol . "test-contract--old") (include_callers . :json-false))))))
            (should (equal "alias" (alist-get 'kind res)))
            (should (equal "test-contract--cmd" (alist-get 'alias_of res)))
            (should (equal "9.9" (alist-get 'since (alist-get 'obsolete (alist-get 'declarations res))))))
          ;; a primitive says so; an unknown name says to load the library
          (let ((res (alist-get 'result (efrit-tool-library-contract '((symbol . "car") (include_callers . :json-false))))))
            (should (equal "primitive (C)" (alist-get 'source res))))
          (should (string-match-p "is its library loaded"
                                  (format "%s" (efrit-tool-library-contract '((symbol . "no-such-function-xyz"))))))
          (should (string-match-p "not a function"
                                  (format "%s" (efrit-tool-library-contract '((symbol . "test-contract--plain")))))))
      (dolist (f '(test-contract--helper test-contract--cmd test-contract--caller test-contract--old))
        (fmakunbound f))
      (setq features (delq 'test-contract-fixture features))
      (delete-file file))))

;;; test-tool-contract.el ends here
