;;; test-sandbox.el --- scope sandbox: core, store, eval enforcement -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-sandbox)
(require 'efrit-sandbox-store)
(require 'efrit-sandbox-eval)

(defmacro test-sb--in-project (&rest body)
  "Run BODY with a fresh temp project as the sandbox root and clean tables."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "efrit-sb-" t)))
          (efrit-project-root root)
          (efrit-sandbox-enabled t)
          (efrit-sandbox-default-project-grants '(read))
          (efrit-sandbox-request-function nil)
          (efrit-sandbox--session-grants (make-hash-table :test 'equal))
          (efrit-sandbox--project-grants (make-hash-table :test 'equal))
          (efrit-sandbox--once-grant nil)
          (efrit-sandbox-store--loaded (make-hash-table :test 'equal)))
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(defvar efrit-project-root)

;;; Core

(ert-deftest test-sb-project-read-is-default-write-is-not ()
  (test-sb--in-project
    (should (efrit-sandbox-allowed-p 'read (expand-file-name "a.el" root)))
    (should (efrit-sandbox-allowed-p 'read (expand-file-name "sub/b.el" root)))
    (should-not (efrit-sandbox-allowed-p 'write (expand-file-name "a.el" root)))
    (should-not (efrit-sandbox-allowed-p 'read "/etc/hostname"))
    (should-not (efrit-sandbox-allowed-p 'elisp t))
    (should-not (efrit-sandbox-allowed-p 'shell t))))

(ert-deftest test-sb-check-denies-without-prompt-function ()
  (test-sb--in-project
    (should (efrit-sandbox-check 'read (expand-file-name "x" root)))
    (condition-case err
        (progn (efrit-sandbox-check 'write (expand-file-name "x" root) "edit_file") (should nil))
      (efrit-sandbox-denied
       (let ((req (cadr err)))
         (should (eq (efrit-sandbox-request-cap req) 'write))
         ;; suggested target for an in-project write is the root
         (should (equal (efrit-sandbox-request-target req) root))
         (should (equal (efrit-sandbox-request-tool req) "edit_file")))))))

(ert-deftest test-sb-prompt-scopes ()
  (test-sb--in-project
    (defvar test-sb--answers nil)
    (let ((efrit-sandbox-request-function (lambda (_req) (pop test-sb--answers))))
      ;; once: allowed exactly once
      (setq test-sb--answers '(once))
      (should (efrit-sandbox-check 'write (expand-file-name "a" root)))
      (should-error (efrit-sandbox-check 'write (expand-file-name "a" root)) :type 'efrit-sandbox-denied)
      ;; session: sticks, not persisted
      (setq test-sb--answers '(session))
      (should (efrit-sandbox-check 'write (expand-file-name "a" root)))
      (should (efrit-sandbox-allowed-p 'write (expand-file-name "other" root)))
      (should-not (file-exists-p (efrit-sandbox-store-file root)))
      ;; project: persisted
      (setq test-sb--answers '(project))
      (should (efrit-sandbox-check 'elisp t))
      (should (file-exists-p (efrit-sandbox-store-file root)))
      (should (= (logand (file-modes (efrit-sandbox-store-file root)) #o077) 0))
      ;; deny
      (setq test-sb--answers '(nil))
      (should-error (efrit-sandbox-check 'shell t) :type 'efrit-sandbox-denied))))

(ert-deftest test-sb-outside-target-suggestion-is-narrow ()
  (test-sb--in-project
    (defvar test-sb--seen nil)
    (let ((efrit-sandbox-request-function (lambda (req) (setq test-sb--seen req) nil))
          (outside (make-temp-file "efrit-sb-out-" t)))
      (unwind-protect
          (progn
            (ignore-errors (efrit-sandbox-check 'read (expand-file-name "deep/f.txt" outside)))
            ;; directory of the file, not / or ~
            (should (string-prefix-p (file-name-as-directory (file-truename outside))
                                     (efrit-sandbox-request-target test-sb--seen)))
            (should (string-suffix-p "deep/" (efrit-sandbox-request-target test-sb--seen))))
        (delete-directory outside t)))))

(ert-deftest test-sb-always-deny-cannot-be-granted ()
  (test-sb--in-project
    (let ((efrit-sandbox-request-function (lambda (_) 'project)))
      (should-error (efrit-sandbox-check 'read (expand-file-name ".efrit/x" root))
                    :type 'efrit-sandbox-denied)
      (should-error (efrit-sandbox-check 'write "~/.ssh/config") :type 'efrit-sandbox-denied)
      (should-not (gethash root efrit-sandbox--project-grants)))))

(ert-deftest test-sb-remote-root-is-a-different-scope ()
  (test-sb--in-project
    (efrit-sandbox-grant 'write root 'session)
    (should (efrit-sandbox-allowed-p 'write (expand-file-name "f" root)))
    ;; same localname on a remote host is NOT covered
    (should-not (efrit-sandbox--under-p (concat "/ssh:h:" root "f") root))))

(ert-deftest test-sb-disabled-is-noop ()
  (test-sb--in-project
    (let ((efrit-sandbox-enabled nil))
      (should (efrit-sandbox-check 'shell t))
      (should (efrit-sandbox-check 'write "/etc/x")))))

;;; Store

(ert-deftest test-sb-store-roundtrip ()
  (test-sb--in-project
    (efrit-sandbox-grant 'write root 'project)
    (efrit-sandbox-grant 'elisp t 'project)
    (let ((file (efrit-sandbox-store-file root)))
      (should (file-exists-p file))
      (clrhash efrit-sandbox--project-grants)
      (should (= 2 (length (efrit-sandbox-store-load root))))
      (should (efrit-sandbox-allowed-p 'elisp t))
      (should (efrit-sandbox-allowed-p 'write (expand-file-name "f" root)))
      ;; plain JSON, nothing else
      (with-temp-buffer
        (insert-file-contents file)
        (let ((obj (json-parse-buffer :object-type 'hash-table :array-type 'list)))
          (should (= 1 (gethash "version" obj)))
          (should (= 2 (length (gethash "grants" obj)))))))))

(ert-deftest test-sb-store-drops-invalid-entries ()
  "Bad caps, relative targets, non-project scopes, and non-JSON are ignored."
  (test-sb--in-project
    (let ((file (efrit-sandbox-store-file root)))
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert "{\"version\":1,\"grants\":["
                "{\"cap\":\"write\",\"target\":true,\"scope\":\"session\"},"     ; wrong scope
                "{\"cap\":\"root\",\"target\":true,\"scope\":\"project\"},"      ; unknown cap
                "{\"cap\":\"write\",\"target\":\"rel/path\",\"scope\":\"project\"},"  ; relative
                "{\"cap\":\"read\",\"target\":\"/tmp/ok/\",\"scope\":\"project\"}]}"))  ; valid
      (clrhash efrit-sandbox--project-grants)
      (let ((g (efrit-sandbox-store-load root)))
        (should (= 1 (length g)))
        (should (eq (plist-get (car g) :cap) 'read)))
      (should-not (efrit-sandbox-allowed-p 'write (expand-file-name "f" root)))
      (with-temp-file file (insert "not json {{{"))
      (clrhash efrit-sandbox--project-grants)
      (should-not (efrit-sandbox-store-load root)))))

;;; Eval enforcement: static

(ert-deftest test-sb-eval-inspect-refuses-sandbox-tampering ()
  (dolist (form '((efrit-sandbox-grant 'shell t 'project)
                  (setq efrit-sandbox-enabled nil)
                  (fset 'efrit-sandbox-check #'ignore)
                  (funcall (intern "efrit-sandbox-reset-session"))
                  (let ((file-name-handler-alist nil)) (delete-file "x"))
                  (advice-add 'write-region :around #'ignore)
                  (eval '(delete-file "x"))
                  (apply 'efrit-permission-reset nil)
                  ;; hidden behind a macro
                  (when t (symbol-function 'efrit-sandbox-check))))
    (should (efrit-sandbox-eval-inspect form))))

(ert-deftest test-sb-eval-inspect-accepts-ordinary-code ()
  (dolist (form '((+ 1 2)
                  (with-current-buffer "*scratch*" (buffer-string))
                  (find-file "/tmp/x")
                  (let ((f "a")) (insert-file-contents f))
                  (mapcar #'upcase '("a"))
                  (message "efrit says hi")))          ; plain string, fine
    (should-not (efrit-sandbox-eval-inspect form))))

;;; Eval enforcement: dynamic

(ert-deftest test-sb-eval-requires-elisp-capability ()
  (test-sb--in-project
    (should-error (efrit-sandbox-eval-form '(+ 1 2)) :type 'efrit-sandbox-denied)
    (efrit-sandbox-grant 'elisp t 'session)
    (should (= 3 (efrit-sandbox-eval-form '(+ 1 2))))))

(ert-deftest test-sb-eval-file-primitives-are-checked ()
  (test-sb--in-project
    (efrit-sandbox-grant 'elisp t 'session)
    (let ((inside (expand-file-name "in.txt" root))
          (outside (make-temp-file "efrit-sb-outside-")))
      (unwind-protect
          (progn
            (with-temp-file inside (insert "in"))
            (with-temp-file outside (insert "out"))
            ;; read inside: default grant
            (should (equal "in" (efrit-sandbox-eval-form
                                 `(with-temp-buffer (insert-file-contents ,inside) (buffer-string)))))
            ;; read outside: denied
            (should-error (efrit-sandbox-eval-form
                           `(with-temp-buffer (insert-file-contents ,outside) (buffer-string)))
                          :type 'efrit-sandbox-denied)
            (should (equal "out" (with-temp-buffer (insert-file-contents outside) (buffer-string))))
            ;; write inside without write grant: denied, file untouched
            (should-error (efrit-sandbox-eval-form `(with-temp-file ,inside (insert "changed")))
                          :type 'efrit-sandbox-denied)
            (should (equal "in" (with-temp-buffer (insert-file-contents inside) (buffer-string))))
            ;; delete outside: denied
            (should-error (efrit-sandbox-eval-form `(delete-file ,outside)) :type 'efrit-sandbox-denied)
            (should (file-exists-p outside))
            ;; with a write grant on the root, the write works
            (efrit-sandbox-grant 'write root 'session)
            (efrit-sandbox-eval-form `(with-temp-file ,inside (insert "changed")))
            (should (equal "changed" (with-temp-buffer (insert-file-contents inside) (buffer-string))))
            ;; find-file + save-buffer path
            (efrit-sandbox-eval-form
             `(with-current-buffer (find-file-noselect ,inside)
                (goto-char (point-max)) (insert "!") (save-buffer) (kill-buffer)))
            (should (string-prefix-p "changed!" (with-temp-buffer (insert-file-contents inside) (buffer-string)))))
        (ignore-errors (delete-file outside))))))

(ert-deftest test-sb-eval-processes-need-shell ()
  (test-sb--in-project
    (efrit-sandbox-grant 'elisp t 'session)
    (should-error (efrit-sandbox-eval-form '(shell-command-to-string "echo hi"))
                  :type 'efrit-sandbox-denied)
    (should-error (efrit-sandbox-eval-form '(call-process "true")) :type 'efrit-sandbox-denied)
    (should-error (efrit-sandbox-eval-form '(make-process :name "x" :command '("true")))
                  :type 'efrit-sandbox-denied)
    (efrit-sandbox-grant 'shell t 'session)
    (should (equal "hi\n" (efrit-sandbox-eval-form '(shell-command-to-string "echo hi"))))
    ;; the advice is inert outside a sandboxed eval
    (should (equal "ok\n" (shell-command-to-string "echo ok")))))

(ert-deftest test-sb-eval-network-needs-net ()
  (test-sb--in-project
    (efrit-sandbox-grant 'elisp t 'session)
    (should-error (efrit-sandbox-eval-form '(make-network-process :name "x" :host "localhost" :service 1))
                  :type 'efrit-sandbox-denied)))

(ert-deftest test-sb-eval-nothing-leaks-after-return ()
  "After the eval returns, file ops and processes are unrestricted again."
  (test-sb--in-project
    (efrit-sandbox-grant 'elisp t 'session)
    (ignore-errors (efrit-sandbox-eval-form '(+ 1 1)))
    (should-not efrit-sandbox-eval--active)
    (should-not (rassq #'efrit-sandbox-eval--handler file-name-handler-alist))
    (should (with-temp-buffer (insert-file-contents "/etc/hostname" nil 0 1) t))))

(provide 'test-sandbox)
;;; test-sandbox.el ends here

;;; UI

(require 'efrit-sandbox-ui)

(ert-deftest test-sb-ui-prompt-maps-keys-to-scopes ()
  (test-sb--in-project
    (cl-letf (((symbol-function 'efrit-sandbox-ui--note) #'ignore))
      (dolist (case '((?o . once) (?s . session) (?p . project) (?n . nil)))
        (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) (car case))))
          (should (eq (efrit-sandbox-ui-prompt
                       (efrit-sandbox-request-create :cap 'write :target root :tool "edit_file"))
                      (cdr case))))))))

(ert-deftest test-sb-ui-prompt-end-to-end-grants-and-persists ()
  "Through the real prompt: p grants for the project and writes the JSON."
  (test-sb--in-project
    (let ((efrit-sandbox-request-function #'efrit-sandbox-ui-prompt))
      (cl-letf (((symbol-function 'efrit-sandbox-ui--note) #'ignore)
                ((symbol-function 'read-char-choice) (lambda (&rest _) ?p)))
        (should (efrit-sandbox-check 'write (expand-file-name "x" root) "edit_file"))
        (should (file-exists-p (efrit-sandbox-store-file root)))
        ;; and it is remembered: no prompt this time
        (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) (error "must not prompt"))))
          (should (efrit-sandbox-check 'write (expand-file-name "y" root) "edit_file")))))))

(ert-deftest test-sb-ui-list-shows-and-revokes ()
  (test-sb--in-project
    (efrit-sandbox-grant 'write root 'project)
    (efrit-sandbox-grant 'shell t 'session)
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
      (efrit-sandbox root)
      (with-current-buffer "*efrit-sandbox*"
        (should (derived-mode-p 'efrit-sandbox-list-mode))
        (should (= 3 (length tabulated-list-entries)))   ; default read + 2
        ;; revoke the shell grant (last row)
        (goto-char (point-max)) (forward-line -1)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (efrit-sandbox-list-revoke))
        (should (= 2 (length tabulated-list-entries)))
        (should-not (efrit-sandbox-allowed-p 'shell t))
        (should (efrit-sandbox-allowed-p 'write (expand-file-name "f" root)))))))

(ert-deftest test-sb-permission-prompt-defers-to-sandbox ()
  "With the sandbox on, the per-call permission prompt asks nothing."
  (require 'efrit-permissions)
  (let ((efrit-sandbox-enabled t) (efrit-permission-policy '(write exec)))
    (should-not (efrit-permission-needed-p "eval_sexp")))
  (let ((efrit-sandbox-enabled nil) (efrit-permission-policy '(write exec)))
    (should (efrit-permission-needed-p "eval_sexp"))))
