;;; test-permissions.el --- Tests for efrit-permissions -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-permissions)
(require 'efrit-do-dispatch)
(require 'efrit-do-handlers)

(defun test-perm--input (&rest kvs)
  (let ((h (make-hash-table :test 'equal)))
    (while kvs (puthash (pop kvs) (pop kvs) h))
    h))

(defmacro test-perm--with-prompt-answer (answer &rest body)
  "Run BODY with the interactive prompt answering ANSWER (a symbol)."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'efrit-permission--prompt)
              (lambda (_req) ,answer)))
     ,@body))

(defmacro test-perm--fresh (&rest body)
  (declare (indent 0))
  `(let ((efrit-permission-policy '(write exec))
         (efrit-permission-responder-function nil)
         (efrit-permission--session-grants (make-hash-table :test 'equal)))
     ,@body))

;;; Classification

(ert-deftest test-perm-classes ()
  (should (eq (efrit-permission-tool-class "eval_sexp") 'exec))
  (should (eq (efrit-permission-tool-class "edit_file") 'write))
  (should (eq (efrit-permission-tool-class "read_file") 'read))
  (should (eq (efrit-permission-tool-class "session_complete") 'control))
  (should (eq (efrit-permission-tool-class "no_such_tool") 'read)))

(ert-deftest test-perm-read-and-control-never-gated ()
  (test-perm--fresh
    (cl-letf (((symbol-function 'efrit-permission--prompt)
               (lambda (_) (error "must not prompt"))))
      (should (eq (efrit-permission-check "read_file" (test-perm--input "path" "x")) 'allow))
      (should (eq (efrit-permission-check "todo_write" (test-perm--input)) 'allow))
      (should (eq (efrit-permission-check "editor_state" (test-perm--input)) 'allow)))))

(ert-deftest test-perm-policy-nil-disables ()
  (test-perm--fresh
    (let ((efrit-permission-policy nil))
      (cl-letf (((symbol-function 'efrit-permission--prompt)
                 (lambda (_) (error "must not prompt"))))
        (should (eq (efrit-permission-check "eval_sexp" (test-perm--input "expr" "(x)")) 'allow))))))

;;; Prompt outcomes

(ert-deftest test-perm-prompt-allow-once-does-not-persist ()
  (test-perm--fresh
    (test-perm--with-prompt-answer 'allow
      (should (eq (efrit-permission-check "eval_sexp" (test-perm--input "expr" "1") "s1") 'allow)))
    (test-perm--with-prompt-answer 'deny
      (should (eq (efrit-permission-check "eval_sexp" (test-perm--input "expr" "2") "s1") 'deny)))))

(ert-deftest test-perm-allow-tool-persists-for-session-only ()
  (test-perm--fresh
    (test-perm--with-prompt-answer 'allow-tool
      (should (eq (efrit-permission-check "edit_file" (test-perm--input "path" "a") "s1") 'allow)))
    ;; Same tool, same session: no prompt
    (cl-letf (((symbol-function 'efrit-permission--prompt)
               (lambda (_) (error "must not prompt"))))
      (should (eq (efrit-permission-check "edit_file" (test-perm--input "path" "b") "s1") 'allow)))
    ;; Different tool, same session: prompts
    (test-perm--with-prompt-answer 'deny
      (should (eq (efrit-permission-check "shell_exec" (test-perm--input "command" "ls") "s1") 'deny)))
    ;; Same tool, other session: prompts
    (test-perm--with-prompt-answer 'deny
      (should (eq (efrit-permission-check "edit_file" (test-perm--input "path" "c") "s2") 'deny)))))

(ert-deftest test-perm-allow-all-and-reset ()
  (test-perm--fresh
    (test-perm--with-prompt-answer 'allow-all
      (should (eq (efrit-permission-check "shell_exec" (test-perm--input "command" "ls") "s1") 'allow)))
    (cl-letf (((symbol-function 'efrit-permission--prompt)
               (lambda (_) (error "must not prompt"))))
      (should (eq (efrit-permission-check "eval_sexp" (test-perm--input "expr" "1") "s1") 'allow)))
    (efrit-permission-reset "s1")
    (test-perm--with-prompt-answer 'deny
      (should (eq (efrit-permission-check "eval_sexp" (test-perm--input "expr" "1") "s1") 'deny)))))

(ert-deftest test-perm-quit-at-prompt-denies ()
  (test-perm--fresh
    (cl-letf (((symbol-function 'efrit-permission--prompt)
               (lambda (_) (signal 'quit nil))))
      (should (eq (efrit-permission-check "eval_sexp" (test-perm--input "expr" "1")) 'deny)))))

;;; Responder function

(ert-deftest test-perm-responder-decides-and-sees-request ()
  (test-perm--fresh
    (let ((seen nil))
      (setq efrit-permission-responder-function
            (lambda (req) (setq seen req)
              (if (equal (alist-get :tool req) "shell_exec") 'deny 'allow)))
      (cl-letf (((symbol-function 'efrit-permission--prompt)
                 (lambda (_) (error "must not prompt"))))
        (should (eq (efrit-permission-check "shell_exec" (test-perm--input "command" "rm -rf x") "s1") 'deny))
        (should (equal (alist-get :class seen) 'exec))
        (should (string-match-p "rm -rf x" (alist-get :summary seen)))
        (should (equal (alist-get :session-id seen) "s1"))
        (should (eq (efrit-permission-check "edit_file" (test-perm--input "path" "p") "s1") 'allow))))))

(ert-deftest test-perm-responder-nil-falls-through-and-error-is-tolerated ()
  (test-perm--fresh
    (setq efrit-permission-responder-function (lambda (_) nil))
    (test-perm--with-prompt-answer 'allow
      (should (eq (efrit-permission-check "eval_sexp" (test-perm--input "expr" "1")) 'allow)))
    (setq efrit-permission-responder-function (lambda (_) (error "responder broke")))
    (test-perm--with-prompt-answer 'deny
      (should (eq (efrit-permission-check "eval_sexp" (test-perm--input "expr" "1")) 'deny)))))

(ert-deftest test-perm-responder-allow-tool-is-remembered ()
  (test-perm--fresh
    (setq efrit-permission-responder-function (lambda (_) 'allow-tool))
    (should (eq (efrit-permission-check "edit_file" (test-perm--input "path" "p") "s9") 'allow))
    (setq efrit-permission-responder-function nil)
    (cl-letf (((symbol-function 'efrit-permission--prompt)
               (lambda (_) (error "must not prompt"))))
      (should (eq (efrit-permission-check "edit_file" (test-perm--input "path" "q") "s9") 'allow)))))

;;; Summaries

(ert-deftest test-perm-summaries ()
  (should (string-match-p "\\$ git status"
                          (efrit-permission-summarize "shell_exec" (test-perm--input "command" "git status"))))
  (should (string-match-p "(message \"hi\")"
                          (efrit-permission-summarize "eval_sexp" (test-perm--input "expr" "(message \"hi\")"))))
  (let ((s (efrit-permission-summarize "edit_file"
                                       (test-perm--input "path" "a.el" "old_str" "foo" "new_str" "bar"))))
    (should (string-match-p "a.el" s))
    (should (string-match-p "--- old_str\nfoo" s))
    (should (string-match-p "\\+\\+\\+ new_str\nbar" s)))
  (let ((efrit-permission-summary-max-lines 2))
    (should (string-match-p "more lines"
                            (efrit-permission-summarize "eval_sexp" (test-perm--input "expr" "a\nb\nc\nd"))))))

;;; Dispatcher integration

(ert-deftest test-perm-dispatch-denial-returns-sentinel-without-running ()
  (test-perm--fresh
    (let ((ran nil))
      (cl-letf (((symbol-function 'efrit-permission--prompt) (lambda (_) 'deny))
                ((symbol-function 'efrit-do--handle-eval-sexp)
                 (lambda (&rest _) (setq ran t) "ran")))
        (efrit-do--circuit-breaker-reset)
        (let* ((item (test-perm--input "id" "t1" "name" "eval_sexp"
                                       "input" (test-perm--input "expr" "(delete-file \"x\")")))
               (result (efrit-do--execute-tool item)))
          (should-not ran)
          (should (string= result efrit-permission-denied-result))
          (should (string-prefix-p "Error " result)))))))

(ert-deftest test-perm-dispatch-allow-runs-tool ()
  (test-perm--fresh
    (cl-letf (((symbol-function 'efrit-permission--prompt) (lambda (_) 'allow))
              ((symbol-function 'efrit-do--handle-eval-sexp)
               (lambda (&rest _) "ran")))
      (efrit-do--circuit-breaker-reset)
      (let* ((item (test-perm--input "id" "t2" "name" "eval_sexp"
                                     "input" (test-perm--input "expr" "(+ 1 1)")))
             (result (efrit-do--execute-tool item)))
        (should (string-match-p "ran" result))))))

(provide 'test-permissions)
;;; test-permissions.el ends here
