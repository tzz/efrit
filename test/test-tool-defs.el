;;; test-tool-defs.el --- tools defined in JSON -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-tool-defs)
(require 'efrit-do-schema)
(require 'efrit-do-dispatch)
(require 'efrit-permissions)
(require 'efrit-tools)
(require 'efrit-do-handlers)

(defun test-td--def (&rest kv)
  "A definition hash table from KV; body is given as :kind and :source."
  (let ((h (make-hash-table :test 'equal)) (body (make-hash-table :test 'equal)))
    (while kv
      (let ((k (pop kv)) (v (pop kv)))
        (pcase k
          (:kind (puthash "kind" v body))
          (:source (puthash "source" v body))
          (_ (puthash k v h)))))
    (puthash "body" body h)
    h))

(defun test-td--schema (&rest fields)
  (let ((h (make-hash-table :test 'equal)) (props (make-hash-table :test 'equal)))
    (dolist (f fields)
      (let ((p (make-hash-table :test 'equal))) (puthash "type" "string" p) (puthash f p props)))
    (puthash "type" "object" h) (puthash "properties" props h) (puthash "required" nil h)
    h))

(defmacro test-td--with-roots (&rest body)
  "BODY with a fresh data directory, a fresh project root, empty registry
and definition state, the sandbox off."
  (declare (indent 0))
  `(let* ((efrit-data-directory (file-name-as-directory (make-temp-file "efrit-td-data-" t)))
          (root (file-name-as-directory (make-temp-file "efrit-td-root-" t)))
          (efrit-project-root root)
          (default-directory root)
          (efrit-tool-registry nil)
          (efrit-tool-defs--active nil)
          (efrit-tool-defs--loaded nil)
          (efrit-tool-defs--recent (make-hash-table :test 'equal))
          (efrit-tool-defs--noted (make-hash-table :test 'equal))
          (efrit-tool-defs--history nil)
          (efrit-tool-defs-enabled t)
          (efrit-sandbox-enabled nil)
          (efrit-current-session-id "td-test"))
     (unwind-protect (progn ,@body)
       (delete-directory efrit-data-directory t)
       (delete-directory root t))))

(ert-deftest test-tool-defs-validation-names-what-is-wrong ()
  "A definition is refused with a reason: bad name, a built-in name, no
description, unknown class, unknown kind, an elisp body that is not one
form or that the sandbox would refuse, a template naming an undeclared field."
  (test-td--with-roots
    (let ((good (test-td--def "name" "greet" "description" "d" "class" "read"
                              "input_schema" (test-td--schema "who")
                              :kind "elisp" :source "(concat \"hi \" (alist-get \"who\" input nil nil #'equal))")))
      (should-not (efrit-tool-defs-validate good))
      (let ((d (copy-hash-table good))) (puthash "name" "Greet" d)
           (should (string-match-p "lowercase" (efrit-tool-defs-validate d))))
      (let ((d (copy-hash-table good))) (puthash "name" "eval_sexp" d)
           (should (string-match-p "efrit's own" (efrit-tool-defs-validate d))))
      (let ((d (copy-hash-table good))) (remhash "description" d)
           (should (string-match-p "description" (efrit-tool-defs-validate d))))
      (let ((d (copy-hash-table good))) (puthash "class" "control" d)
           (should (string-match-p "class" (efrit-tool-defs-validate d))))
      (let ((d (test-td--def "name" "x" "description" "d" "class" "read" :kind "python" :source "1")))
        (should (string-match-p "body.kind" (efrit-tool-defs-validate d))))
      (let ((d (test-td--def "name" "x" "description" "d" "class" "read" :kind "elisp" :source "(+ 1 2) (+ 3 4)")))
        (should (string-match-p "one Lisp form" (efrit-tool-defs-validate d))))
      (let ((d (test-td--def "name" "x" "description" "d" "class" "read" :kind "elisp" :source "(fset 'car #'cdr)")))
        (should (string-match-p "body.source:" (efrit-tool-defs-validate d))))
      (let ((d (test-td--def "name" "x" "description" "d" "class" "exec" "input_schema" (test-td--schema "a")
                             :kind "template" :source "ls {{a}} {{b}}")))
        (should (string-match-p "fields the schema does not declare: b" (efrit-tool-defs-validate d)))))))

(ert-deftest test-tool-defs-files-load-into-the-registry-project-over-global ()
  "Definitions in the global and the project file register; a project one
shadows a global one of the same name; a broken one is skipped and the
rest load; a change on disk is picked up at the next load; nothing is
re-read when nothing changed."
  (test-td--with-roots
    (efrit-tool-defs--write-file
     (efrit-tool-defs-global-file)
     (list (test-td--def "name" "shout" "description" "global shout" "class" "read"
                         "input_schema" (test-td--schema "text")
                         :kind "elisp" :source "(upcase (alist-get \"text\" input nil nil #'equal))")
           (test-td--def "name" "only_global" "description" "g" "class" "read" :kind "elisp" :source "42")
           (test-td--def "name" "Broken" "description" "b" "class" "read" :kind "elisp" :source "1")))
    (efrit-tool-defs--write-file
     (efrit-tool-defs-project-file root)
     (list (test-td--def "name" "shout" "description" "project shout" "class" "read"
                         "input_schema" (test-td--schema "text")
                         :kind "elisp" :source "(concat (upcase (alist-get \"text\" input nil nil #'equal)) \"!\")")))
    (should (equal '("only_global" "shout") (sort (efrit-tool-defs-load root) #'string<)))
    (should (equal "project shout" (efrit-registered-tool-description (efrit-registered-tool-get "shout"))))
    (should (eq 'project (nth 2 (efrit-tool-defs-active "shout"))))
    ;; the schema and the permission class see them through the registry
    (should (cl-find "shout" (append (efrit-do--get-current-tools-schema) nil)
                     :key (lambda (e) (alist-get "name" e nil nil #'equal)) :test #'equal))
    (should (eq 'read (efrit-permission-tool-class "shout")))
    ;; the body runs with `input' bound, through eval_sexp's path
    (let ((efrit-tools-sexp-evaluation-enabled t))
      (should (equal "\"HEY!\"" (efrit-tool-registry-dispatch "shout" (let ((h (make-hash-table :test 'equal))) (puthash "text" "hey" h) h))))
      (should (equal "42" (efrit-tool-registry-dispatch "only_global" nil))))
    ;; unchanged files: no reload (the registry keeps what we put there)
    (efrit-register-tool "marker" :function #'ignore :package 'test)
    (efrit-tool-defs-load root)
    (should (efrit-registered-tool-get "marker"))
    ;; a change on disk: re-read, the marker from another package stays
    (sleep-for 1.1)
    (efrit-tool-defs--write-file (efrit-tool-defs-project-file root) nil)
    (should (equal '("only_global" "shout") (sort (efrit-tool-defs-load root) #'string<)))
    (should (equal "global shout" (efrit-registered-tool-description (efrit-registered-tool-get "shout"))))
    (should (efrit-registered-tool-get "marker"))))

(ert-deftest test-tool-defs-define-and-undefine-write-json-and-register ()
  "define_tool validates, writes the scope's file, registers at once; a
second definition of the name replaces the first; undefine_tool removes
it; list_tools shows scope, class and origin."
  (test-td--with-roots
    (let* ((input (make-hash-table :test 'equal)))
      (puthash "definition" (json-serialize
                             (let ((d (test-td--def "name" "double" "description" "Twice n." "class" "read"
                                                    "input_schema" (test-td--schema "n")
                                                    :kind "elisp" :source "(* 2 (string-to-number (alist-get \"n\" input nil nil #'equal)))")))
                               (efrit-settings--serializable d)))
               input)
      (puthash "scope" "project" input)
      (puthash "pattern" "eval_sexp(*) → eval_sexp(string-to-number)" input)
      (let ((r (efrit-tool-defs-handle-define input)))
        (should (string-match-p "Defined double (project, read class)" r)))
      (should (file-exists-p (efrit-tool-defs-project-file root)))
      (should (efrit-registered-tool-get "double"))
      (let ((efrit-tools-sexp-evaluation-enabled t))
        (should (equal "14" (efrit-tool-registry-dispatch "double" (let ((h (make-hash-table :test 'equal))) (puthash "n" "7" h) h)))))
      ;; the file holds one definition with its origin
      (let* ((defs (efrit-tool-defs--read-file (efrit-tool-defs-project-file root)))
             (origin (gethash "origin" (car defs))))
        (should (= 1 (length defs)))
        (should (equal "td-test" (gethash "session" origin)))
        (should (string-match-p "eval_sexp" (gethash "pattern" origin))))
      (let ((listing (efrit-tool-defs-handle-list nil)))
        (should (string-match-p "- double \\[project, read\\]: Twice n\\. (defined .* from eval_sexp" listing)))
      ;; redefining replaces
      (puthash "definition" (let ((d (test-td--def "name" "double" "description" "Twice n, v2." "class" "read"
                                                   :kind "elisp" :source "2"))) d) input)
      (efrit-tool-defs-handle-define input)
      (should (= 1 (length (efrit-tool-defs--read-file (efrit-tool-defs-project-file root)))))
      (should (equal "Twice n, v2." (efrit-registered-tool-description (efrit-registered-tool-get "double"))))
      ;; a bad one is an error result, nothing written
      (puthash "definition" (test-td--def "name" "read_file" "description" "d" "class" "read" :kind "elisp" :source "1") input)
      (should (string-match-p "\\[Error: define_tool: read_file is one of efrit's own tools\\]" (efrit-tool-defs-handle-define input)))
      ;; undefine
      (let ((u (make-hash-table :test 'equal)))
        (puthash "name" "double" u) (puthash "scope" "project" u)
        (should (string-match-p "Removed double" (efrit-tool-defs-handle-undefine u)))
        (should-not (efrit-registered-tool-get "double"))
        (should-not (file-exists-p (efrit-tool-defs-project-file root)))
        (should (string-match-p "No tool double" (efrit-tool-defs-handle-undefine u))))
      (should (string-match-p "No user-defined tools" (efrit-tool-defs-handle-list nil))))))

(ert-deftest test-tool-defs-template-body-runs-as-a-shell-line ()
  "A template body fills {{field}} placeholders, shell-quoted, and runs
through shell_exec."
  (skip-unless (executable-find "sh"))
  (test-td--with-roots
    (let ((def (test-td--def "name" "say" "description" "echo" "class" "exec"
                             "input_schema" (test-td--schema "what")
                             :kind "template" :source "echo {{what}}")))
      (should (equal "echo it\\'s" (efrit-tool-defs--fill-template "echo {{what}}" '(("what" . "it's")))))
      (efrit-tool-defs--register def 'project)
      (let* ((efrit-do-shell-timeout 30)
             (out (efrit-tool-registry-dispatch "say" (let ((h (make-hash-table :test 'equal))) (puthash "what" "a b" h) h))))
        (should (string-match-p "\\[Executed: echo a\\\\ b\\]" out))
        (should (string-match-p "\\[Result: a b" out))))))

(ert-deftest test-tool-defs-watch-notes-a-repeated-sequence-once ()
  "The same 2-call sequence three times in a session makes one note, with
the calls' shapes; a fourth round says nothing; a loop of one call is not
a routine; define_tool itself and defined tools are not watched; a
different session has its own memory."
  (test-td--with-roots
    (let ((published nil)
          (efrit-tool-defs-repeat-threshold 3))
      (cl-letf (((symbol-function 'efrit-publish) (lambda (type data) (push (cons type data) published))))
        (let ((eval-in (lambda (expr) (let ((h (make-hash-table :test 'equal))) (puthash "expr" expr h) h)))
              (shell-in (lambda (line) (let ((h (make-hash-table :test 'equal))) (puthash "command" line h) h))))
          ;; two rounds and a half: nothing yet (the shapes match, the
          ;; exact forms and lines differ)
          (should-not (efrit-tool-defs-watch "eval_sexp" (funcall eval-in "(project-root (project-current))")))
          (should-not (efrit-tool-defs-watch "shell_exec" (funcall shell-in "make test 2>&1 | tail")))
          (should-not (efrit-tool-defs-watch "eval_sexp" (funcall eval-in "(project-root 'x)")))
          (should-not (efrit-tool-defs-watch "shell_exec" (funcall shell-in "make check")))
          (should-not (efrit-tool-defs-watch "eval_sexp" (funcall eval-in "(project-root)")))
          ;; the third round completes the pattern
          (let ((note (efrit-tool-defs-watch "shell_exec" (funcall shell-in "make -k"))))
            (should note)
            (should (string-match-p "you ran eval_sexp(project-root) → shell_exec(make) 3 times" note))
            (should (string-match-p "define_tool" note)))
          (should (equal 'note (car (car published))))
          (should (string-match-p "⚙ you ran" (alist-get :text (cdr (car published)))))
          ;; a fourth round: already noted
          (should-not (efrit-tool-defs-watch "eval_sexp" (funcall eval-in "(project-root)")))
          (should-not (efrit-tool-defs-watch "shell_exec" (funcall shell-in "make")))
          (should (= 1 (length published)))
          ;; one call repeated is a loop, not a routine
          (dotimes (_ 6) (should-not (efrit-tool-defs-watch "read_file" nil)))
          ;; another session starts from nothing
          (let ((efrit-current-session-id "other"))
            (should-not (efrit-tool-defs-watch "eval_sexp" (funcall eval-in "(project-root)")))
            (should-not (efrit-tool-defs-watch "shell_exec" (funcall shell-in "make"))))
          ;; define_tool is never part of a pattern
          (dotimes (_ 6) (should-not (efrit-tool-defs-watch "define_tool" nil)))
          (efrit-tool-defs-forget "td-test")
          (should-not (gethash "td-test" efrit-tool-defs--recent)))))))

(ert-deftest test-tool-defs-define-under-the-sandbox-asks-once-and-takes-the-edit ()
  "With the sandbox on, define_tool asks for a write on the definitions
file with the JSON as the editable detail; the request is once-only
whatever the user picks; an edited definition is what gets defined; a
refusal defines nothing."
  (require 'efrit-sandbox)
  (require 'efrit-sandbox-eval)
  (test-td--with-roots
    (let* ((efrit-sandbox-enabled t)
           (efrit-sandbox-default-project-grants '(read))
           (efrit-sandbox--session-grants (make-hash-table :test 'equal))
           (efrit-sandbox--project-grants (make-hash-table :test 'equal))
           (efrit-sandbox--turn-state (make-hash-table :test 'equal))
           (efrit-sandbox-store--loaded (make-hash-table :test 'equal))
           (efrit-sandbox-expected-write-roots nil)
           (seen nil)
           (input (make-hash-table :test 'equal)))
      (puthash "definition" (test-td--def "name" "twice" "description" "Twice." "class" "read"
                                          "input_schema" (test-td--schema "n")
                                          :kind "elisp" :source "(* 2 (string-to-number (alist-get \"n\" input nil nil #'equal)))")
               input)
      (puthash "scope" "project" input)
      ;; the user edits the definition at the prompt: description and body change
      (let ((efrit-sandbox-request-function
             (lambda (req)
               (push req seen)
               (should (eq 'write (efrit-sandbox-request-cap req)))
               (should (equal "define_tool" (efrit-sandbox-request-tool req)))
               (should (efrit-sandbox-request-editable-p req))
               (should (efrit-sandbox-request-once-only-p req))
               (should (string-match-p "\"name\": \"twice\"" (efrit-sandbox-request-detail req)))
               (setf (efrit-sandbox-request-edited req)
                     (replace-regexp-in-string "(\\* 2 " "(* 3 "
                                               (replace-regexp-in-string "Twice\\." "Thrice." (efrit-sandbox-request-detail req))))
               'session)))
        (should (string-match-p "Defined twice (project, read class)" (efrit-tool-defs-handle-define input)))
        (should (= 1 (length seen)))
        (should (equal "Thrice." (efrit-registered-tool-description (efrit-registered-tool-get "twice"))))
        ;; the body runs as eval_sexp runs: under the project's elisp
        ;; grant, with no further prompt once that is in place
        (let ((efrit-tools-sexp-evaluation-enabled t)
              (efrit-sandbox-default-project-grants '(read elisp)))
          (should (equal "21" (efrit-tool-registry-dispatch "twice" (let ((h (make-hash-table :test 'equal))) (puthash "n" "7" h) h)))))
        (should (= 1 (length seen))))
      ;; a second definition asks again: the session answer did not stand
      (puthash "definition" (test-td--def "name" "other" "description" "d" "class" "read" :kind "elisp" :source "1") input)
      (let ((efrit-sandbox-request-function (lambda (req) (push req seen) nil)))
        (should (string-match-p "sandbox denied" (format "%s" (condition-case err (efrit-tool-defs-handle-define input)
                                                                 (efrit-sandbox-denied (format "sandbox denied %S" (cadr err)))))))
        (should (= 2 (length seen)))
        (should-not (efrit-registered-tool-get "other"))
        (should (= 1 (length (efrit-tool-defs--read-file (efrit-tool-defs-project-file root)))))))))

(ert-deftest test-tool-defs-watch-notes-a-routine-seen-in-enough-sessions ()
  "A 2-call sequence run once in each of three sessions of the same project
is noted in the third, from the history file; the file holds shapes
only; a fourth session says nothing; another project starts from zero;
undefining a tool whose origin names the pattern lets it be proposed
again; the within-session note also marks the history."
  (test-td--with-roots
    (let ((published nil)
          (efrit-tool-defs-session-threshold 3)
          (eval-in (lambda (expr) (let ((h (make-hash-table :test 'equal))) (puthash "expr" expr h) h)))
          (shell-in (lambda (line) (let ((h (make-hash-table :test 'equal))) (puthash "command" line h) h))))
      (cl-letf (((symbol-function 'efrit-publish) (lambda (type data) (push (cons type data) published))))
        (dolist (id '("morning-1" "morning-2"))
          (let ((efrit-current-session-id id)
                (efrit-tool-defs--recent (make-hash-table :test 'equal)))
            (should-not (efrit-tool-defs-watch "eval_sexp" (funcall eval-in "(vc-git-branch)")))
            (should-not (efrit-tool-defs-watch "shell_exec" (funcall shell-in "jira view ABC-1")))))
        ;; the file is there, with shapes and session ids, no arguments
        (let ((text (with-temp-buffer (insert-file-contents (efrit-tool-defs-history-file)) (buffer-string))))
          (should (string-match-p "eval_sexp(vc-git-branch) → shell_exec(jira)" text))
          (should (string-match-p "morning-2" text))
          (should-not (string-match-p "ABC-1" text)))
        ;; a fresh Emacs: the history is read back from disk
        (setq efrit-tool-defs--history nil)
        (let ((efrit-current-session-id "morning-3")
              (efrit-tool-defs--recent (make-hash-table :test 'equal)))
          (should-not (efrit-tool-defs-watch "eval_sexp" (funcall eval-in "(vc-git-branch)")))
          (let ((note (efrit-tool-defs-watch "shell_exec" (funcall shell-in "jira view ABC-2"))))
            (should note)
            (should (string-match-p "you ran eval_sexp(vc-git-branch) → shell_exec(jira) in 3 separate sessions of this project" note)))
          (should (= 1 (length published))))
        ;; the fourth session: noted already
        (let ((efrit-current-session-id "morning-4")
              (efrit-tool-defs--recent (make-hash-table :test 'equal)))
          (should-not (efrit-tool-defs-watch "eval_sexp" (funcall eval-in "(vc-git-branch)")))
          (should-not (efrit-tool-defs-watch "shell_exec" (funcall shell-in "jira view ABC-3"))))
        ;; another project: its own count
        (let* ((other (file-name-as-directory (make-temp-file "efrit-td-other-" t)))
               (efrit-project-root other) (default-directory other)
               (efrit-current-session-id "elsewhere")
               (efrit-tool-defs--recent (make-hash-table :test 'equal)))
          (unwind-protect
              (progn
                (should-not (efrit-tool-defs-watch "eval_sexp" (funcall eval-in "(vc-git-branch)")))
                (should-not (efrit-tool-defs-watch "shell_exec" (funcall shell-in "jira view X-1")))
                (should (= 1 (length (gethash "sessions"
                                              (gethash "eval_sexp(vc-git-branch) → shell_exec(jira)"
                                                       (gethash (abbreviate-file-name other) (efrit-tool-defs--history))))))))
            (delete-directory other t)))
        ;; a tool defined from the pattern, then removed: the routine can be proposed again
        (let ((input (make-hash-table :test 'equal)))
          (puthash "definition" (test-td--def "name" "ticket" "description" "d" "class" "read" :kind "elisp" :source "1") input)
          (puthash "pattern" "eval_sexp(vc-git-branch) → shell_exec(jira)" input)
          (should (string-match-p "Defined ticket" (efrit-tool-defs-handle-define input)))
          (let ((u (make-hash-table :test 'equal)))
            (puthash "name" "ticket" u)
            (should (string-match-p "Removed ticket" (efrit-tool-defs-handle-undefine u)))))
        (let ((efrit-current-session-id "morning-5")
              (efrit-tool-defs--recent (make-hash-table :test 'equal)))
          (should-not (efrit-tool-defs-watch "eval_sexp" (funcall eval-in "(vc-git-branch)")))
          (should (efrit-tool-defs-watch "shell_exec" (funcall shell-in "jira view ABC-5"))))
        ;; the within-session detector marks the history too: no second
        ;; note from the other memory later
        (let ((efrit-current-session-id "busy")
              (efrit-tool-defs--recent (make-hash-table :test 'equal))
              (before (length published)))
          (dotimes (_ 2)
            (efrit-tool-defs-watch "eval_sexp" (funcall eval-in "(project-root)"))
            (efrit-tool-defs-watch "eval_sexp" (funcall eval-in "(buffer-list)")))
          (efrit-tool-defs-watch "eval_sexp" (funcall eval-in "(project-root)"))
          (should (efrit-tool-defs-watch "eval_sexp" (funcall eval-in "(buffer-list)")))
          (should (= (1+ before) (length published)))
          (should (efrit-tool-defs--history-noted-p
                   (gethash "eval_sexp(project-root) → eval_sexp(buffer-list)"
                            (gethash (efrit-tool-defs--history-root) (efrit-tool-defs--history))))))))))

(provide 'test-tool-defs)
;;; test-tool-defs.el ends here
