;;; test-api.el --- request context in API failures -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-api)

(ert-deftest test-api-describe-failure-names-endpoint-purpose-model ()
  (let ((efrit-api-request-purpose nil) (efrit-default-model "m-default"))
    (let ((s (efrit-api-describe-failure "curl exit 7 (connection refused)"
                                         "https://gw.example.com/v1/messages?key=SECRET"
                                         "m-1" "curl (streaming)" "the model's next turn")))
      (should (string-match-p "\\`the model's next turn → https://gw.example.com/v1/messages (model m-1, via curl (streaming)) failed\\.\n" s))
      (should (string-match-p "connection refused" s))
      ;; the query string never appears
      (should-not (string-match-p "SECRET\\|key=" s)))
    ;; defaults: purpose from the dynamic var, model from efrit-default-model
    (let ((efrit-api-request-purpose "listing models"))
      (should (string-match-p "\\`listing models → .*model m-default" (efrit-api-describe-failure "x"))))))

(ert-deftest test-api-async-failure-carries-context ()
  "An error before the request leaves (no key) still says what was attempted."
  (let ((efrit-api-key nil) (got nil)
        (efrit-api-request-purpose "reviewing 2 proposed tool call(s)"))
    (cl-letf (((symbol-function 'efrit-common-get-api-key) (lambda () (error "No API key found"))))
      (efrit-api-request-async '(("model" . "m-2")) #'ignore (lambda (e) (setq got e)))
      (should got)
      (should (string-match-p "reviewing 2 proposed tool call" got))
      (should (string-match-p "model m-2" got))
      (should (string-match-p "No API key found" got)))))

(ert-deftest test-api-refusal-retries-with-inlined-system ()
  "A pre-generation refusal of a request with a system prompt is retried
once with the prompt folded into the first user message; the retry's
response is marked.  A refusal without a system prompt is returned as is."
  (let ((sent nil) (efrit-api-inline-system-on-refusal t))
    (cl-letf (((symbol-function 'efrit-api--request-sync-1)
               (lambda (req &optional _timeout)
                 (push req sent)
                 (let ((r (make-hash-table :test 'equal)))
                   (if (alist-get "system" req nil nil #'equal)
                       (progn (puthash "stop_reason" "refusal" r) (puthash "content" (vector) r))
                     (puthash "stop_reason" "end_turn" r))
                   r))))
      (let ((r (efrit-api-request-sync '(("model" . "m") ("system" . "Be brief.")
                                         ("messages" . [(("role" . "user") ("content" . "hi"))])))))
        (should (equal (gethash "stop_reason" r) "end_turn"))
        (should (gethash "efrit-inlined-system" r))
        (should (= 2 (length sent)))
        (let* ((retry (car sent))
               (first (aref (alist-get "messages" retry nil nil #'equal) 0)))
          (should-not (alist-get "system" retry nil nil #'equal))
          (should (string-match-p "\\`Be brief\\.\n\n---\n\nhi\\'" (alist-get "content" first nil nil #'equal)))))
      ;; a cacheable (vector) system prompt is flattened to its text
      (setq sent nil)
      (let ((r (efrit-api-request-sync `(("model" . "m")
                                         ("system" . ,(vector '(("type" . "text") ("text" . "Sys A")) '(("type" . "text") ("text" . "Sys B"))))
                                         ("messages" . [(("role" . "user") ("content" . "hi"))])))))
        (should (gethash "efrit-inlined-system" r))
        (should (string-prefix-p "Sys A\nSys B" (alist-get "content" (aref (alist-get "messages" (car sent) nil nil #'equal) 0) nil nil #'equal))))
      ;; no system prompt: no retry
      (setq sent nil)
      (cl-letf (((symbol-function 'efrit-api--request-sync-1)
                 (lambda (&rest _) (let ((r (make-hash-table :test 'equal))) (puthash "stop_reason" "refusal" r) r))))
        (let ((r (efrit-api-request-sync '(("model" . "m") ("messages" . [(("role" . "user") ("content" . "hi"))])))))
          (should (equal (gethash "stop_reason" r) "refusal")))))))

(provide 'test-api)
;;; test-api.el ends here
