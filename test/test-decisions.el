;;; test-decisions.el --- efrit-decisions: backends, protocol, many, tool, command, gnus triage -*- lexical-binding: t; -*-

;;; Commentary:

;; Over a fake curl (test/scripts/mock-decisions-curl.py) that answers
;; from the input text, so every expectation is predictable.  Nothing
;; touches the network.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-http)
(require 'efrit-sandbox)
(require 'efrit-decisions)
(require 'efrit-tool-decisions)
(require 'efrit-gnus)

(defun test-decisions--raw (subject body)
  "A raw RFC 822 message with SUBJECT and BODY."
  (concat "From: Ann <ann@example.com>\r\nTo: me@example.com\r\nDate: Mon, 01 Sep 2026 10:00:00 +0000\r\n"
          "Subject: " subject "\r\nMessage-ID: <" subject "@x>\r\nMIME-Version: 1.0\r\n"
          "Content-Type: text/plain; charset=utf-8\r\n\r\n" body "\r\n"))

(defmacro test-decisions--with-articles (articles &rest body)
  "Run BODY with `gnus-request-article' serving ARTICLES ((GROUP . NUMBER) . RAW)."
  (declare (indent 1))
  `(let ((served ,articles))
     (cl-letf (((symbol-function 'gnus-request-article)
                (lambda (number group &optional buffer)
                  (when-let* ((raw (cdr (assoc (cons group number) served))))
                    (with-current-buffer (or buffer (current-buffer))
                      (insert raw))
                    t)))
               ((symbol-function 'gnus-request-head) (lambda (&rest _) nil)))
       ,@body)))

(defconst test-decisions--script
  (expand-file-name "scripts/mock-decisions-curl.py"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defconst test-decisions--questions
  (list (efrit-decisions-question 'predicate "urgent" "Does it need an answer today?")
        (efrit-decisions-question 'choice "kind" "What is it?"
                                  :choices '(("bug" . "broken") ("ask" . "a request") "other"))
        (efrit-decisions-question 'score "mood" "How does the writer feel?"
                                  :levels '("angry" "neutral" "happy"))))

(defmacro test-decisions--with (&rest body)
  "Run BODY with the fake curl, a capture file, and one backend."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "efrit-decisions-test" t))
          (capture (expand-file-name "capture.jsonl" dir))
          (efrit-http-curl-program test-decisions--script)
          (efrit-decisions-backends nil)
          (efrit-decisions-parallel 2)
          (efrit-http--active nil)
          (efrit-api-key "sk-test-key-1234567890abcdefghij")
          (efrit-api-auth-scheme 'x-api-key)
          (efrit-api-request-transforms nil)
          (efrit-sandbox-enabled nil)
          (process-environment (cons (concat "MOCK_DECISIONS_CAPTURE=" capture) process-environment)))
     (skip-unless (executable-find "python3"))
     (efrit-decisions-add-backend :name "fast" :protocol 'decisions-v1
                                  :url "https://gw.example.test/v1/decisions" :model "example/Decision-1"
                                  :options '(("temperature" . 0)))
     (unwind-protect (progn ,@body)
       (efrit-http-cancel)
       (delete-directory dir t))))

(defun test-decisions--captured (dir)
  "The requests the fake curl recorded under DIR, oldest first."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "capture.jsonl" dir))
    (mapcar (lambda (l) (json-parse-string l :object-type 'alist :array-type 'list))
            (split-string (buffer-string) "\n" t))))

(ert-deftest test-decisions-questions-are-checked ()
  "Malformed questions are refused with a reason before any request."
  (should-error (efrit-decisions-check-questions nil))
  (should-error (efrit-decisions-check-questions '((:type guess :name "x" :instructions "y"))))
  (should-error (efrit-decisions-check-questions '((:type predicate :instructions "y"))))
  (should-error (efrit-decisions-check-questions '((:type choice :name "x" :instructions "y" :choices ("a")))))
  (should-error (efrit-decisions-check-questions '((:type score :name "x" :instructions "y" :levels ("a")))))
  (should-error (efrit-decisions-check-questions (list (efrit-decisions-question 'predicate "x" "y")
                                                       (efrit-decisions-question 'predicate "x" "z"))))
  (should (efrit-decisions-check-questions test-decisions--questions))
  (should-error (efrit-decisions-question 'maybe "x" "y")))

(ert-deftest test-decisions-request-and-parse ()
  "The decisions-v1 body carries model, input and the questions in wire
shape with the backend's options; answers normalize per type and a
short or error response is a readable error."
  (test-decisions--with
    (let ((body (efrit-decisions-request 'decisions-v1 (efrit-decisions-backend "fast")
                                         "hello" test-decisions--questions '(("temperature" . 0.5)))))
      (should (equal "example/Decision-1" (alist-get "model" body nil nil #'equal)))
      (should (equal "hello" (alist-get "input" body nil nil #'equal)))
      (should (equal 0.5 (alist-get "temperature" body nil nil #'equal)))
      (let ((qs (append (alist-get "questions" body nil nil #'equal) nil)))
        (should (= 3 (length qs)))
        (should (equal "predicate" (alist-get "type" (nth 0 qs) nil nil #'equal)))
        (should (equal [(("value" . "bug") ("description" . "broken"))
                        (("value" . "ask") ("description" . "a request"))
                        (("value" . "other") ("description" . ""))]
                       (alist-get "choices" (nth 1 qs) nil nil #'equal)))
        (should (equal "angry" (alist-get "label" (aref (alist-get "levels" (nth 2 qs) nil nil #'equal) 0) nil nil #'equal)))))
    (let ((answers (efrit-decisions-parse
                    'decisions-v1 test-decisions--questions
                    "{\"answers\":[{\"probability\":0.93},{\"choice\":\"ask\",\"probabilities\":{\"bug\":0.1,\"ask\":0.8,\"other\":0.1}},{\"score\":1.7}]}")))
      (should (= 3 (length answers)))
      (should (equal 0.93 (alist-get 'probability (nth 0 answers))))
      (should (alist-get 'value (nth 0 answers)))
      (should (equal "ask" (alist-get 'choice (nth 1 answers))))
      (should (equal 0.8 (alist-get 'probability (nth 1 answers))))
      (should (equal 0.8 (alist-get "ask" (alist-get 'probabilities (nth 1 answers)) nil nil #'equal)))
      (should (equal 1.7 (alist-get 'score (nth 2 answers))))
      (should (equal "happy" (alist-get 'level (nth 2 answers))))
      (should (equal "yes 0.93" (efrit-decisions-format-answer (nth 0 answers))))
      (should (equal "ask 0.80" (efrit-decisions-format-answer (nth 1 answers))))
      (should (equal "happy 1.70" (efrit-decisions-format-answer (nth 2 answers)))))
    ;; a list of probabilities in the options' order is read too; a
    ;; missing choice is taken from the largest
    (let ((answers (efrit-decisions-parse
                    'decisions-v1 (list (nth 1 test-decisions--questions))
                    "{\"answers\":[{\"probabilities\":[0.2,0.1,0.7]}]}")))
      (should (equal "other" (alist-get 'choice (car answers))))
      (should (equal 0.7 (alist-get 'probability (car answers)))))
    (should-error (efrit-decisions-parse 'decisions-v1 test-decisions--questions "{\"answers\":[{\"probability\":0.5}]}"))
    (should-error (efrit-decisions-parse 'decisions-v1 test-decisions--questions "{\"error\":{\"message\":\"nope\"}}"))))

(ert-deftest test-decisions-decide-over-curl ()
  "A decision goes out as curl with efrit's key and the backend's options,
and the answers come back normalized; a failing endpoint is a readable
error naming the backend."
  (test-decisions--with
    (let ((answers (efrit-decide "yes, there is a bug and I am angry" test-decisions--questions)))
      (should (equal 0.9 (alist-get 'probability (efrit-decisions-answer answers "urgent"))))
      (should (equal "bug" (alist-get 'choice (efrit-decisions-answer answers "kind"))))
      (should (equal 0.0 (alist-get 'score (efrit-decisions-answer answers "mood"))))
      (should (equal "angry" (alist-get 'level (efrit-decisions-answer answers "mood")))))
    (let ((sent (car (test-decisions--captured dir))))
      (should (equal "https://gw.example.test/v1/decisions" (alist-get 'url sent)))
      (should (equal 0 (alist-get 'temperature (alist-get 'body sent))))
      (should (string-match-p "x-api-key: sk-test" (alist-get 'config sent))))
    (efrit-decisions-add-backend :name "broken" :protocol 'decisions-v1
                                 :url "https://gw.example.test/v1/error400" :model "m")
    (let ((err (should-error (efrit-decide "x" test-decisions--questions :backend "broken"))))
      (should (string-match-p "broken: model not available" (cadr err))))
    (efrit-decisions-add-backend :name "short" :protocol 'decisions-v1
                                 :url "https://gw.example.test/v1/short" :model "m")
    (should-error (efrit-decide "x" test-decisions--questions :backend "short"))
    (should-error (efrit-decide "x" test-decisions--questions :backend "nope") :type 'user-error)))

(ert-deftest test-decisions-many-keeps-order-and-parallel-cap ()
  "decide-many answers in the inputs' order whatever the completion order,
runs at most `efrit-decisions-parallel' at once, reports progress, and
a cancel stops what is left."
  (test-decisions--with
    (efrit-decisions-add-backend :name "slow" :protocol 'decisions-v1
                                 :url "https://gw.example.test/v1/slow" :model "m")
    (let* ((inputs '("one yes bug" "two ask happy" "three other" "four yes neutral" "five"))
           (progress nil) (peak 0)
           (results (progn
                      ;; sample the in-flight count while waiting
                      (let ((done nil) (out nil))
                        (efrit-decide-many inputs test-decisions--questions
                                           (lambda (r) (setq out r done t))
                                           :backend "slow"
                                           :progress (lambda (d n) (push (cons d n) progress)))
                        (while (not done)
                          (setq peak (max peak (length efrit-http--active)))
                          (accept-process-output nil 0.05))
                        out))))
      (should (= 5 (length results)))
      (should (equal inputs (mapcar #'car results)))
      (should (cl-every (lambda (r) (null (nth 2 r))) results))
      (should (equal '("bug" "ask" "other" "other" "other")
                     (mapcar (lambda (r) (alist-get 'choice (efrit-decisions-answer (nth 1 r) "kind"))) results)))
      (should (equal '(t nil nil t nil)
                     (mapcar (lambda (r) (alist-get 'value (efrit-decisions-answer (nth 1 r) "urgent"))) results)))
      (should (<= peak 2))
      (should (>= peak 1))
      (should (equal '((1 . 5) (2 . 5) (3 . 5) (4 . 5) (5 . 5)) (reverse progress))))
    ;; the sync form, and an empty input list
    (should (= 2 (length (efrit-decide-many-sync '("a" "b") test-decisions--questions))))
    (should (null (efrit-decide-many-sync nil test-decisions--questions)))
    ;; cancel: in-flight requests end, the callback never fires
    (let* ((fired nil)
           (cancel (efrit-decide-many '("a" "b" "c") test-decisions--questions (lambda (_) (setq fired t))
                                      :backend "slow")))
      (should (> (funcall cancel) 0))
      (let ((deadline (+ (float-time) 3)))
        (while (and efrit-http--active (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should-not fired)
      (should (null efrit-http--active)))))

(ert-deftest test-decisions-tool-registers-and-runs ()
  "The decide tool is always registered (its description says when no
backend is configured and a call says the fix), takes JSON-shaped
questions, asks the sandbox for the host, and returns one row per input."
  (test-decisions--with
    (efrit-decisions-ensure-tools)
    (should (efrit-registered-tool-get "decide"))
    (should (eq 'exec (efrit-tool-registry-class "decide")))
    (let ((asked nil))
      (cl-letf (((symbol-function 'efrit-sandbox-check)
                 (lambda (cap target tool _detail) (setq asked (list cap target tool)) t)))
        (let* ((efrit-sandbox-enabled t)
               (input (let ((h (make-hash-table :test 'equal)))
                        (puthash "inputs" ["yes bug angry" "ask happy"] h)
                        (puthash "questions"
                                 (vector (let ((q (make-hash-table :test 'equal)))
                                           (puthash "type" "predicate" q) (puthash "name" "urgent" q)
                                           (puthash "instructions" "Urgent?" q) q)
                                         (let ((q (make-hash-table :test 'equal)))
                                           (puthash "type" "choice" q) (puthash "name" "kind" q)
                                           (puthash "instructions" "Kind?" q)
                                           (puthash "choices" (vector (let ((c (make-hash-table :test 'equal)))
                                                                        (puthash "value" "bug" c) (puthash "description" "broken" c) c)
                                                                      "ask" "other")
                                                    q)
                                           q)
                                         (let ((q (make-hash-table :test 'equal)))
                                           (puthash "type" "score" q) (puthash "name" "mood" q)
                                           (puthash "instructions" "Mood?" q)
                                           (puthash "levels" ["angry" "neutral" "happy"] q) q))
                                 h)
                        h))
               (result (efrit-tool-registry-dispatch "decide" input)))
          (should (equal '(net (host . "gw.example.test") "decide") asked))
          (should (string-match-p "\\`2 inputs, 3 questions, fast" result))
          (should (string-match-p "yes bug angry\turgent=yes 0.90\tkind=bug 0.80\tmood=angry 0.00" result))
          (should (string-match-p "ask happy\turgent=no 0.10\tkind=ask 0.80\tmood=happy 2.00" result)))))
    ;; errors are results, not signals
    (should (string-match-p "inputs is required"
                            (efrit-tool-registry-dispatch "decide" (make-hash-table :test 'equal))))
    (efrit-decisions-remove-backend "fast")
    (should (efrit-registered-tool-get "decide"))
    (should (string-match-p "NONE CONFIGURED" (efrit-registered-tool-description (efrit-registered-tool-get "decide"))))
    (should (string-match-p "no decision backend configured"
                            (efrit-tool-registry-dispatch "decide"
                                                          (let ((h (make-hash-table :test 'equal)))
                                                            (puthash "inputs" ["x"] h)
                                                            (puthash "questions" (vector (let ((q (make-hash-table :test 'equal)))
                                                                                           (puthash "type" "predicate" q) (puthash "name" "a" q)
                                                                                           (puthash "instructions" "b" q) q))
                                                                     h)
                                                            h))))))

(ert-deftest test-decisions-lines-command-and-question-sets ()
  "efrit-decide-lines asks about each non-blank line of the region with a
named set from the questions file (or one typed yes/no question) and
shows a sortable table; w copies it as TSV."
  (test-decisions--with
    (let ((efrit-decisions-questions-file (expand-file-name "decisions.el" dir)))
      (with-temp-file efrit-decisions-questions-file
        (prin1 `(("triage" . ,test-decisions--questions)) (current-buffer)))
      (should (equal '("triage") (mapcar #'car (efrit-decisions-question-sets))))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "triage")))
        (should (equal test-decisions--questions (efrit-decisions-read-questions))))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Is it on fire?")))
        (let ((qs (efrit-decisions-read-questions)))
          (should (= 1 (length qs)))
          (should (eq 'predicate (plist-get (car qs) :type)))
          (should (equal "Is it on fire?" (plist-get (car qs) :instructions)))))
      (with-temp-buffer
        (insert "yes bug angry\n\n   ask happy  \nother\n")
        (cl-letf (((symbol-function 'pop-to-buffer) #'set-buffer))
          (efrit-decide-lines (point-min) (point-max) test-decisions--questions)))
      (with-current-buffer "*efrit decisions*"
        (should (eq major-mode 'efrit-decisions-results-mode))
        (should (= 3 (length tabulated-list-entries)))
        (should (equal "yes bug angry" (aref (cadr (nth 0 tabulated-list-entries)) 0)))
        (should (equal "0.90" (aref (cadr (nth 0 tabulated-list-entries)) 1)))
        (should (equal "bug 0.80" (aref (cadr (nth 0 tabulated-list-entries)) 2)))
        (should (equal "2.00 happy" (aref (cadr (nth 1 tabulated-list-entries)) 3)))
        (efrit-decisions-results-copy)
        (should (string-match-p "\\`Input\turgent\tkind\tmood\nyes bug angry\t0.90\tbug 0.80\t0.00 angry\n" (current-kill 0)))
        (kill-buffer)))))

(ert-deftest test-decisions-gnus-triage-table ()
  "efrit-gnus-triage renders each article, asks the decision model, and
shows a table sorted by urgency with subject and sender; without a
backend it says what to configure."
  (test-decisions--with
    (let ((efrit-gnus-triage-questions
           (list (efrit-decisions-question 'choice "action" "What?"
                                           :choices '("reply-today" "read" "ignore"))
                 (efrit-decisions-question 'score "urgency" "How soon?"
                                           :levels '("none" "low" "high")))))
      (test-decisions--with-articles
          (list (cons '("nnml:mail" . 1) (test-decisions--raw "Newsletter" "read this, urgency none"))
                (cons '("nnml:mail" . 2) (test-decisions--raw "Outage" "reply-today, urgency high"))
                (cons '("nnml:mail" . 3) (test-decisions--raw "Lunch" "read; low urgency")))
        (cl-letf (((symbol-function 'pop-to-buffer) #'set-buffer))
          (efrit-gnus-triage '(("nnml:mail" . 1) ("nnml:mail" . 2) ("nnml:mail" . 3))))
        (with-current-buffer "*efrit triage*"
          (should (= 3 (length tabulated-list-entries)))
          ;; sorted by urgency: Outage (high=2) first, then Lunch (low=1), then Newsletter (none=0)
          (should (equal '(("nnml:mail" . 2) ("nnml:mail" . 3) ("nnml:mail" . 1))
                         (mapcar #'car tabulated-list-entries)))
          (should (equal "reply-today 0.80" (aref (cadr (car tabulated-list-entries)) 2)))
          (should (equal "high 2.00" (aref (cadr (car tabulated-list-entries)) 3)))
          (should (lookup-key (current-local-map) (kbd "RET")))
          (kill-buffer))))
    (let ((efrit-decisions-backends nil))
      (should-error (efrit-gnus-triage '(("nnml:mail" . 1))) :type 'user-error))))

(provide 'test-decisions)
;;; test-decisions.el ends here
