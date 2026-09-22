;;; test-efrit-gnus.el --- Tests for efrit-gnus -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-gnus)
(require 'efrit-tool-registry)

(defvar test-gnus--articles nil
  "Alist ((GROUP . NUMBER) . RAW) served by the mocked `gnus-request-article'.")

(defun test-gnus--raw (subject body &optional html attachment)
  "A raw RFC 822 message with SUBJECT and text BODY; optional HTML alternative and ATTACHMENT."
  (let ((boundary "b1"))
    (concat "From: Ann <ann@example.com>\r\nTo: me@example.com\r\nDate: Mon, 01 Sep 2026 10:00:00 +0000\r\n"
            "Subject: " subject "\r\nMessage-ID: <" subject "@x>\r\nMIME-Version: 1.0\r\n"
            (if (or html attachment)
                (concat "Content-Type: multipart/mixed; boundary=\"" boundary "\"\r\n\r\n"
                        "--" boundary "\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n" body "\r\n"
                        (when html (concat "--" boundary "\r\nContent-Type: text/html\r\n\r\n" html "\r\n"))
                        (when attachment
                          (concat "--" boundary "\r\nContent-Type: application/pdf; name=\"" attachment
                                  "\"\r\nContent-Disposition: attachment; filename=\"" attachment
                                  "\"\r\n\r\n%PDF-1.4 fake\r\n"))
                        "--" boundary "--\r\n")
              (concat "Content-Type: text/plain; charset=utf-8\r\n\r\n" body "\r\n")))))

(defmacro test-gnus--with-articles (articles &rest body)
  "Run BODY with `gnus-request-article' serving ARTICLES ((GROUP . NUMBER) . RAW)."
  (declare (indent 1))
  `(let ((test-gnus--articles ,articles))
     (cl-letf (((symbol-function 'gnus-request-article)
                (lambda (number group &optional buffer)
                  (when-let* ((raw (cdr (assoc (cons group number) test-gnus--articles))))
                    (with-current-buffer (or buffer (current-buffer))
                      (insert raw))
                    t)))
               ((symbol-function 'gnus-request-head) (lambda (&rest _) nil)))
       ,@body)))

(ert-deftest test-efrit-gnus-render-article ()
  "An article renders as headers, reference, attachments and decoded body;
long bodies are cut with a marker."
  (test-gnus--with-articles
      `((("nnml:mail" . 1) . ,(encode-coding-string
                               (test-gnus--raw "Plan" "Körper ✓ line one.\n\nline two." nil "deck.pdf")
                               'utf-8))
        (("nnml:mail" . 2) . ,(test-gnus--raw "Long" (make-string 500 ?x))))
    (let ((text (efrit-gnus--render "nnml:mail" 1)))
      (should (string-match-p "^From: Ann <ann@example.com>$" text))
      (should (string-match-p "^Subject: Plan$" text))
      (should (string-match-p "^Article: nnml:mail#1$" text))
      (should (string-match-p "^Attachments: deck.pdf (application/pdf" text))
      (should (string-match-p "Körper ✓ line one\\.\n\nline two\\." text)))
    (let* ((efrit-gnus-article-max-chars 100)
           (text (efrit-gnus--render "nnml:mail" 2)))
      (should (string-match-p "\\[\\.\\.\\. 400 more characters not shown \\.\\.\\.\\]" text)))
    (should-not (efrit-gnus--render "nnml:mail" 99))))

(ert-deftest test-efrit-gnus-payload-budget ()
  "The payload keeps whole articles up to the budget and hands back the rest
as the next batch; numbering runs over the whole selection."
  (test-gnus--with-articles
      (mapcar (lambda (n) (cons (cons "nnml:mail" n) (test-gnus--raw (format "m%d" n) (make-string 300 ?y))))
              '(1 2 3))
    (pcase-let* ((efrit-gnus-max-chars 900)
                 (`(,text ,sent ,chars ,rest)
                  (efrit-gnus--payload '(("nnml:mail" . 1) ("nnml:mail" . 2) ("nnml:mail" . 3)))))
      (should (= 2 sent))
      (should (<= chars 900))
      (should (string-match-p "=== Article 1 of 3 ===" text))
      (should (string-match-p "=== Article 2 of 3 ===" text))
      (should-not (string-match-p "Article 3 of 3" text))
      (should (equal '(("nnml:mail" . 3)) rest))
      ;; The next batch continues the numbering.
      (pcase-let ((`(,text2 ,sent2 ,_ ,rest2) (efrit-gnus--payload rest '(2 . 3))))
        (should (= 1 sent2))
        (should (string-match-p "=== Article 3 of 3 ===" text2))
        (should-not rest2)))
    ;; An article bigger than the budget still goes out, alone.
    (pcase-let* ((efrit-gnus-max-chars 100)
                 (`(,text ,sent ,_ ,rest) (efrit-gnus--payload '(("nnml:mail" . 1) ("nnml:mail" . 2)))))
      (should (= 1 sent))
      (should (string-match-p "Article 1 of 2" text))
      (should (equal '(("nnml:mail" . 2)) rest)))))

(ert-deftest test-efrit-gnus-batches-over-turns ()
  "A selection over the budget is sent as consecutive turns: the first at
once, each next one when the agent reports idle, then a closing message."
  (test-gnus--with-articles
      (mapcar (lambda (n) (cons (cons "nnml:mail" n) (test-gnus--raw (format "m%d" n) (make-string 300 ?y))))
              '(1 2 3 4 5))
    (let ((submitted nil) (subscribed nil) (timers nil))
      (cl-letf (((symbol-function 'efrit-submit)
                 (lambda (shown api) (push (cons shown api) submitted) t))
                ((symbol-function 'efrit-gnus-ensure-tools) #'ignore)
                ((symbol-function 'efrit-gnus--require) (lambda (&rest _) t))
                ((symbol-function 'efrit-subscribe) (lambda (type fn) (push (cons type fn) subscribed) fn))
                ((symbol-function 'run-at-time) (lambda (_s _r fn &rest _) (push fn timers) nil)))
        (let ((efrit-gnus-max-chars 900) (efrit-gnus-confirm nil)
              (efrit-gnus--queue nil) (efrit-gnus--closing nil) (efrit-gnus--watching nil))
          (efrit-gnus--submit (mapcar (lambda (n) (cons "nnml:mail" n)) '(1 2 3 4 5))
                              '("Triage." . "Overall triage.") "unread in mail")
          ;; First batch went out: articles 1-2 of 5, and it says so.
          (should (= 1 (length submitted)))
          (should (equal "analyze articles 1-2 of 5 from unread in mail: Triage." (car (car submitted))))
          (should (string-match-p "articles 1-2 here, the rest follow" (cdr (car submitted))))
          (should (= 1 (length efrit-gnus--queue)))
          (should subscribed)
          ;; Agent goes idle: the next batch is scheduled and sent.
          (efrit-gnus--send-next '((:status . idle)))
          (should timers) (funcall (pop timers))
          (should (= 2 (length submitted)))
          (should (equal "analyze articles 3-4 of 5 from unread in mail: Triage." (car (car submitted))))
          (efrit-gnus--send-next '((:status . idle)))
          (funcall (pop timers))
          (should (= 3 (length submitted)))
          (should (equal "analyze articles 5-5 of 5 from unread in mail: Triage." (car (car submitted))))
          (should-not efrit-gnus--queue)
          (should (equal "Overall triage." efrit-gnus--closing))
          ;; The closing does not fire while batches remain or on non-idle.
          (efrit-gnus--send-closing '((:status . working)))
          (should-not timers)
          (efrit-gnus--send-closing '((:status . idle)))
          (funcall (pop timers))
          (should (= 4 (length submitted)))
          (should (equal "over the whole selection" (car (car submitted))))
          (should (string-match-p "That was the whole selection. Overall triage." (cdr (car submitted))))
          (should-not efrit-gnus--closing))))))

(ert-deftest test-efrit-gnus-prompts-are-pairs ()
  "Every built-in prompt has a per-batch and an over-everything part."
  (dolist (e efrit-gnus-prompts)
    (should (stringp (car e)))
    (should (stringp (cadr e)))
    (should (stringp (cddr e))))
  (should (assoc "trends, accomplishments, concerns" efrit-gnus-prompts)))

(ert-deftest test-efrit-gnus-submit-builds-turn ()
  "The submission fills the prompt, confirms once, and hands efrit a short
shown line plus the full API text; a busy efrit is a user error."
  (test-gnus--with-articles
      (mapcar (lambda (n) (cons (cons "nnml:mail" n) (test-gnus--raw (format "m%d" n) "hello"))) '(1 2))
    (let (submitted)
      (cl-letf (((symbol-function 'efrit-submit)
                 (lambda (shown api) (setq submitted (list shown api)) t))
                ((symbol-function 'efrit-gnus-ensure-tools) #'ignore)
                ((symbol-function 'efrit-gnus--require) (lambda (&rest _) t))
                ((symbol-function 'y-or-n-p) (lambda (_) t)))
        (let ((efrit-gnus-confirm 'once) (efrit-gnus--confirmed nil))
          (efrit-gnus--submit '(("nnml:mail" . 1) ("nnml:mail" . 2))
                              "Summarize {count} articles of {group}." "group mail" "mail")
          (should efrit-gnus--confirmed)
          (should (equal "analyze 2 articles from group mail: Summarize 2 articles of mail."
                         (car submitted)))
          (should (string-prefix-p "Summarize 2 articles of mail." (cadr submitted)))
          (should (string-match-p "gnus_search and gnus_articles" (cadr submitted)))
          (should (string-match-p "=== Article 1 of 2 ===" (cadr submitted)))
          (should (string-match-p "Subject: m2" (cadr submitted)))
          (efrit-gnus--submit '(("nnml:mail" . 1)) (cdr (assoc "summarize" efrit-gnus-prompts)))
          (should (equal "analyze 1 article from the selection: summarize" (car submitted)))
          ;; The summary prompt is armed for after the turn.
          (should (string-match-p "main threads and themes" efrit-gnus--closing))
          ;; A typed prompt gets a derived summary.
          (efrit-gnus--submit '(("nnml:mail" . 1)) "Which ones mention budgets?")
          (should (string-match-p "over the whole selection as one: Which ones mention budgets\\?" efrit-gnus--closing))))
      (cl-letf (((symbol-function 'efrit-submit) (lambda (&rest _) nil))
                ((symbol-function 'efrit-gnus-ensure-tools) #'ignore)
                ((symbol-function 'efrit-gnus--require) (lambda (&rest _) t)))
        (let ((efrit-gnus-confirm nil))
          (should-error (efrit-gnus--submit '(("nnml:mail" . 1)) "x") :type 'user-error)))
      ;; Default: no confirmation prompt at all.
      (should-not efrit-gnus-confirm))))

(ert-deftest test-efrit-gnus-tools ()
  "gnus_articles takes GROUP#NUMBER references; bad ones are reported; the
tools register as read-only under package efrit-gnus."
  (test-gnus--with-articles
      `((("nnml:mail" . 7) . ,(test-gnus--raw "seven" "body seven")))
    (let ((text (efrit-gnus--tool-articles '(("refs" . ["nnml:mail#7"])))))
      (should (string-match-p "Article: nnml:mail#7" text))
      (should (string-match-p "body seven" text)))
    (should (string-match-p "No valid references" (efrit-gnus--tool-articles '(("refs" . ["nonsense"])))))
    (should (equal '("nnml:mail" . 7) (efrit-gnus--parse-ref "nnml:mail#7")))
    (should (equal '("nnimap+work:a#b/c" . 12) (efrit-gnus--parse-ref "nnimap+work:a#b/c#12")))
    (should-not (efrit-gnus--parse-ref "no-number")))
  (let ((efrit-tool-registry nil))
    (cl-letf (((symbol-function 'efrit-gnus--require) (lambda (&rest _) t)))
      (efrit-gnus-ensure-tools)
      (should (equal '("gnus_articles" "gnus_groups" "gnus_search")
                     (sort (mapcar #'car efrit-tool-registry) #'string<)))
      (should (cl-every (lambda (e) (eq 'read (efrit-registered-tool-class (cdr e)))) efrit-tool-registry))
      (should (equal '("gnus_articles" "gnus_groups" "gnus_search")
                     (sort (efrit-unregister-package-tools 'efrit-gnus) #'string<))))))

(ert-deftest test-efrit-gnus-keys ()
  "The command map is under the prefix in summary and group mode maps."
  (should (keymapp efrit-gnus-map))
  (should (eq 'efrit-gnus-analyze (lookup-key efrit-gnus-map "a")))
  (should (eq 'efrit-gnus-analyze-unread (lookup-key efrit-gnus-map "u")))
  (when efrit-gnus-summary-prefix-key
    (should (eq efrit-gnus-map (lookup-key gnus-summary-mode-map (kbd efrit-gnus-summary-prefix-key))))))


(ert-deftest test-efrit-gnus-expand-links ()
  "Links in the body are offered to `efrit-gnus-expand-link-functions';
the first non-nil result follows the article; a failing expander leaves
a note; at most `efrit-gnus-expand-links-max' are expanded."
  (test-gnus--with-articles
      `((("nnml:mail" . 1)
         . ,(test-gnus--raw "recap" "Notes at https://docs.example/d/AAA. Also https://other.example/x and https://docs.example/d/BBB")))
    (let ((efrit-gnus-expand-link-functions
           (list (lambda (url) (when (string-match-p "docs\\.example/d/AAA" url) "Doc A text"))
                 (lambda (url) (when (string-match-p "docs\\.example/d/BBB" url) (error "no access")))))
          (efrit-gnus-expand-links-max 5))
      (let ((text (efrit-gnus--render "nnml:mail" 1)))
        (should (string-match-p "--- Linked document: https://docs.example/d/AAA ---\nDoc A text" text))
        (should (string-match-p "--- Linked document: https://docs.example/d/BBB ---\n\\[could not fetch: no access\\]" text))
        (should-not (string-match-p "Linked document: https://other.example" text)))
      ;; The link cap counts expansions, not URLs seen.
      (let ((efrit-gnus-expand-links-max 1))
        (let ((text (efrit-gnus--render "nnml:mail" 1)))
          (should (string-match-p "Doc A text" text))
          (should-not (string-match-p "could not fetch" text)))))
    ;; No expanders: nothing appended.
    (let ((efrit-gnus-expand-link-functions nil))
      (should-not (string-match-p "Linked document" (efrit-gnus--render "nnml:mail" 1))))
    (should (equal '("https://a.example/p" "https://b.example/q")
                   (efrit-gnus--article-links "see https://a.example/p, and (https://b.example/q).")))))

(provide 'test-efrit-gnus)
;;; test-efrit-gnus.el ends here
