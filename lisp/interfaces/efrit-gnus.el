;;; efrit-gnus.el --- Ask efrit about mail and news in Gnus -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, ai, mail, gnus

;;; Commentary:

;; Gnus shows the mail; efrit reads it for you.  This file does two
;; things, for any Gnus backend (nnimap, nnml, nntp, nngmail...):
;;
;; 1. Commands that hand efrit a set of articles and a question.  In a
;;    summary buffer, `efrit-gnus-analyze' takes the articles you mean
;;    (the active region's lines, else the process-marked ones, else
;;    the one at point), a prompt chosen by name from
;;    `efrit-gnus-prompts' or typed, and starts an efrit turn with the
;;    articles inlined as text.  `efrit-gnus-analyze-thread' takes the
;;    thread at point, `efrit-gnus-analyze-unread' every unread article
;;    of a group, `efrit-gnus-analyze-group' the newest N of a group,
;;    and `efrit-gnus-analyze-search' the results of a Gnus search
;;    (`gnus-search', so the query syntax is your server's).  The turn
;;    shows in the efrit buffer as one line ("analyze 12 articles from
;;    unread in INBOX: triage"); the model receives the prompt and the
;;    text.
;;
;; 2. Tools the model can call on its own in any turn, registered with
;;    `efrit-register-tool': `gnus_groups' (subscribed groups with
;;    unread counts), `gnus_search' (a `gnus-search' query on a
;;    server; returns a listing with article references), and
;;    `gnus_articles' (full text of articles by reference).  With these
;;    efrit can follow an analysis up -- "find her reply", "what else
;;    came from that list this week" -- without leaving the
;;    conversation.  They are read-only.  Nothing here marks, moves or
;;    edits an article.
;;
;; An article reference is GROUP#NUMBER, the full Gnus group name and
;; the article number in it; that is what the listings show and what
;; `gnus_articles' takes.
;;
;; Articles are rendered through Gnus's own fetch (`gnus-request-article')
;; and MIME layer: headers, the text/plain part, else text/html through
;; shr, attachment names and sizes.  Each article is capped at
;; `efrit-gnus-article-max-chars' and the whole payload at
;; `efrit-gnus-max-chars'; what does not fit is listed by header with a
;; note, so the model knows what it did not read.
;;
;; Reading an article in Gnus marks it read.  These commands do not go
;; through the summary's article display, so they leave marks alone.
;;
;; Links: a backend that can fetch a linked document (a Google Doc a
;; meeting recap points to, say) adds a function to
;; `efrit-gnus-expand-link-functions'; the document's text then follows
;; the article in everything the model sees, with no extra command.
;;
;; Privacy: article text goes to the model efrit is configured for, as
;; anything you ask efrit does.  The first analysis in an Emacs session
;; says how many articles and characters it is about to send and asks;
;; see `efrit-gnus-confirm'.
;;
;; Keys: `efrit-gnus-map' is bound under `efrit-gnus-summary-prefix-key'
;; (default `C-c g') in every Gnus summary and group buffer, and the analyze
;; commands are on the Gnus summary menu.  Backends may bind the map
;; elsewhere too (nngmail puts it under its `L A').

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gnus)
(require 'gnus-sum)
(require 'gnus-group)
(require 'gnus-int)
(require 'gnus-range)
(require 'mm-decode)
(require 'mm-view)
(require 'nnheader)
(require 'message)

(declare-function efrit-register-tool "efrit-tool-registry")
(declare-function efrit-unregister-package-tools "efrit-tool-registry")
(declare-function efrit-submit "efrit-agent-input")
(declare-function efrit-reload "efrit-reload")
(declare-function nngmail-reload "nngmail-reload")
(declare-function shr-insert-document "shr")
(declare-function libxml-parse-html-region "xml.c")
(declare-function gnus-search-server-to-engine "gnus-search")
(declare-function gnus-search-run-search "gnus-search")
(defvar shr-width) (defvar shr-use-fonts) (defvar shr-inhibit-images)
(defvar gnus-summary-article-menu)

(defgroup efrit-gnus nil
  "Ask efrit about articles in Gnus."
  :group 'efrit
  :prefix "efrit-gnus-")

(defcustom efrit-gnus-prompts
  '(("summarize" .
     "Summarize these messages. Lead with what matters most; group by thread or topic; name who wants what from whom. Keep it short.")
    ("action items" .
     "List every action item, request or deadline in these messages, each with who asked, who must act, and by when. Say which are addressed to me. Omit anything already done.")
    ("draft replies" .
     "For each message that expects a reply from me, draft one in my voice: brief, plain, specific. Show the draft under the message's subject. Say which messages need no reply and why.")
    ("who and what" .
     "Who are the people in these messages and what does each one want or report? One line per person.")
    ("timeline" .
     "Put the events, decisions and commitments in these messages on a dated timeline, oldest first, with the message each comes from.")
    ("triage" .
     "Sort these messages into: needs my reply today, needs my reply this week, read only, ignore. One line each with the reason. Be decisive.")
    ("extract" .
     "Extract every concrete fact from these messages -- names, dates, amounts, links, identifiers, decisions -- as a table with the message it came from.")
    ("tone check" .
     "Read these messages for tone: is anyone upset, blocked, or waiting on me? Quote the line that tells you and say what a good response would be."))
  "Named prompts offered by the analysis commands: (NAME . PROMPT).
Pick one by NAME at the prompt, or type your own text.  A prompt may
mention the group, the count and the query with the placeholders
{group}, {count} and {query}."
  :type '(alist :key-type string :value-type string))

(defcustom efrit-gnus-default-prompt "summarize"
  "Name of the prompt in `efrit-gnus-prompts' offered first."
  :type 'string)

(defcustom efrit-gnus-article-max-chars 12000
  "Characters kept of one article's body; the rest is cut and marked."
  :type 'integer)

(defcustom efrit-gnus-max-chars 150000
  "Characters of article text sent in one analysis, all articles together.
Roughly 40k tokens.  Articles past this are listed by header only, with
a note, so the model knows what it did not read."
  :type 'integer)

(defcustom efrit-gnus-tool-max-articles 25
  "Most articles the `gnus_articles' tool returns in full at once."
  :type 'integer)

(defcustom efrit-gnus-search-limit 50
  "Most results an analysis or the `gnus_search' tool takes from a search."
  :type 'integer)

(defcustom efrit-gnus-confirm 'once
  "Whether to confirm before sending article text to the model.
`once' asks the first time in an Emacs session, `always' every time,
nil never.  The prompt states how many articles and characters go out."
  :type '(choice (const once) (const always) (const nil)))

(defcustom efrit-gnus-html-renderer 'shr
  "How text/html bodies are turned into text: `shr', or nil to skip them."
  :type '(choice (const shr) (const nil)))

(defcustom efrit-gnus-headers '("From" "To" "Cc" "Newsgroups" "Date" "Subject" "Message-ID")
  "Headers included above each article body, in this order."
  :type '(repeat string))

(defcustom efrit-gnus-expand-link-functions nil
  "Functions that turn a URL found in an article into text for the model.
Each is called with one argument, the URL, and returns a string (the
linked document's text, already trimmed to a sensible size) or nil to
pass.  The first non-nil result is appended to the article under a
\"Linked document: URL\" heading.  Errors are caught: a failing
expander contributes a one-line note instead, so the analysis still
runs.  Backends add expanders for documents they can fetch; nngmail
adds one for Google Docs.  Only URLs matching
`efrit-gnus-expand-link-regexp' are offered."
  :type 'hook)

(defcustom efrit-gnus-expand-link-regexp "https?://[^][<>\"'()\\ \t\n]+"
  "URLs in an article body that are offered to `efrit-gnus-expand-link-functions'."
  :type 'regexp)

(defcustom efrit-gnus-expand-links-max 5
  "Most links expanded per article; the rest are listed as URLs only."
  :type 'integer)

(defcustom efrit-gnus-summary-prefix-key "C-c g"
  "Prefix under which `efrit-gnus-map' is bound in Gnus summary and group buffers.
nil installs no binding.  Set before this file loads.  Not `C-c e',
which efrit's own global map uses for `efrit-do'."
  :type '(choice (const nil) key-sequence))

(defvar efrit-gnus--confirmed nil
  "Non-nil once the user agreed to send article text this session.")

;;;; Loading efrit's own pieces

(defun efrit-gnus--require (feature)
  "Load efrit FEATURE, loading efrit itself first.
efrit's libraries live in subdirectories that efrit.el adds to
`load-path' when it loads; with a deferred `use-package' that has not
happened yet when a summary command runs."
  (unless (featurep 'efrit)
    (unless (locate-library "efrit")
      (user-error "efrit-gnus: efrit is not on your load-path"))
    (require 'efrit))
  (require feature))

;;;; Article references

(defun efrit-gnus--ref (group number)
  "The reference string for article NUMBER of GROUP."
  (format "%s#%d" group number))

(defun efrit-gnus--parse-ref (ref)
  "Parse REF (\"GROUP#NUMBER\") into (GROUP . NUMBER), or nil."
  (when (and (stringp ref) (string-match "\\`\\(.+\\)#\\([0-9]+\\)\\'" ref))
    (cons (match-string 1 ref) (string-to-number (match-string 2 ref)))))

;;;; Rendering an article as text

(defun efrit-gnus--decoded-part (handle)
  "The text of leaf HANDLE, transfer- and charset-decoded, as a multibyte string."
  (let* ((charset (or (mail-content-type-get (mm-handle-type handle) 'charset) "utf-8"))
         (coding (or (mm-charset-to-coding-system charset nil t) 'utf-8))
         (bytes (mm-get-part handle)))
    (if (multibyte-string-p bytes) bytes (decode-coding-string bytes coding))))

(defun efrit-gnus--part-text (handle)
  "Text of leaf MIME HANDLE, or nil if it is not text we can show."
  (let ((type (mm-handle-media-type handle)))
    (cond
     ((equal type "text/plain")
      (efrit-gnus--decoded-part handle))
     ((and (equal type "text/html") (eq efrit-gnus-html-renderer 'shr)
           (fboundp 'libxml-parse-html-region))
      (require 'shr)
      (with-temp-buffer
        (insert (efrit-gnus--decoded-part handle))
        (let ((dom (libxml-parse-html-region (point-min) (point-max))))
          (erase-buffer)
          (when dom
            (let ((shr-width 80) (shr-use-fonts nil) (shr-inhibit-images t))
              (shr-insert-document dom))))
        (buffer-substring-no-properties (point-min) (point-max))))
     ((equal type "text/html")
      ;; No libxml: strip tags rather than drop the body.
      (with-temp-buffer
        (insert (efrit-gnus--decoded-part handle))
        (goto-char (point-min))
        (while (re-search-forward "<[^>]+>" nil t) (replace-match " " t t))
        (replace-regexp-in-string "[ \t]+" " " (buffer-string) t t)))
     (t nil))))

(defun efrit-gnus--walk-parts (handle texts attachments)
  "Collect text parts and attachment names from HANDLE into TEXTS and ATTACHMENTS.
Both are cons cells whose car is a list, pushed onto in order.  For a
multipart/alternative only the best part is kept (plain over html)."
  (cond
   ((and (listp handle) (stringp (car handle)))
    (let ((type (car handle))
          (children (cdr handle)))
      (if (equal type "multipart/alternative")
          (let* ((leaves (seq-remove (lambda (h) (and (listp h) (stringp (car h)))) children))
                 (best (or (seq-find (lambda (h) (equal (mm-handle-media-type h) "text/plain")) leaves)
                           (seq-find (lambda (h) (equal (mm-handle-media-type h) "text/html")) leaves)
                           (car children))))
            (when best (efrit-gnus--walk-parts best texts attachments)))
        (dolist (child children)
          (efrit-gnus--walk-parts child texts attachments)))))
   ((and (listp handle) (bufferp (car handle)))
    (let* ((disposition (mm-handle-disposition handle))
           (filename (or (mail-content-type-get disposition 'filename)
                         (mail-content-type-get (mm-handle-type handle) 'name)))
           (text (and (not (equal (car disposition) "attachment"))
                      (efrit-gnus--part-text handle))))
      (if text
          (push text (car texts))
        (push (format "%s (%s, %s)"
                      (or filename "unnamed")
                      (mm-handle-media-type handle)
                      (file-size-human-readable
                       (with-current-buffer (mm-handle-buffer handle) (buffer-size))))
              (car attachments)))))))

(defun efrit-gnus--clip (text limit)
  "TEXT cut to LIMIT characters with a visible marker when cut."
  (if (<= (length text) limit)
      text
    (concat (substring text 0 limit)
            (format "\n[... %d more characters not shown ...]" (- (length text) limit)))))

(defun efrit-gnus--fetch-raw (group number)
  "The raw article NUMBER of GROUP as a unibyte string, or nil.
Through `gnus-request-article', so every backend works and no summary
mark changes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (when (ignore-errors (gnus-request-article number group (current-buffer)))
      (buffer-string))))

(defun efrit-gnus--article-links (body)
  "Distinct URLs in BODY, in order of first appearance, trailing punctuation dropped."
  (let (out (start 0))
    (while (string-match efrit-gnus-expand-link-regexp body start)
      (let ((url (string-trim-right (match-string 0 body) "[.,;:!?)]+")))
        (unless (member url out) (push url out)))
      (setq start (match-end 0)))
    (nreverse out)))

(defun efrit-gnus--expand-link (url)
  "Text for URL from `efrit-gnus-expand-link-functions', or nil.
An expander that signals contributes a note naming the error."
  (catch 'done
    (dolist (fn efrit-gnus-expand-link-functions)
      (condition-case err
          (when-let* ((text (funcall fn url)))
            (throw 'done text))
        (error
         (throw 'done (format "[could not fetch: %s]" (error-message-string err))))))
    nil))

(defun efrit-gnus--expand-links (body)
  "Linked documents of BODY as text blocks to append, or nil.
At most `efrit-gnus-expand-links-max' links are fetched."
  (when efrit-gnus-expand-link-functions
    (let ((n 0) blocks)
      (dolist (url (efrit-gnus--article-links body))
        (when (< n efrit-gnus-expand-links-max)
          (when-let* ((text (efrit-gnus--expand-link url)))
            (cl-incf n)
            (push (format "--- Linked document: %s ---\n%s" url text) blocks))))
      (and blocks (string-join (nreverse blocks) "\n\n")))))

(defun efrit-gnus--render (group number)
  "Article NUMBER of GROUP as plain text: headers, body, attachment list.
Documents linked from the body that an `efrit-gnus-expand-link-functions'
entry can fetch follow the body.  Returns nil when the article cannot
be fetched."
  (when-let* ((raw (efrit-gnus--fetch-raw group number)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert raw)
      (goto-char (point-min))
      (while (search-forward "\r\n" nil t) (replace-match "\n" t t))
      (let* ((headers
              (save-restriction
                (message-narrow-to-head)
                (delq nil
                      (mapcar (lambda (h)
                                (when-let* ((v (message-fetch-field h)))
                                  (cons h (replace-regexp-in-string
                                           "[\n\r\t]+" " " (mail-decode-encoded-word-string v) t t))))
                              efrit-gnus-headers))))
             (handles (mm-dissect-buffer t))
             (texts (list nil))
             (attachments (list nil)))
        (unwind-protect
            (progn
              (when handles (efrit-gnus--walk-parts handles texts attachments))
              (let* ((body (string-join (nreverse (car texts)) "\n\n"))
                     (body (replace-regexp-in-string "\n\\{3,\\}" "\n\n" (string-trim body) t t)))
                (concat
                 (mapconcat (lambda (h) (format "%s: %s" (car h) (cdr h))) headers "\n")
                 (format "\nArticle: %s" (efrit-gnus--ref group number))
                 (and (car attachments)
                      (format "\nAttachments: %s" (string-join (nreverse (car attachments)) "; ")))
                 "\n\n"
                 (if (string-empty-p body)
                     "[no text body]"
                   (efrit-gnus--clip body efrit-gnus-article-max-chars))
                 (when-let* ((linked (efrit-gnus--expand-links body)))
                   (concat "\n\n" linked)))))
          (when handles (mm-destroy-parts handles)))))))

(defun efrit-gnus--header-line (group number)
  "One line describing article NUMBER of GROUP for listings, from its head."
  (let ((head (with-temp-buffer
                (when (ignore-errors (gnus-request-head number group))
                  (with-current-buffer nntp-server-buffer
                    (goto-char (point-min))
                    (nnheader-parse-head t))))))
    (if head
        (format "- %s | %s | %s | %s"
                (efrit-gnus--ref group number)
                (mail-header-date head) (mail-header-from head) (mail-header-subject head))
      (format "- %s | (header unavailable)" (efrit-gnus--ref group number)))))

(defun efrit-gnus--progress (done total)
  "Show rendering progress DONE of TOTAL in the echo area, throttled."
  (when (and (> total 3) (or (= done total) (zerop (% done 5))))
    (message "efrit-gnus: rendering %d/%d" done total)))

(defun efrit-gnus--payload (refs)
  "The articles REFS ((GROUP . NUMBER) ...) rendered, within `efrit-gnus-max-chars'.
Returns (TEXT SENT-COUNT TOTAL-CHARS).  Articles that do not fit are
listed by header with a note."
  (let ((budget efrit-gnus-max-chars)
        (parts nil) (sent 0) (total 0) (skipped nil) (i 0)
        (n (length refs)))
    (pcase-dolist (`(,group . ,number) refs)
      (cl-incf i)
      (efrit-gnus--progress i n)
      (if skipped
          (push (cons group number) skipped)
        (let ((text (or (efrit-gnus--render group number)
                        (format "%s\n\n[article text unavailable]"
                                (efrit-gnus--header-line group number)))))
          (if (> (length text) budget)
              (push (cons group number) skipped)
            (cl-decf budget (length text))
            (cl-incf total (length text))
            (cl-incf sent)
            (push (format "=== Article %d of %d ===\n%s" i n text) parts)))))
    (list (concat (string-join (nreverse parts) "\n\n")
                  (when skipped
                    (setq skipped (nreverse skipped))
                    (format "\n\n=== %d more article%s not included (over the %d character limit); headers only ===\n%s"
                            (length skipped) (if (= 1 (length skipped)) "" "s")
                            efrit-gnus-max-chars
                            (mapconcat (lambda (r) (efrit-gnus--header-line (car r) (cdr r)))
                                       skipped "\n"))))
          sent total)))

;;;; Prompts and submission

(defvar efrit-gnus-prompt-history nil)
(defvar efrit-gnus-query-history nil)

(defun efrit-gnus--read-prompt (&optional default)
  "Ask for a prompt: a name from `efrit-gnus-prompts' or free text."
  (let* ((names (mapcar #'car efrit-gnus-prompts))
         (default (or default efrit-gnus-default-prompt))
         (choice (completing-read
                  (format "Ask efrit (name or your own question, default %s): " default)
                  names nil nil nil 'efrit-gnus-prompt-history default)))
    (or (cdr (assoc choice efrit-gnus-prompts)) choice)))

(defun efrit-gnus--fill (prompt group count query)
  "PROMPT with {group}, {count} and {query} filled in."
  (let ((s prompt))
    (setq s (replace-regexp-in-string "{group}" (or group "") s t t))
    (setq s (replace-regexp-in-string "{count}" (number-to-string count) s t t))
    (setq s (replace-regexp-in-string "{query}" (or query "") s t t))
    s))

(defun efrit-gnus--prompt-name (prompt)
  "The short name of PROMPT for the conversation line: its key, or its start."
  (or (car (rassoc prompt efrit-gnus-prompts))
      (truncate-string-to-width prompt 40 nil nil "…")))

(defun efrit-gnus--confirm (count chars)
  "Ask, per `efrit-gnus-confirm', before sending COUNT articles of CHARS."
  (or (null efrit-gnus-confirm)
      (and (eq efrit-gnus-confirm 'once) efrit-gnus--confirmed)
      (when (y-or-n-p (format "Send %d article%s (%d characters) to efrit's model? "
                              count (if (= count 1) "" "s") chars))
        (setq efrit-gnus--confirmed t))))

(defun efrit-gnus--submit (refs prompt &optional where group query)
  "Render REFS, confirm, and start an efrit turn with PROMPT.
WHERE names the selection in the conversation line (\"unread in INBOX\");
GROUP and QUERY fill the prompt's placeholders."
  (efrit-gnus--require 'efrit-agent-input)
  (efrit-gnus-ensure-tools)
  (pcase-let* ((prompt (efrit-gnus--fill prompt group (length refs) query))
               (`(,text ,sent ,chars) (efrit-gnus--payload refs)))
    (unless (efrit-gnus--confirm sent chars)
      (user-error "efrit-gnus: not sent"))
    (let ((shown (format "analyze %d article%s from %s: %s"
                         (length refs) (if (= 1 (length refs)) "" "s")
                         (or where "the selection") (efrit-gnus--prompt-name prompt)))
          (api (concat
                prompt
                "\n\nThe articles follow, oldest first, each with its reference (GROUP#NUMBER). "
                "Refer to them by subject and sender, not by number. "
                "If you need other articles, the rest of a conversation, or the full text of one "
                "that was cut, the gnus_search and gnus_articles tools give it to you.\n\n"
                text)))
      (unless (efrit-submit shown api)
        (user-error "efrit is busy with another turn; try again when it is idle")))))

;;;; Choosing articles

(defun efrit-gnus--summary-refs ()
  "References of the articles the user means in this summary, oldest first.
The active region's lines, else the process-marked articles, else the
article at point."
  (unless (derived-mode-p 'gnus-summary-mode)
    (user-error "efrit-gnus: not in a summary buffer"))
  (let ((numbers (if (use-region-p)
                     (save-excursion
                       (let ((end (region-end)) out)
                         (goto-char (region-beginning))
                         (while (< (point) end)
                           (when-let* ((a (gnus-summary-article-number)))
                             (push a out))
                           (forward-line 1))
                         (nreverse out)))
                   (gnus-summary-work-articles nil))))
    (when (null numbers) (user-error "efrit-gnus: no article here"))
    (mapcar (lambda (n) (cons gnus-newsgroup-name n)) (sort (copy-sequence numbers) #'<))))

(defun efrit-gnus--group-for-command ()
  "The group a command applies to: the summary's, the group line's, or asked."
  (or (and (derived-mode-p 'gnus-summary-mode) gnus-newsgroup-name)
      (and (derived-mode-p 'gnus-group-mode) (gnus-group-group-name))
      (gnus-group-completing-read "Group: ")))

(defun efrit-gnus--group-article-numbers (group)
  "All article numbers of GROUP, ascending, from its active range less `unexist'."
  (let ((active (or (gnus-active group) (gnus-activate-group group))))
    (unless active (user-error "efrit-gnus: cannot activate %s" group))
    (range-list-difference
     (range-uncompress active)
     (cdr (assq 'unexist (gnus-info-marks (gnus-get-info group)))))))

;;;; Commands

;;;###autoload
(defun efrit-gnus-analyze (prompt)
  "Ask efrit PROMPT about the selected articles in this summary.
The selection is the active region's articles, else the process-marked
ones, else the article at point.  Interactively PROMPT is picked from
`efrit-gnus-prompts' by name or typed."
  (interactive (list (efrit-gnus--read-prompt)))
  (let ((refs (efrit-gnus--summary-refs)))
    (efrit-gnus--submit refs prompt (format "group %s" (gnus-group-real-name gnus-newsgroup-name))
                        (gnus-group-real-name gnus-newsgroup-name))))

;;;###autoload
(defun efrit-gnus-analyze-thread (prompt)
  "Ask efrit PROMPT about the thread of the article at point, as shown in this summary."
  (interactive (list (efrit-gnus--read-prompt)))
  (unless (derived-mode-p 'gnus-summary-mode)
    (user-error "efrit-gnus: not in a summary buffer"))
  (let ((numbers (gnus-summary-articles-in-thread)))
    (when (null numbers) (user-error "efrit-gnus: no thread here"))
    (efrit-gnus--submit (mapcar (lambda (n) (cons gnus-newsgroup-name n)) (sort numbers #'<))
                        prompt "this thread" (gnus-group-real-name gnus-newsgroup-name))))

;;;###autoload
(defun efrit-gnus-analyze-unread (group prompt)
  "Ask efrit PROMPT about every unread article of GROUP.
Interactively GROUP is the current one in a summary or on a group line.
Nothing is marked read by this.  Defaults to the `triage' prompt."
  (interactive (list (efrit-gnus--group-for-command) (efrit-gnus--read-prompt "triage")))
  (let ((numbers (gnus-list-of-unread-articles group)))
    (when (null numbers) (user-error "efrit-gnus: nothing unread in %s" (gnus-group-real-name group)))
    (efrit-gnus--submit (mapcar (lambda (n) (cons group n)) numbers)
                        prompt (format "unread in %s" (gnus-group-real-name group))
                        (gnus-group-real-name group))))

;;;###autoload
(defun efrit-gnus-analyze-group (group count prompt)
  "Ask efrit PROMPT about the newest COUNT articles of GROUP.
Interactively COUNT is the prefix argument or is asked for."
  (interactive
   (let* ((g (efrit-gnus--group-for-command))
          (n (if current-prefix-arg (prefix-numeric-value current-prefix-arg)
               (read-number (format "Newest how many of %s? " (gnus-group-real-name g)) 20))))
     (list g n (efrit-gnus--read-prompt))))
  (let* ((numbers (efrit-gnus--group-article-numbers group))
         (newest (last numbers count)))
    (when (null newest) (user-error "efrit-gnus: %s is empty" (gnus-group-real-name group)))
    (efrit-gnus--submit (mapcar (lambda (n) (cons group n)) newest)
                        prompt (format "newest %d of %s" (length newest) (gnus-group-real-name group))
                        (gnus-group-real-name group))))

;;;###autoload
(defun efrit-gnus-analyze-search (server query prompt)
  "Ask efrit PROMPT about the results of a Gnus search for QUERY on SERVER.
QUERY is in the syntax of SERVER's search engine (`gnus-search'); it is
passed raw.  At most `efrit-gnus-search-limit' results are used."
  (interactive
   (list (gnus-completing-read "Server" (mapcar #'car gnus-server-alist) t)
         (read-string "Search query: " nil 'efrit-gnus-query-history)
         (efrit-gnus--read-prompt)))
  (let ((refs (efrit-gnus--search server query efrit-gnus-search-limit)))
    (when (null refs) (user-error "efrit-gnus: no results for %S" query))
    (efrit-gnus--submit refs prompt (format "search %S on %s" query server) nil query)))

(defun efrit-gnus--search (server query limit)
  "Run QUERY on SERVER through `gnus-search'; return up to LIMIT (GROUP . NUMBER)."
  (require 'gnus-search)
  (let* ((engine (or (gnus-search-server-to-engine server)
                     (user-error "efrit-gnus: no search engine for server %s" server)))
         (results (gnus-search-run-search engine server `((query . ,query) (raw . t)) nil))
         (refs nil))
    (dotimes (i (length results))
      (let ((r (aref results i)))
        ;; A result vector is [GROUP NUMBER SCORE]; the group comes back
        ;; as the backend's short name or a full one.
        (let ((group (aref r 0)) (number (aref r 1)))
          (push (cons (if (gnus-group-prefixed-p group) group
                        (gnus-group-full-name group (gnus-server-to-method server)))
                      number)
                refs))))
    (seq-take (nreverse refs) limit)))

;;;; Tools for the model

(defun efrit-gnus--tool-groups (_input)
  "gnus_groups: subscribed groups with their unread counts."
  (let (rows)
    (dolist (info (cdr gnus-newsrc-alist))
      (when (<= (gnus-info-level info) gnus-level-subscribed)
        (let* ((group (gnus-info-group info))
               (unread (gnus-group-unread group)))
          (push (format "- %s: %s unread" group (if (numberp unread) unread "?")) rows))))
    (if rows
        (concat "Subscribed Gnus groups (full names, use them in gnus_search and article references):\n"
                (string-join (nreverse rows) "\n"))
      "No subscribed groups (is Gnus running?).")))

(defun efrit-gnus--tool-search (input)
  "gnus_search: a search on a Gnus server; a listing with references."
  (let* ((server (or (alist-get "server" input nil nil #'equal)
                     (car (car gnus-server-alist))))
         (query (alist-get "query" input nil nil #'equal))
         (limit (min 100 (or (alist-get "limit" input nil nil #'equal) 20)))
         (refs (efrit-gnus--search server query limit)))
    (if (null refs)
        (format "No articles match %S on %s." query server)
      (concat (format "%d article%s for %S on %s. Use gnus_articles with the references for the text.\n"
                      (length refs) (if (= 1 (length refs)) "" "s") query server)
              (mapconcat (lambda (r) (efrit-gnus--header-line (car r) (cdr r))) refs "\n")))))

(defun efrit-gnus--tool-articles (input)
  "gnus_articles: full text of the articles with the given references."
  (let* ((raw (alist-get "refs" input nil nil #'equal))
         (raw (if (vectorp raw) (append raw nil) raw))
         (refs (delq nil (mapcar #'efrit-gnus--parse-ref (seq-take raw efrit-gnus-tool-max-articles)))))
    (if (null refs)
        "No valid references; a reference is GROUP#NUMBER as shown by gnus_search."
      (car (efrit-gnus--payload refs)))))

(defconst efrit-gnus--tools
  `(("gnus_groups"
     "The user's subscribed Gnus mail and news groups with unread counts. Read-only. Group names from here go into gnus_search and article references."
     (("type" . "object") ("properties" . ()))
     ,#'efrit-gnus--tool-groups)
    ("gnus_search"
     "Search the user's mail or news through Gnus on a server (IMAP SEARCH, Gmail, notmuch... whatever the server's engine is; the query is passed raw). Returns a listing with article references (GROUP#NUMBER), dates, senders and subjects. Read-only. Follow up with gnus_articles for the text."
     (("type" . "object")
      ("properties" . (("query" . (("type" . "string") ("description" . "Search query in the server's syntax")))
                       ("server" . (("type" . "string") ("description" . "Gnus server name; default the first configured")))
                       ("limit" . (("type" . "integer") ("description" . "Most results, default 20, max 100")))))
      ("required" . ["query"]))
     ,#'efrit-gnus--tool-search)
    ("gnus_articles"
     "Full text of Gnus articles by reference (GROUP#NUMBER, from gnus_search or from the conversation): headers, body as text, attachment names. Read-only; does not mark anything read. Long bodies are cut with a marker."
     (("type" . "object")
      ("properties" . (("refs" . (("type" . "array") ("items" . (("type" . "string"))) ("description" . "Article references GROUP#NUMBER")))))
      ("required" . ["refs"]))
     ,#'efrit-gnus--tool-articles))
  "The tools offered to efrit: (NAME DESCRIPTION INPUT-SCHEMA FUNCTION).")

;;;###autoload
(defun efrit-gnus-ensure-tools ()
  "Register the Gnus tools with efrit.  Idempotent; the commands call it."
  (interactive)
  (efrit-gnus--require 'efrit-tool-registry)
  (pcase-dolist (`(,name ,description ,schema ,fn) efrit-gnus--tools)
    (efrit-register-tool name :description description :input-schema schema
                         :function fn :class 'read :package 'efrit-gnus))
  (when (called-interactively-p 'any)
    (message "efrit-gnus: %d tools registered" (length efrit-gnus--tools))))

(defun efrit-gnus-remove-tools ()
  "Withdraw the Gnus tools from efrit."
  (interactive)
  (when (featurep 'efrit-tool-registry)
    (efrit-unregister-package-tools 'efrit-gnus)))

;;;; Reloading

;;;###autoload
(defun efrit-gnus-reload (&optional verbose)
  "Reload efrit from source, then any nngmail, then re-register the tools.
For working on efrit and a Gnus backend at once.  With VERBOSE list
each file as it loads."
  (interactive "P")
  (let ((t0 (float-time)) (efrit-count 0) (other-count 0))
    (efrit-gnus--require 'efrit-reload)
    (setq efrit-count (or (efrit-reload verbose) 0))
    (when (and (featurep 'nngmail) (locate-library "nngmail-reload"))
      (require 'nngmail-reload)
      (setq other-count (or (funcall 'nngmail-reload verbose) 0)))
    (funcall 'efrit-gnus-ensure-tools)
    (funcall 'efrit-gnus--install-keys)
    (message "efrit-gnus: reloaded %d efrit and %d nngmail libraries in %.1fs; %d tools registered"
             efrit-count other-count (- (float-time) t0) (length efrit-gnus--tools))))

;;;; Keys and menu

(defvar efrit-gnus-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'efrit-gnus-analyze)
    (define-key map "t" #'efrit-gnus-analyze-thread)
    (define-key map "u" #'efrit-gnus-analyze-unread)
    (define-key map "g" #'efrit-gnus-analyze-group)
    (define-key map "s" #'efrit-gnus-analyze-search)
    map)
  "Keymap of the efrit commands for Gnus.")

(defun efrit-gnus--install-keys ()
  "Bind `efrit-gnus-map' under `efrit-gnus-summary-prefix-key' in Gnus summaries.
Called at load and again by the reloaders (the map is a `defvar' that a
reload rebuilds)."
  (when efrit-gnus-summary-prefix-key
    (define-key gnus-summary-mode-map (kbd efrit-gnus-summary-prefix-key) efrit-gnus-map)
    (define-key gnus-group-mode-map (kbd efrit-gnus-summary-prefix-key) efrit-gnus-map)))

(efrit-gnus--install-keys)

(defconst efrit-gnus--menu
  '("Ask efrit"
    ["About the selection..." efrit-gnus-analyze t]
    ["About this thread..." efrit-gnus-analyze-thread t]
    ["About all unread here..." efrit-gnus-analyze-unread t]
    ["About the newest N here..." efrit-gnus-analyze-group t]
    ["About a search..." efrit-gnus-analyze-search t])
  "The submenu added to Gnus's Article menu.")

(defun efrit-gnus--install-menu ()
  "Add the \"Ask efrit\" submenu to the summary's Article menu.
Gnus builds that menu lazily, in `gnus-summary-make-menu-bar' when the
first summary opens; adding to the mode map before then made a second
\"Article\" menu.  So this runs from `gnus-summary-menu-hook', which
Gnus calls once the menus exist."
  (when (boundp 'gnus-summary-article-menu)
    (easy-menu-add-item gnus-summary-article-menu nil efrit-gnus--menu)))

(add-hook 'gnus-summary-menu-hook #'efrit-gnus--install-menu)
;; A summary already open (this file loaded late, or reloaded) has its
;; menu built; add to it now.
(efrit-gnus--install-menu)

(provide 'efrit-gnus)

;;; efrit-gnus.el ends here
