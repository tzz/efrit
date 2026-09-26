;;; efrit-markdown.el --- In-place Markdown rendering for streamed text -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; The model answers in Markdown.  This renders it in place: the
;; markup characters are deleted and the text that remains gets
;; faces, links and code highlighting as text properties.  No
;; overlays -- they scale badly and fight `field' and read-only
;; text -- and no second buffer.
;;
;; Streaming.  The text arrives in chunks and the renderer runs after
;; each one.  A construct that may still grow (the last line, an open
;; fenced block) is left alone; everything before it is final.  That
;; frontier is the watermark: `efrit-markdown-render' stores it as an
;; offset from the region start in the `efrit-markdown-watermark'
;; property of the first character, and the next call starts there.
;; An offset, not a position, so the value survives text moving above
;; the region.  `efrit-markdown-render' with COMPLETE non-nil renders
;; to the end and releases what was held back.
;;
;; Rendered code is tagged `efrit-markdown-frozen' and skipped by the
;; inline passes, so a `*' inside a code span never becomes emphasis.
;;
;; What is rendered: ATX headers, bold, italic, strikethrough, inline
;; code, fenced code blocks (with the language mode's fontification
;; and a block background), [title](url) links, bare URLs, file
;; references like `lisp/efrit.el:120' (also inside code spans, where
;; the model puts them), bullets and horizontal rules.  Tables and
;; images are not rendered; they stay as text.
;;
;; Every face is also written as `font-lock-face', and the range is
;; marked `fontified', so a font-lock pass over the buffer neither
;; wipes nor redoes the work.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'browse-url)

(defgroup efrit-markdown nil
  "Rendering of the model's Markdown in efrit buffers."
  :group 'efrit
  :prefix "efrit-markdown-")

(defcustom efrit-markdown-enabled t
  "Whether the model's answers are rendered as Markdown in the agent buffer.
nil shows the raw text."
  :type 'boolean)

(defcustom efrit-markdown-fontify-code t
  "Whether fenced code blocks are fontified with their language's major mode."
  :type 'boolean)

(defcustom efrit-markdown-language-modes
  '(("elisp" . emacs-lisp-mode) ("emacs-lisp" . emacs-lisp-mode) ("lisp" . lisp-mode)
    ("el" . emacs-lisp-mode) ("sh" . sh-mode) ("bash" . sh-mode) ("shell" . sh-mode)
    ("zsh" . sh-mode) ("js" . js-mode) ("javascript" . js-mode) ("ts" . typescript-ts-mode)
    ("py" . python-mode) ("python" . python-mode) ("json" . js-json-mode)
    ("yaml" . yaml-mode) ("yml" . yaml-mode) ("c" . c-mode) ("cpp" . c++-mode)
    ("c++" . c++-mode) ("go" . go-mode) ("rust" . rust-mode) ("rs" . rust-mode)
    ("html" . html-mode) ("css" . css-mode) ("sql" . sql-mode) ("diff" . diff-mode)
    ("org" . org-mode) ("makefile" . makefile-mode) ("make" . makefile-mode))
  "Fence language names to major modes.  A name not here tries NAME-mode."
  :type '(alist :key-type string :value-type symbol))

(defcustom efrit-markdown-open-file-function #'find-file-other-window
  "How a file reference is opened; called with the file name."
  :type 'function)

;;;; Faces

(defface efrit-markdown-header
  '((t :inherit bold :height 1.1))
  "Headers.")

(defface efrit-markdown-bold '((t :inherit bold)) "Bold text.")
(defface efrit-markdown-italic '((t :inherit italic)) "Italic text.")
(defface efrit-markdown-strike '((t :strike-through t)) "Struck text.")

(defface efrit-markdown-inline-code
  '((((background dark)) :background "#2a2a2a" :inherit fixed-pitch)
    (t :background "#f0f0f0" :inherit fixed-pitch))
  "Inline code.")

(defface efrit-markdown-code-block
  '((((background dark)) :background "#1e1e1e" :extend t)
    (t :background "#f5f5f5" :extend t))
  "Background of a fenced code block.")

(defface efrit-markdown-code-label
  '((t :inherit shadow :height 0.85))
  "The language label above a code block.")

(defface efrit-markdown-link
  '((t :inherit link))
  "Links and file references.")

(defface efrit-markdown-bullet
  '((t :inherit font-lock-keyword-face))
  "List bullets.")

(defface efrit-markdown-rule
  '((t :inherit shadow :strike-through t))
  "Horizontal rules.")

;;;; Properties and helpers

(defconst efrit-markdown--frozen 'efrit-markdown-frozen
  "Property on rendered ranges the inline passes must not touch.")

(defun efrit-markdown--frozen-p (pos)
  "Non-nil when POS is inside rendered code."
  (get-text-property pos efrit-markdown--frozen))

(defun efrit-markdown--span-frozen-p (start end)
  "Non-nil when any character of START..END is frozen."
  (text-property-not-all start end efrit-markdown--frozen nil))

(defun efrit-markdown--add-face (start end face)
  "Add FACE to START..END, keeping faces already there."
  (add-face-text-property start end face)
  ;; Mirror for font-lock: the whole composed face list
  (let ((pos start))
    (while (< pos end)
      (let ((next (or (next-single-property-change pos 'face nil end) end)))
        (put-text-property pos next 'font-lock-face (get-text-property pos 'face))
        (setq pos next)))))

(defun efrit-markdown--delete (start end)
  "Delete START..END, keeping markers and point sensible."
  (delete-region start end))

;;;; Links

(defvar efrit-markdown-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'efrit-markdown-follow-link)
    (define-key map [mouse-1] #'efrit-markdown-follow-link)
    (define-key map [mouse-2] #'efrit-markdown-follow-link)
    map)
  "Keys on a rendered link.")

(defun efrit-markdown-link-at-point (&optional pos)
  "The link target at POS (default point): a URL, or (FILE LINE COLUMN)."
  (get-text-property (or pos (point)) 'efrit-markdown-target))

(defun efrit-markdown-follow-link (&optional event)
  "Open the link at point (or at EVENT's position)."
  (interactive (list last-nonmenu-event))
  (let* ((pos (if (and event (listp event) (eventp event))
                  (posn-point (event-end event))
                (point)))
         (target (efrit-markdown-link-at-point pos)))
    (pcase target
      ((pred stringp) (browse-url target))
      (`(,file ,line ,column)
       (funcall efrit-markdown-open-file-function file)
       (when line
         (goto-char (point-min))
         (forward-line (1- line))
         (when column (move-to-column column))))
      (_ (user-error "No link here")))))

(defun efrit-markdown--linkify (start end target help)
  "Make START..END a link to TARGET with HELP as its help-echo."
  (efrit-markdown--add-face start end 'efrit-markdown-link)
  (add-text-properties start end
                       (list 'efrit-markdown-target target
                             'keymap efrit-markdown-link-map
                             'mouse-face 'highlight
                             'help-echo help
                             'follow-link t)))

;;;; Passes over a region
;;
;; Each pass walks START..END (markers: the passes delete markup, so
;; the caller keeps END as a marker) and rewrites in place.

(defun efrit-markdown--pass-fences (start end complete)
  "Render complete fenced code blocks in START..END.
Returns the position of an open fence (a block still streaming) or
nil.  With COMPLETE, an open fence at the end is rendered as is."
  (goto-char start)
  (let ((open nil))
    (while (and (not open)
                (re-search-forward "^\\(`\\{3,\\}\\|~\\{3,\\}\\)[ \t]*\\([^ \t\n`]*\\)[^\n]*\n" end t))
      (let* ((fence-start (match-beginning 0))
             (fence (match-string 1))
             (lang (match-string 2))
             (body-start (match-end 0))
             (close-re (format "^%s%s*[ \t]*$" (regexp-quote fence) (regexp-quote (substring fence 0 1))))
             (body-end (and (re-search-forward close-re end t) (match-beginning 0)))
             (close-end (and body-end (min end (1+ (match-end 0))))))
        (cond
         ((and (not body-end) (not complete))
          (setq open fence-start))
         (t
          (unless body-end (setq body-end end close-end end))
          (efrit-markdown--render-code-block fence-start body-start body-end close-end lang)
          (goto-char fence-start)))))
    open))

(defun efrit-markdown--language-mode (lang)
  "The major mode for fence language LANG, or nil."
  (when (and lang (not (string-empty-p lang)))
    (let ((mode (or (cdr (assoc (downcase lang) efrit-markdown-language-modes))
                    (intern-soft (concat (downcase lang) "-mode")))))
      (and mode (fboundp mode) mode))))

(defun efrit-markdown--fontify-string (text mode)
  "TEXT with the faces MODE gives it, as `face' properties."
  (condition-case nil
      (with-temp-buffer
        (insert text)
        (delay-mode-hooks (funcall mode))
        (font-lock-ensure)
        ;; font-lock leaves the faces under `face'; keep them
        (buffer-string))
    (error text)))

(defun efrit-markdown--render-code-block (fence-start body-start body-end close-end lang)
  "Turn the fenced block at FENCE-START into a highlighted block.
The fence lines are deleted; BODY-START..BODY-END stays, fontified per
LANG, on a block background, with the language as a small label."
  (let* ((body (buffer-substring-no-properties body-start body-end))
         (mode (and efrit-markdown-fontify-code (efrit-markdown--language-mode lang)))
         (styled (if mode (efrit-markdown--fontify-string body mode) body))
         (props (text-properties-at fence-start)))
    ;; Replace fence + body + close with label + styled body
    (delete-region fence-start close-end)
    (goto-char fence-start)
    (let ((start (point)))
      (insert (propertize (concat (if (and lang (not (string-empty-p lang))) lang "code") "\n")
                          'face 'efrit-markdown-code-label))
      (insert styled)
      (unless (bolp) (insert "\n"))
      (let ((end (point)))
        ;; The fragment's own properties (type, id, read-only...) so the
        ;; block stays part of the message
        (let ((keep (cl-loop for (k v) on props by #'cddr
                             unless (memq k '(face font-lock-face fontified efrit-markdown-frozen))
                             append (list k v))))
          (when keep (add-text-properties start end keep)))
        (efrit-markdown--add-face start end 'efrit-markdown-code-block)
        (add-text-properties start end (list efrit-markdown--frozen t 'fontified t))
        (goto-char end)))))

(defun efrit-markdown--pass-regexp (start end regexp function)
  "Call FUNCTION at every match of REGEXP in START..END outside frozen text.
FUNCTION runs with the match data set and may delete text; it must
leave point where scanning continues."
  (goto-char start)
  (while (re-search-forward regexp end t)
    (if (efrit-markdown--span-frozen-p (match-beginning 0) (match-end 0))
        (goto-char (match-end 0))
      (funcall function))))

(defun efrit-markdown--pass-headers (start end)
  "ATX headers: the hashes go, the line gets the header face."
  (efrit-markdown--pass-regexp
   start end "^\\(#\\{1,6\\}\\)[ \t]+\\([^\n]*\\)$"
   (lambda ()
     (let ((text-start (match-beginning 2)) (text-end (match-end 2)))
       (efrit-markdown--add-face text-start text-end 'efrit-markdown-header)
       (delete-region (match-beginning 1) text-start)))))

(defun efrit-markdown--pass-rules (start end)
  "Horizontal rules become a thin line."
  (efrit-markdown--pass-regexp
   start end "^\\(?:-\\{3,\\}\\|\\*\\{3,\\}\\|_\\{3,\\}\\)[ \t]*$"
   (lambda ()
     (let ((s (match-beginning 0)) (e (match-end 0)))
       (delete-region s e)
       (efrit-markdown--insert-like s (make-string 40 ?\s))
       (efrit-markdown--add-face s (point) 'efrit-markdown-rule)))))

(defun efrit-markdown--insert-like (pos text)
  "Insert TEXT at POS carrying the non-face properties of the char at POS.
So a bullet or a label stays part of the message it is in (its
`efrit-id', read-only, field): a bare insert broke the message into
runs and readers of the buffer got a tail (2026-09-25)."
  (goto-char pos)
  (let ((props (cl-loop for (k v) on (text-properties-at pos) by #'cddr
                        unless (memq k '(face font-lock-face fontified efrit-markdown-frozen
                                             efrit-markdown-target keymap mouse-face help-echo))
                        append (list k v))))
    (insert (if props (apply #'propertize text props) text))))

(defun efrit-markdown--pass-bullets (start end)
  "List bullets get a face; `- ' becomes `• '."
  (efrit-markdown--pass-regexp
   start end "^\\([ \t]*\\)\\([-*+]\\|[0-9]+\\.\\)[ \t]+"
   (lambda ()
     (let ((s (match-beginning 2)) (e (match-end 2)))
       (when (memq (char-after s) '(?- ?* ?+))
         (goto-char s) (delete-char 1)
         (efrit-markdown--insert-like s "•") (setq e (point)))
       (efrit-markdown--add-face s e 'efrit-markdown-bullet)
       (goto-char (match-end 0))))))

(defun efrit-markdown--pass-inline-code (start end)
  "Code spans: backticks go, the span is frozen with the code face.
File references inside are linked first, since the model puts nearly
every citation in backticks."
  (efrit-markdown--pass-regexp
   start end "\\(`+\\)\\([^`\n]+?\\)\\1"
   (lambda ()
     (let* ((open-s (match-beginning 1)) (open-e (match-end 1))
            (text-e (match-end 2))
            (close-e (match-end 0)))
       (delete-region text-e close-e)
       (delete-region open-s open-e)
       (let ((s open-s) (e (- text-e (- open-e open-s))))
         (efrit-markdown--add-face s e 'efrit-markdown-inline-code)
         (efrit-markdown--link-file-refs s e)
         (put-text-property s e efrit-markdown--frozen t)
         (goto-char e))))))

(defun efrit-markdown--pass-emphasis (start end)
  "Bold, italic, strikethrough.  Delimiters go, the text gets the face.
Runs until nothing changes, so nested `***x***' resolves."
  (let ((changed t))
    (while changed
      (setq changed nil)
      (dolist (spec '(("\\*\\*\\([^*\n]+?\\)\\*\\*" . efrit-markdown-bold)
                      ("__\\([^_\n]+?\\)__" . efrit-markdown-bold)
                      ("~~\\([^~\n]+?\\)~~" . efrit-markdown-strike)
                      ("\\(?:^\\|[^*[:alnum:]]\\)\\(\\*\\([^*\n]+?\\)\\*\\)" . efrit-markdown-italic)
                      ("\\(?:^\\|[^_[:alnum:]]\\)\\(_\\([^_\n]+?\\)_\\)\\(?:[^_[:alnum:]]\\|$\\)" . efrit-markdown-italic)))
        (efrit-markdown--pass-regexp
         start end (car spec)
         (lambda ()
           (let* ((italic (eq (cdr spec) 'efrit-markdown-italic))
                  (whole-s (if italic (match-beginning 1) (match-beginning 0)))
                  (whole-e (if italic (match-end 1) (match-end 0)))
                  (text-s (if italic (match-beginning 2) (match-beginning 1)))
                  (text-e (if italic (match-end 2) (match-end 1)))
                  (before (- text-s whole-s))
                  (after (- whole-e text-e)))
             (delete-region text-e whole-e)
             (delete-region whole-s text-s)
             (efrit-markdown--add-face whole-s (- text-e before) (cdr spec))
             (goto-char (- whole-e before after))
             (setq changed t))))))))

(defun efrit-markdown--pass-links (start end)
  "[title](url): the title stays as a link to the url."
  (efrit-markdown--pass-regexp
   start end "\\[\\([^][\n]+\\)\\](\\(<[^>\n]+>\\|[^()[:space:]\n]*\\(?:([^()\n]*)[^()[:space:]\n]*\\)*\\))"
   (lambda ()
     (let* ((s (match-beginning 0)) (e (match-end 0))
            (title (match-string 1))
            (url (string-trim (match-string 2) "<" ">")))
       (delete-region s e)
       (efrit-markdown--insert-like s title)
       (efrit-markdown--linkify s (point) url url)))))

(defun efrit-markdown--pass-urls (start end)
  "Bare URLs become links."
  (efrit-markdown--pass-regexp
   start end browse-url-button-regexp
   (lambda ()
     (let ((s (match-beginning 0)) (e (match-end 0)))
       (unless (get-text-property s 'efrit-markdown-target)
         (let ((url (string-trim-right (buffer-substring-no-properties s e) "[.,;:!?)]+")))
           (efrit-markdown--linkify s (+ s (length url)) url url)))
       (goto-char e)))))

(defconst efrit-markdown--file-ref-body
  "\\(\\(?:~\\|\\.\\{1,2\\}\\)?/?\\(?:[[:alnum:]_.-]+/\\)*[[:alnum:]_-]+\\.[[:alnum:]]+\\)\\(?::\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\|#L\\([0-9]+\\)\\)?"
  "A path with an extension, optionally `:LINE', `:LINE:COL' or `#LNN'.
Group 1 path, 2 line, 3 column, 4 line (the #L form).")

(defconst efrit-markdown--file-ref-regexp
  (concat "\\(?:^\\|[^[:alnum:]_/.-]\\)" efrit-markdown--file-ref-body)
  "A file reference in prose: at a line start or after a non-path character.")

(defconst efrit-markdown--file-ref-in-code-regexp
  (concat "\\(?:\\=\\|^\\|[^[:alnum:]_/.-]\\)" efrit-markdown--file-ref-body)
  "A file reference inside a code span: also at the span's very start.")

(defun efrit-markdown--file-ref-file (path)
  "PATH as an existing file, relative to the project when relative, or nil."
  (let ((root (or (and (fboundp 'efrit-tool--get-project-root)
                       (ignore-errors (funcall 'efrit-tool--get-project-root)))
                  default-directory)))
    (let ((file (expand-file-name path root)))
      (and (file-exists-p file) file))))

(defun efrit-markdown--link-file-refs (start end)
  "Link the file references in START..END that name existing files."
  (goto-char start)
  (while (re-search-forward efrit-markdown--file-ref-in-code-regexp end t)
    (let* ((s (match-beginning 1)) (e (match-end 0))
           (path (match-string 1))
           (line (or (match-string 2) (match-string 4)))
           (col (match-string 3))
           (file (efrit-markdown--file-ref-file path)))
      (when (and file (not (get-text-property s 'efrit-markdown-target)))
        (efrit-markdown--linkify s e
                                 (list file (and line (string-to-number line))
                                       (and col (string-to-number col)))
                                 (concat "open " (abbreviate-file-name file))))
      (goto-char e))))

(defun efrit-markdown--pass-file-refs (start end)
  "File references in prose (outside code, which the code pass did)."
  (goto-char start)
  (while (re-search-forward efrit-markdown--file-ref-regexp end t)
    (let ((s (match-beginning 1)) (e (match-end 0)))
      (unless (or (efrit-markdown--span-frozen-p s e)
                  (get-text-property s 'efrit-markdown-target))
        (let* ((path (match-string 1))
               (line (or (match-string 2) (match-string 4)))
               (col (match-string 3))
               (file (efrit-markdown--file-ref-file path)))
          (when file
            (efrit-markdown--linkify s e
                                     (list file (and line (string-to-number line))
                                           (and col (string-to-number col)))
                                     (concat "open " (abbreviate-file-name file))))))
      (goto-char e))))

;;;; Entry point

(defun efrit-markdown--watermark (start)
  "The rendered frontier of the region at START, as a position."
  (let ((offset (get-text-property start 'efrit-markdown-watermark)))
    (if offset (min (+ start offset) (point-max)) start)))

(defun efrit-markdown--set-watermark (start pos)
  "Record POS as the frontier of the region at START."
  (when (< start (point-max))
    (put-text-property start (1+ start) 'efrit-markdown-watermark (- pos start))))

(defun efrit-markdown--safe-frontier (end complete open-fence)
  "Where rendering may stop being final: the start of the last line,
or of an open fence, whichever is earlier.  END when COMPLETE."
  (if complete
      end
    (let ((last-line (save-excursion (goto-char end) (line-beginning-position))))
      (min last-line (or open-fence end)))))

(defun efrit-markdown-render (start end &optional complete)
  "Render the Markdown in START..END in place; return the new END.
Text before the stored watermark is left as it is.  Constructs that
may still grow are held back unless COMPLETE.  START and END may be
markers.  Safe to call on every streamed chunk."
  (when efrit-markdown-enabled
    (let* ((start (if (markerp start) (marker-position start) start))
           (end-marker (if (markerp end) end (copy-marker end t)))
           (from (max start (efrit-markdown--watermark start)))
           (inhibit-read-only t)
           (inhibit-modification-hooks t)
           (buffer-undo-list t))
      (save-excursion
        (save-restriction
          (widen)
          (let* ((open-fence (efrit-markdown--pass-fences from end-marker complete))
                 (frontier (efrit-markdown--safe-frontier end-marker complete open-fence))
                 (limit (copy-marker frontier t)))
            (when (< from limit)
              (efrit-markdown--pass-headers from limit)
              (efrit-markdown--pass-rules from limit)
              (efrit-markdown--pass-inline-code from limit)
              (efrit-markdown--pass-links from limit)
              (efrit-markdown--pass-emphasis from limit)
              (efrit-markdown--pass-bullets from limit)
              (efrit-markdown--pass-urls from limit)
              (efrit-markdown--pass-file-refs from limit)
              (put-text-property from limit 'fontified t))
            (efrit-markdown--set-watermark start (marker-position limit))
            (set-marker limit nil))))
      (prog1 (marker-position end-marker)
        (unless (markerp end) (set-marker end-marker nil))))))

(defun efrit-markdown-render-string (text)
  "TEXT rendered as Markdown, as a propertized string."
  (with-temp-buffer
    (insert text)
    (efrit-markdown-render (point-min) (point-max) t)
    (buffer-string)))

(provide 'efrit-markdown)

;;; efrit-markdown.el ends here
