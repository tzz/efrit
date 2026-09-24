;;; efrit-prompts.el --- A library of two-part prompts, with an editor -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "29.1") (transient "0.4"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Some efrit commands run one question over many items: every unread
;; message in a group, every recap of a quarter.  Such a question has
;; two parts.  The ITEM part is asked of each batch of items ("for
;; each message, one line: topics, decisions, worries").  The SUMMARY
;; part is asked once at the end, over everything ("now three
;; sections: trends, accomplishments, concerns").  This file keeps
;; those prompts, lets you pick one, and lets you edit them.
;;
;; A prompt is a plist: (:name NAME :item ITEM :summary SUMMARY
;; :description DESCRIPTION :builtin BOOL).  Prompts live in
;; `efrit-prompts-builtin' (from code; a package adds its own with
;; `efrit-prompts-define') and in a JSON file under
;; `efrit-data-directory' (yours: edits, new prompts, and the built-in
;; ones you changed, which are saved as overrides).  `efrit-prompts'
;; returns the merged list.
;;
;; Picking: `efrit-prompts-read' is a transient menu: one key per
;; prompt, with the first line of the item part as a hint, the last
;; used one first.  `/' types a one-off question instead.  `e' opens
;; the manager.  Without a display (batch), it falls back to
;; `completing-read'.
;;
;; The manager, `M-x efrit-prompts-manage', is a table: RET edits,
;; `a' adds, `d' deletes (a changed built-in goes back to its default),
;; `c' copies, `r' renames, `s' asks efrit to suggest improvements to
;; the prompt at point, `v' shows it in full.  `m' opens a transient
;; menu with the same actions for the row.
;;
;; The editor is one buffer with two sections, "Per item" and "Over
;; everything", plus the name and description; `C-c C-c' saves,
;; `C-c C-k' abandons, `C-c C-s' asks efrit to suggest a better
;; version of both parts.  The suggestion arrives in a review buffer
;; as a diff against your text; `a' accepts it into the editor, `q'
;; discards it.  The suggestion costs one model call and goes to the
;; model efrit is configured for; nothing else here talks to the
;; network.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'tabulated-list)
(require 'efrit-config)
(require 'efrit-log)
(require 'efrit-api)
(require 'efrit-chat-response)

(declare-function efrit-settings-write-json "efrit-settings")
(declare-function efrit-show-popup "efrit-ui-helpers")
(declare-function efrit-ui-badge "efrit-ui-helpers")
(declare-function transient-quit-one "transient")
(declare-function transient-parse-suffixes "transient")
(declare-function efrit-prompts-chooser "efrit-prompts")
(defvar transient-post-exit-hook)
(defvar efrit-default-model)

(defgroup efrit-prompts nil
  "Named two-part prompts for analyses over many items."
  :group 'efrit
  :prefix "efrit-prompts-")

(defcustom efrit-prompts-file
  (expand-file-name "prompts.json" efrit-data-directory)
  "Where your prompts and edits to the built-in ones are kept."
  :type 'file)

(defcustom efrit-prompts-suggest-model nil
  "Model asked for prompt suggestions; nil means `efrit-default-model'."
  :type '(choice (const nil) string))

(defcustom efrit-prompts-suggest-instructions
  "You improve prompts that an assistant runs over many mail messages or documents. Each prompt has two parts. The ITEM part runs on a batch of items and must ask for a short, specific answer per item. The SUMMARY part runs once afterwards over all the per-item answers and the items, and must ask for one consolidated answer: merged, deduplicated, with names and dates, and with anything unresolved called out. Keep the user's intent and vocabulary. Prefer concrete asks over adjectives. Say what to omit. Keep each part under 120 words. Reply with exactly two blocks, nothing else:\n\n=== ITEM ===\n<item prompt>\n=== SUMMARY ===\n<summary prompt>"
  "The system prompt for `efrit-prompts-suggest'."
  :type 'string)

(defface efrit-prompts-name
  '((t :inherit bold))
  "The prompt's name in listings and headers.")

(defface efrit-prompts-builtin
  '((((background dark)) :foreground "#1b1b1b" :background "#8fa8c8")
    (t :foreground "white" :background "#4a6b95"))
  "Badge of a built-in prompt.")

(defface efrit-prompts-yours
  '((((background dark)) :foreground "#1b1b1b" :background "#8fbc8f")
    (t :foreground "white" :background "#2e8b57"))
  "Badge of a prompt you wrote.")

(defface efrit-prompts-changed
  '((((background dark)) :foreground "#1b1b1b" :background "#e6b422")
    (t :foreground "#1b1b1b" :background "#f0c040"))
  "Badge of a built-in prompt you changed.")

(defface efrit-prompts-section
  '((t :inherit font-lock-keyword-face :weight bold :overline t))
  "Section headings in the editor.")

(defface efrit-prompts-hint
  '((t :inherit shadow))
  "The first line of a prompt shown beside its name in the chooser.")

;;;; The library

(defvar efrit-prompts-builtin nil
  "Prompts defined in code: a list of plists, oldest first.
Use `efrit-prompts-define' to add to it.")

(defvar efrit-prompts--user nil
  "Prompts from `efrit-prompts-file': a list of plists, or nil before loading.")

(defvar efrit-prompts--loaded nil)

(defvar efrit-prompts-last nil
  "Name of the prompt picked most recently; offered first next time.")

(defvar efrit-prompts-change-hook nil
  "Run after the library changed and was saved.")

(defun efrit-prompts-define (name item summary &optional description)
  "Define built-in prompt NAME with ITEM and SUMMARY parts and a DESCRIPTION.
Redefining NAME replaces the earlier definition."
  (unless (and (stringp name) (not (string-empty-p name)))
    (error "efrit-prompts: a prompt needs a name"))
  (setq efrit-prompts-builtin
        (append (cl-remove name efrit-prompts-builtin
                           :key (lambda (p) (plist-get p :name)) :test #'equal)
                (list (list :name name :item item :summary summary
                            :description (or description "") :builtin t))))
  name)

(defun efrit-prompts--plist-from-json (obj)
  "The prompt plist for a hash table OBJ read from the file."
  (list :name (gethash "name" obj)
        :item (gethash "item" obj "")
        :summary (gethash "summary" obj "")
        :description (gethash "description" obj "")))

(defun efrit-prompts--load ()
  "Read `efrit-prompts-file' once.  A missing or broken file is empty."
  (unless efrit-prompts--loaded
    (setq efrit-prompts--loaded t
          efrit-prompts--user
          (when (file-readable-p efrit-prompts-file)
            (condition-case err
                (let ((obj (with-temp-buffer
                             (insert-file-contents efrit-prompts-file)
                             (json-parse-buffer :object-type 'hash-table :array-type 'list))))
                  (mapcar #'efrit-prompts--plist-from-json
                          (and (hash-table-p obj) (gethash "prompts" obj))))
              (error
               (efrit-log 'warn "prompts: cannot read %s: %s" efrit-prompts-file
                          (error-message-string err))
               nil))))))

(defun efrit-prompts--save ()
  "Write the user prompts to `efrit-prompts-file'."
  (require 'efrit-settings)
  (let ((obj (make-hash-table :test 'equal)))
    (puthash "version" 1 obj)
    (puthash "prompts"
             (vconcat (mapcar (lambda (p)
                                (let ((h (make-hash-table :test 'equal)))
                                  (puthash "name" (plist-get p :name) h)
                                  (puthash "item" (plist-get p :item) h)
                                  (puthash "summary" (plist-get p :summary) h)
                                  (puthash "description" (or (plist-get p :description) "") h)
                                  h))
                              efrit-prompts--user))
             obj)
    (efrit-settings-write-json efrit-prompts-file obj)
    (run-hooks 'efrit-prompts-change-hook)))

(defun efrit-prompts-reload ()
  "Forget the loaded user prompts, so the next call reads the file again."
  (interactive)
  (setq efrit-prompts--loaded nil efrit-prompts--user nil))

(defun efrit-prompts ()
  "All prompts: yours, then the built-in ones you have not overridden.
A user prompt with a built-in's name is that built-in, changed; it
carries :builtin t and :changed t."
  (efrit-prompts--load)
  (let ((builtin-names (mapcar (lambda (p) (plist-get p :name)) efrit-prompts-builtin)))
    (append
     (mapcar (lambda (p)
               (if (member (plist-get p :name) builtin-names)
                   (append p '(:builtin t :changed t))
                 p))
             efrit-prompts--user)
     (seq-remove (lambda (p) (efrit-prompts--user-entry (plist-get p :name)))
                 efrit-prompts-builtin))))

(defun efrit-prompts--user-entry (name)
  "The user's prompt plist named NAME, or nil."
  (efrit-prompts--load)
  (seq-find (lambda (p) (equal (plist-get p :name) name)) efrit-prompts--user))

(defun efrit-prompts-get (name)
  "The prompt named NAME (user version first), or nil."
  (seq-find (lambda (p) (equal (plist-get p :name) name)) (efrit-prompts)))

(defun efrit-prompts-names ()
  "The names of all prompts, `efrit-prompts-last' first."
  (let ((names (mapcar (lambda (p) (plist-get p :name)) (efrit-prompts))))
    (if (and efrit-prompts-last (member efrit-prompts-last names))
        (cons efrit-prompts-last (remove efrit-prompts-last names))
      names)))

(defun efrit-prompts-put (name item summary &optional description)
  "Save prompt NAME with ITEM, SUMMARY and DESCRIPTION as yours.
Replaces a user prompt of that name; over a built-in it becomes the
override.  Returns the plist."
  (unless (and (stringp name) (not (string-empty-p (string-trim name))))
    (user-error "efrit-prompts: the prompt needs a name"))
  (when (string-empty-p (string-trim (or item "")))
    (user-error "efrit-prompts: the per-item part is empty"))
  (efrit-prompts--load)
  (let ((entry (list :name name :item item :summary (or summary "")
                     :description (or description ""))))
    (setq efrit-prompts--user
          (append (cl-remove name efrit-prompts--user
                             :key (lambda (p) (plist-get p :name)) :test #'equal)
                  (list entry)))
    (efrit-prompts--save)
    entry))

(defun efrit-prompts-delete (name)
  "Delete your prompt NAME.  A changed built-in returns to its default."
  (efrit-prompts--load)
  (unless (efrit-prompts--user-entry name)
    (user-error "efrit-prompts: %s is built in and unchanged; nothing to delete" name))
  (setq efrit-prompts--user
        (cl-remove name efrit-prompts--user
                   :key (lambda (p) (plist-get p :name)) :test #'equal))
  (efrit-prompts--save))

(defun efrit-prompts-rename (old new)
  "Rename your prompt OLD to NEW."
  (let ((p (or (efrit-prompts--user-entry old)
               (user-error "efrit-prompts: %s is not yours to rename; copy it first" old))))
    (when (efrit-prompts-get new)
      (user-error "efrit-prompts: a prompt named %s exists" new))
    (efrit-prompts-delete old)
    (efrit-prompts-put new (plist-get p :item) (plist-get p :summary) (plist-get p :description))))

(defun efrit-prompts-pair (prompt)
  "PROMPT (a plist, a name, or (ITEM . SUMMARY)) as (ITEM . SUMMARY)."
  (cond
   ((and (consp prompt) (keywordp (car prompt)))
    (cons (plist-get prompt :item) (plist-get prompt :summary)))
   ((consp prompt) prompt)
   ((stringp prompt)
    (if-let* ((p (efrit-prompts-get prompt)))
        (efrit-prompts-pair p)
      (cons prompt nil)))
   (t (error "efrit-prompts: bad prompt %S" prompt))))

(defun efrit-prompts-name-of (item)
  "The name of the prompt whose per-item part is ITEM, or nil."
  (plist-get (seq-find (lambda (p) (equal (plist-get p :item) item)) (efrit-prompts)) :name))

;;;; Choosing one

(defun efrit-prompts--first-line (text width)
  "TEXT's first sentence, on one line, cut to WIDTH characters."
  (let* ((text (replace-regexp-in-string "[ \t\n]+" " " (or text "")))
         (end (string-match "\\. " text))
         (text (if end (substring text 0 end) text)))
    (truncate-string-to-width text width nil nil "…")))

(defvar efrit-prompts--choice nil
  "The chooser's answer: a name, (typed . TEXT), `manage', or `pending'.")
(defvar efrit-prompts--depth nil)
(defvar efrit-prompts--purpose nil)

(defconst efrit-prompts--keys "asdfghjklqwrtyuiopzxcvbnm1234567890"
  "Keys handed to prompts in the chooser, in order.  `e', `/' and `?' are taken.")

(defun efrit-prompts--choose (value)
  "Record VALUE as the chooser's answer and close the menu."
  (setq efrit-prompts--choice value)
  (transient-quit-one))

(defun efrit-prompts--chooser-children (_group)
  "The prompt rows of the chooser, built when it opens.
Names are padded to one column so the hints line up."
  (let* ((names (efrit-prompts-names))
         (keys (append efrit-prompts--keys nil))
         (name-width (apply #'max 8 (mapcar #'length names)))
         (hint-width (max 20 (- (frame-width) name-width 12)))
         (rows nil))
    (dolist (name names)
      (when keys
        (let* ((key (string (pop keys)))
               (p (efrit-prompts-get name))
               (hint (efrit-prompts--first-line (plist-get p :item) hint-width)))
          (push (list key
                      (lambda () (interactive) (efrit-prompts--choose name))
                      :description (concat (propertize (format (format "%%-%ds" name-width) name)
                                                       'face 'efrit-prompts-name)
                                           "  " (propertize hint 'face 'efrit-prompts-hint)))
                rows))))
    (transient-parse-suffixes 'efrit-prompts-chooser (nreverse rows))))

(defun efrit-prompts--chooser-heading ()
  (concat (propertize (or efrit-prompts--purpose "Which prompt?") 'face 'efrit-prompts-name)
          (propertize "   (each runs per batch, then once over everything)" 'face 'shadow)))

(defun efrit-prompts--exit-recursive-edit ()
  (when (and efrit-prompts--depth (= (recursion-depth) efrit-prompts--depth))
    (exit-recursive-edit)))

(defconst efrit-prompts--chooser-definition
  '(transient-define-prefix efrit-prompts-chooser ()
     "Pick a prompt."
     [:description efrit-prompts--chooser-heading
      :class transient-column
      :setup-children efrit-prompts--chooser-children]
     [["Or"
       ("/" "type a question for this run only"
        (lambda () (interactive) (efrit-prompts--choose 'typed)))
       ("e" "edit the prompts (manager)"
        (lambda () (interactive) (efrit-prompts--choose 'manage)))]])
  "The chooser prefix, kept as data so a reload redefines it.")

(defun efrit-prompts--define-chooser ()
  "Define the chooser when transient is available; return non-nil then."
  (when (require 'transient nil t)
    (unless (fboundp 'efrit-prompts-chooser)
      (eval efrit-prompts--chooser-definition t))
    t))

(defun efrit-prompts--read-typed ()
  (let ((text (read-string "Your question (asked per batch, then over everything): ")))
    (when (string-empty-p (string-trim text)) (user-error "Nothing to ask"))
    text))

(defun efrit-prompts--read-with-menu (purpose default)
  "The transient chooser; returns a name, a typed string, or `manage'."
  (setq efrit-prompts--choice 'pending
        efrit-prompts--purpose purpose
        efrit-prompts-last (or default efrit-prompts-last))
  (let ((efrit-prompts--depth (1+ (recursion-depth))))
    (unwind-protect
        (progn
          (add-hook 'transient-post-exit-hook #'efrit-prompts--exit-recursive-edit)
          (run-at-time 0 nil (lambda () (call-interactively #'efrit-prompts-chooser)))
          (condition-case nil (recursive-edit) (quit nil)))
      (remove-hook 'transient-post-exit-hook #'efrit-prompts--exit-recursive-edit)))
  (pcase efrit-prompts--choice
    ('pending (keyboard-quit))
    ('typed (efrit-prompts--read-typed))
    (v v)))

(defun efrit-prompts--read-with-completion (purpose default)
  "The fallback chooser for batch use or without transient."
  (let* ((names (efrit-prompts-names))
         (default (or default (car names)))
         (choice (completing-read
                  (format "%s (name, your own question, or \"edit\"; default %s): "
                          (or purpose "Prompt") default)
                  (cons "edit" names) nil nil nil 'efrit-prompts-history default)))
    (cond ((equal choice "edit") 'manage)
          (t choice))))

(defvar efrit-prompts-history nil)

;;;###autoload
(defun efrit-prompts-read (&optional purpose default)
  "Ask the user for a prompt and return (ITEM . SUMMARY).
PURPOSE is a short phrase for the heading (\"Analyze 113 unread in
recaps\"); DEFAULT a prompt name offered first.  A named prompt is
remembered in `efrit-prompts-last'.  Typing a question returns it with a
nil summary.  `e' opens the manager and asks again when it closes."
  (let ((choice (if (and (display-graphic-p) (not noninteractive)
                         (efrit-prompts--define-chooser))
                    (efrit-prompts--read-with-menu purpose default)
                  (efrit-prompts--read-with-completion purpose default))))
    (cond
     ((eq choice 'manage)
      (efrit-prompts-manage t)
      (efrit-prompts-read purpose default))
     ((efrit-prompts-get choice)
      (setq efrit-prompts-last choice)
      (efrit-prompts-pair (efrit-prompts-get choice)))
     (t (cons choice nil)))))

;;;; Suggestions from the model

(defun efrit-prompts--suggest-request (name item summary purpose)
  "The API request asking for a better ITEM and SUMMARY for prompt NAME."
  `(("model" . ,(or efrit-prompts-suggest-model efrit-default-model))
    ("max_tokens" . 1200)
    ("system" . ,(efrit-api-cacheable-system efrit-prompts-suggest-instructions))
    ("messages" . [(("role" . "user")
                    ("content" . ,(format "Prompt name: %s\n%s\nCurrent ITEM part:\n%s\n\nCurrent SUMMARY part:\n%s\n\nImprove both parts."
                                          name
                                          (if (and purpose (not (string-empty-p purpose)))
                                              (format "What the user wants from it: %s\n" purpose)
                                            "")
                                          item (if (string-empty-p (or summary "")) "(none yet)" summary))))])))

(defun efrit-prompts-parse-suggestion (text)
  "Parse the model's TEXT into (ITEM . SUMMARY), or nil if it is not in the format."
  (when (and (stringp text)
             (string-match "=== ITEM ===[ \t]*\n\\(\\(?:.\\|\n\\)*?\\)\n?=== SUMMARY ===[ \t]*\n\\(\\(?:.\\|\n\\)*\\)\\'" text))
    (cons (string-trim (match-string 1 text))
          (string-trim (match-string 2 text)))))

(defun efrit-prompts--response-text (response)
  (let ((content (efrit-response-content response)) (texts nil))
    (when content
      (dotimes (i (length content))
        (let ((item (aref content i)))
          (when (and (hash-table-p item) (equal (gethash "type" item) "text"))
            (push (gethash "text" item) texts)))))
    (string-join (nreverse texts) "")))

(defun efrit-prompts-suggest (name item summary purpose callback)
  "Ask the model for a better version of prompt NAME's ITEM and SUMMARY.
PURPOSE is optional free text on what the user wants.  CALLBACK gets
\(ITEM . SUMMARY), or a string naming the failure."
  (let ((efrit-api-request-purpose (format "suggesting improvements to the prompt %S" name)))
    (efrit-api-request-async
     (efrit-prompts--suggest-request name item summary purpose)
     (lambda (response)
       (funcall callback
                (cond
                 ((null response) "no response")
                 ((efrit-response-error response)
                  (efrit-error-message (efrit-response-error response)))
                 (t (let ((text (efrit-prompts--response-text response)))
                      (or (efrit-prompts-parse-suggestion text)
                          (format "the model did not answer in the expected format: %s"
                                  (truncate-string-to-width text 200 nil nil "…"))))))))
     (lambda (error-message) (funcall callback (format "%s" error-message))))))

;;;; The editor

(defvar-local efrit-prompts-edit--original nil
  "The prompt plist being edited, or nil for a new one.")
(defvar-local efrit-prompts-edit--on-save nil
  "Function called with the saved plist, or nil.")
(defvar-local efrit-prompts-edit--suggestion nil
  "The last suggestion (ITEM . SUMMARY) not yet accepted, or nil.")

(defconst efrit-prompts-edit--sections
  '((:name "Name" "One short label; it is what you pick in the chooser.")
    (:description "What it is for" "Optional. Shown in the manager.")
    (:item "Per item" "Asked of every batch of items. Ask for a short, specific answer per item.")
    (:summary "Over everything" "Asked once at the end, across all items and their answers. Ask for one consolidated answer."))
  "The editor's sections: (KEY HEADING HELP).")

(defun efrit-prompts-edit--insert (plist)
  "Fill the editor buffer from PLIST."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (section efrit-prompts-edit--sections)
      (pcase-let ((`(,key ,heading ,help) section))
        (insert (propertize (format "%-18s" heading)
                            'face 'efrit-prompts-section
                            'efrit-prompts-section key
                            'read-only t 'rear-nonsticky t)
                (propertize (concat "  " help "\n") 'face 'shadow 'read-only t
                            'rear-nonsticky t))
        (insert (or (plist-get plist key) "") "\n\n")))
    (goto-char (point-min))
    (forward-line 1)
    (set-buffer-modified-p nil)))

(defun efrit-prompts-edit--read ()
  "The editor buffer's content as a plist."
  (save-excursion
    (let ((out nil) (pos (point-min)))
      (while (setq pos (text-property-not-all pos (point-max) 'efrit-prompts-section nil))
        (let* ((key (get-text-property pos 'efrit-prompts-section))
               (start (save-excursion (goto-char pos) (forward-line 1) (point)))
               (next (or (text-property-not-all start (point-max) 'efrit-prompts-section nil)
                         (point-max)))
               (text (string-trim (buffer-substring-no-properties start next))))
          (setq out (plist-put out key text))
          (setq pos next)))
      out)))

(defun efrit-prompts-edit-save ()
  "Save the prompt in this editor and close it."
  (interactive)
  (let* ((p (efrit-prompts-edit--read))
         (old-name (plist-get efrit-prompts-edit--original :name))
         (name (plist-get p :name)))
    (when (and old-name (not (equal old-name name))
               (efrit-prompts--user-entry old-name)
               (not (plist-get efrit-prompts-edit--original :builtin)))
      ;; Renamed a prompt of yours: drop the old name.
      (efrit-prompts-delete old-name))
    (let ((saved (efrit-prompts-put name (plist-get p :item) (plist-get p :summary)
                                    (plist-get p :description)))
          (on-save efrit-prompts-edit--on-save))
      (set-buffer-modified-p nil)
      (message "Saved prompt %s" name)
      (quit-window t)
      (when on-save (funcall on-save saved)))))

(defun efrit-prompts-edit-abandon ()
  "Close the editor without saving."
  (interactive)
  (when (or (not (buffer-modified-p)) (yes-or-no-p "Discard the changes? "))
    (set-buffer-modified-p nil)
    (quit-window t)))

(defun efrit-prompts-edit--replace-section (key text)
  "Put TEXT into section KEY of the editor."
  (let* ((pos (text-property-not-all (point-min) (point-max) 'efrit-prompts-section nil))
         start end)
    (while (and pos (not (eq (get-text-property pos 'efrit-prompts-section) key)))
      (setq pos (text-property-not-all (1+ pos) (point-max) 'efrit-prompts-section nil)))
    (unless pos (error "No section %s" key))
    (setq start (save-excursion (goto-char pos) (forward-line 1) (point)))
    (setq end (or (text-property-not-all start (point-max) 'efrit-prompts-section nil) (point-max)))
    (save-excursion
      (goto-char start)
      (let ((inhibit-read-only t))
        (delete-region start end)
        (insert text "\n\n")))))

(defun efrit-prompts-edit-suggest (&optional purpose)
  "Ask efrit to improve both parts; review the answer as a diff.
With a prefix argument, first ask what the prompt is for (PURPOSE),
which the model is told."
  (interactive (list (and current-prefix-arg
                          (read-string "What do you want this prompt to get you? "))))
  (let* ((p (efrit-prompts-edit--read))
         (editor (current-buffer)))
    (when (string-empty-p (plist-get p :item))
      (user-error "Write a per-item part first; the model improves, it does not invent"))
    (message "efrit: asking for a better %s…" (plist-get p :name))
    (efrit-prompts-suggest
     (plist-get p :name) (plist-get p :item) (plist-get p :summary) purpose
     (lambda (result)
       (if (stringp result)
           (message "efrit-prompts: no suggestion: %s" result)
         (when (buffer-live-p editor)
           (with-current-buffer editor
             (setq efrit-prompts-edit--suggestion result))
           (efrit-prompts--show-suggestion editor p result)))))))

(defun efrit-prompts--diff-text (label old new)
  "A unified diff of OLD against NEW, headed LABEL, or a note when equal."
  (if (equal (string-trim old) (string-trim new))
      (format "--- %s: unchanged\n" label)
    (let ((a (make-temp-file "efrit-prompt-old")) (b (make-temp-file "efrit-prompt-new")))
      (unwind-protect
          (progn
            (with-temp-file a (insert old "\n"))
            (with-temp-file b (insert new "\n"))
            (with-temp-buffer
              (call-process diff-command nil t nil "-u" "--label" (concat label " (yours)")
                            "--label" (concat label " (suggested)") a b)
              (buffer-string)))
        (delete-file a) (delete-file b)))))

(defvar efrit-prompts-review-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'efrit-prompts-review-accept)
    (define-key map (kbd "q") #'quit-window)
    map))

(defvar-local efrit-prompts-review--editor nil)

(defun efrit-prompts--show-suggestion (editor p result)
  "Show RESULT (ITEM . SUMMARY) against P's parts, with EDITOR to accept into."
  (require 'efrit-ui-helpers)
  (require 'diff-mode)
  (let ((win (efrit-show-popup
              "*efrit prompt suggestion*"
              (lambda ()
                (insert (propertize (format "Suggested changes to %s" (plist-get p :name))
                                    'face 'efrit-prompts-name)
                        (propertize "   a accepts them into the editor, q discards\n\n" 'face 'shadow))
                (insert (efrit-prompts--diff-text "Per item" (plist-get p :item) (car result)))
                (insert "\n")
                (insert (efrit-prompts--diff-text "Over everything" (plist-get p :summary) (cdr result)))
                (when (fboundp 'diff-mode) (diff-mode))
                (setq efrit-prompts-review--editor editor)
                (use-local-map (make-composed-keymap efrit-prompts-review-map (current-local-map))))
              #'fundamental-mode)))
    (with-current-buffer (window-buffer win)
      (setq efrit-prompts-review--editor editor))
    win))

(defun efrit-prompts-review-accept ()
  "Put the suggestion into the editor and close this review."
  (interactive)
  (let ((editor efrit-prompts-review--editor))
    (unless (buffer-live-p editor) (user-error "The editor is gone"))
    (quit-window t)
    (with-current-buffer editor
      (pcase-let ((`(,item . ,summary) efrit-prompts-edit--suggestion))
        (efrit-prompts-edit--replace-section :item item)
        (efrit-prompts-edit--replace-section :summary summary)
        (setq efrit-prompts-edit--suggestion nil))
      (message "Suggestion in place; C-c C-c saves it, C-c C-k abandons"))
    (pop-to-buffer editor)))

(defvar efrit-prompts-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'efrit-prompts-edit-save)
    (define-key map (kbd "C-c C-k") #'efrit-prompts-edit-abandon)
    (define-key map (kbd "C-c C-s") #'efrit-prompts-edit-suggest)
    map))

(define-derived-mode efrit-prompts-edit-mode text-mode "Efrit-Prompt"
  "Edit one two-part prompt.
\\{efrit-prompts-edit-mode-map}"
  (setq-local fill-column 78)
  (visual-line-mode 1)
  (setq header-line-format
        (substitute-command-keys
         " \\[efrit-prompts-edit-save] save   \\[efrit-prompts-edit-abandon] abandon   \\[efrit-prompts-edit-suggest] ask efrit for a better version (C-u: say what you want)")))

;;;###autoload
(defun efrit-prompts-edit (&optional name on-save)
  "Edit the prompt NAME in a buffer; with no NAME, a new prompt.
ON-SAVE, when given, is called with the saved plist."
  (interactive (list (completing-read "Edit prompt: " (efrit-prompts-names) nil t)))
  (let* ((p (and name (efrit-prompts-get name)))
         (buf (get-buffer-create (format "*efrit prompt: %s*" (or name "new")))))
    (with-current-buffer buf
      (efrit-prompts-edit-mode)
      (setq efrit-prompts-edit--original p
            efrit-prompts-edit--on-save on-save)
      (efrit-prompts-edit--insert (or p '(:name "" :description "" :item "" :summary ""))))
    (pop-to-buffer buf)))

;;;; The manager

(defvar efrit-prompts-manage-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'efrit-prompts-manage-edit)
    (define-key map (kbd "e") #'efrit-prompts-manage-edit)
    (define-key map (kbd "a") #'efrit-prompts-manage-add)
    (define-key map (kbd "c") #'efrit-prompts-manage-copy)
    (define-key map (kbd "d") #'efrit-prompts-manage-delete)
    (define-key map (kbd "r") #'efrit-prompts-manage-rename)
    (define-key map (kbd "s") #'efrit-prompts-manage-suggest)
    (define-key map (kbd "v") #'efrit-prompts-manage-view)
    (define-key map (kbd "SPC") #'efrit-prompts-manage-view)
    (define-key map (kbd "m") #'efrit-prompts-manage-menu)
    (define-key map (kbd "g") #'efrit-prompts-manage-refresh)
    (define-key map (kbd "?") #'efrit-prompts-manage-help)
    (define-key map (kbd "q") #'efrit-prompts-manage-quit)
    map))

(define-derived-mode efrit-prompts-manage-mode tabulated-list-mode "Efrit-Prompts"
  "Manage efrit's two-part prompts.
\\{efrit-prompts-manage-mode-map}"
  (setq tabulated-list-format
        [("" 9 nil) ("Name" 34 t) ("Per item" 44 nil) ("Over everything" 0 nil)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun efrit-prompts--badge (p)
  (require 'efrit-ui-helpers)
  (cond ((plist-get p :changed) (efrit-ui-badge "changed" 'efrit-prompts-changed))
        ((plist-get p :builtin) (efrit-ui-badge "built-in" 'efrit-prompts-builtin))
        (t (efrit-ui-badge "yours" 'efrit-prompts-yours))))

(defun efrit-prompts--entries ()
  (mapcar (lambda (p)
            (list (plist-get p :name)
                  (vector (efrit-prompts--badge p)
                          (propertize (plist-get p :name) 'face 'efrit-prompts-name)
                          (efrit-prompts--first-line (plist-get p :item) 42)
                          (efrit-prompts--first-line (plist-get p :summary) 60))))
          (efrit-prompts)))

(defun efrit-prompts-manage-refresh ()
  "Re-read the prompts and redraw."
  (interactive)
  (efrit-prompts-reload)
  (let ((at (tabulated-list-get-id)))
    (setq tabulated-list-entries (efrit-prompts--entries))
    (tabulated-list-print t)
    (when at
      (goto-char (point-min))
      (while (and (not (eobp)) (not (equal (tabulated-list-get-id) at)))
        (forward-line 1))
      (when (eobp) (goto-char (point-min))))))

(defun efrit-prompts--refresh-manager ()
  "Redraw the manager buffer if it is live."
  (when-let* ((buf (get-buffer "*efrit prompts*")))
    (with-current-buffer buf (efrit-prompts-manage-refresh))))

(add-hook 'efrit-prompts-change-hook #'efrit-prompts--refresh-manager)

(defun efrit-prompts-manage--name ()
  (or (tabulated-list-get-id) (user-error "No prompt on this line")))

(defun efrit-prompts-manage-edit ()
  "Edit the prompt at point."
  (interactive)
  (efrit-prompts-edit (efrit-prompts-manage--name)))

(defun efrit-prompts-manage-add ()
  "Write a new prompt."
  (interactive)
  (efrit-prompts-edit nil))

(defun efrit-prompts-manage-copy (new)
  "Copy the prompt at point under the name NEW and open it for editing."
  (interactive (list (read-string (format "Copy %s as: " (efrit-prompts-manage--name)))))
  (let ((p (efrit-prompts-get (efrit-prompts-manage--name))))
    (efrit-prompts-put new (plist-get p :item) (plist-get p :summary) (plist-get p :description))
    (efrit-prompts-edit new)))

(defun efrit-prompts-manage-delete ()
  "Delete the prompt at point; a changed built-in goes back to its default."
  (interactive)
  (let* ((name (efrit-prompts-manage--name))
         (p (efrit-prompts-get name)))
    (when (yes-or-no-p (if (plist-get p :changed)
                           (format "Restore the built-in %s? " name)
                         (format "Delete %s? " name)))
      (efrit-prompts-delete name)
      (message "%s %s" name (if (plist-get p :changed) "restored" "deleted")))))

(defun efrit-prompts-manage-rename (new)
  "Rename the prompt at point to NEW."
  (interactive (list (read-string (format "Rename %s to: " (efrit-prompts-manage--name)))))
  (efrit-prompts-rename (efrit-prompts-manage--name) new))

(defun efrit-prompts-manage-view ()
  "Show the prompt at point in full."
  (interactive)
  (require 'efrit-ui-helpers)
  (let ((p (efrit-prompts-get (efrit-prompts-manage--name))))
    (efrit-show-popup
     "*efrit prompt*"
     (lambda ()
       (insert (propertize (plist-get p :name) 'face '(:inherit efrit-prompts-name :height 1.2))
               "  " (efrit-prompts--badge p) "\n")
       (unless (string-empty-p (or (plist-get p :description) ""))
         (insert (propertize (plist-get p :description) 'face 'shadow) "\n"))
       (dolist (section '((:item . "Per item") (:summary . "Over everything")))
         (insert "\n" (propertize (cdr section) 'face 'efrit-prompts-section) "\n"
                 (or (plist-get p (car section)) "") "\n"))
       (let ((fill-column 78)) (fill-region (point-min) (point-max)))))))

(defun efrit-prompts-manage-suggest ()
  "Open the prompt at point in the editor and ask efrit for a better version."
  (interactive)
  (efrit-prompts-edit (efrit-prompts-manage--name))
  (call-interactively #'efrit-prompts-edit-suggest))

(defun efrit-prompts-manage-help ()
  "Describe the keys."
  (interactive)
  (message "RET/e edit  a add  c copy  d delete/restore  r rename  s suggest  v view  m menu  g refresh  q quit"))

(defconst efrit-prompts--row-menu-definition
  '(transient-define-prefix efrit-prompts-row-menu ()
     "Actions on the prompt at point."
     [:description (lambda () (propertize (efrit-prompts-manage--name) 'face 'efrit-prompts-name))
      ["Change"
       ("e" "edit" efrit-prompts-manage-edit)
       ("s" "ask efrit for a better version" efrit-prompts-manage-suggest)
       ("r" "rename" efrit-prompts-manage-rename)
       ("c" "copy as..." efrit-prompts-manage-copy)]
      ["Remove"
       ("d" "delete (or restore a built-in)" efrit-prompts-manage-delete)]
      ["Look"
       ("v" "view in full" efrit-prompts-manage-view)
       ("q" "done" transient-quit-one)]])
  "The row menu, kept as data so a reload redefines it.")

(defun efrit-prompts-manage-menu ()
  "Open the row menu for the prompt at point."
  (interactive)
  (efrit-prompts-manage--name)
  (unless (require 'transient nil t) (user-error "The row menu needs transient"))
  (unless (fboundp 'efrit-prompts-row-menu)
    (eval efrit-prompts--row-menu-definition t))
  (call-interactively 'efrit-prompts-row-menu))

;;;###autoload
(defun efrit-prompts-manage (&optional wait)
  "List and edit efrit's two-part prompts.
With WAIT, block until the manager buffer is closed (the chooser uses
this so it can ask again with the edited list)."
  (interactive)
  (let ((buf (get-buffer-create "*efrit prompts*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'efrit-prompts-manage-mode)
        (efrit-prompts-manage-mode))
      (efrit-prompts-manage-refresh)
      (setq header-line-format
            " RET edit   a add   c copy   d delete   s ask efrit to improve   v view   m menu   ? help   q back"))
    (pop-to-buffer buf)
    (when wait
      (efrit-prompts--wait-for-buffer buf))))

(defvar efrit-prompts-manage--waiting nil
  "Recursion depth of the chooser's wait on the manager, or nil.")

(defun efrit-prompts-manage-quit ()
  "Leave the manager.
Ends the chooser's wait when it is waiting, so it asks again with the
edited list; otherwise buries the buffer as `quit-window' does."
  (interactive)
  (if (and efrit-prompts-manage--waiting
           (= (recursion-depth) efrit-prompts-manage--waiting))
      (progn (quit-window) (exit-recursive-edit))
    (quit-window)))

(defun efrit-prompts--wait-for-buffer (buf)
  "Recursive edit until the manager BUF is quit (`q') or killed.
Not until it leaves the window: the view popup and the editor open
over it and took it out of every window, which ended the wait as soon
as `v' or `e' was pressed (2026-09-24)."
  (let ((efrit-prompts-manage--waiting (1+ (recursion-depth)))
        (on-kill nil))
    (setq on-kill (lambda ()
                    (when (and (eq (current-buffer) buf)
                               (= (recursion-depth) efrit-prompts-manage--waiting))
                      (exit-recursive-edit))))
    (unwind-protect
        (progn
          (add-hook 'kill-buffer-hook on-kill)
          (condition-case nil (recursive-edit) (quit nil)))
      (remove-hook 'kill-buffer-hook on-kill))))

(provide 'efrit-prompts)

;;; efrit-prompts.el ends here
