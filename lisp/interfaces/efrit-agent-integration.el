;;; efrit-agent-integration.el --- Hook integrations for efrit-agent -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.12.12
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; The agent buffer's subscriptions to the event bus: tool rows,
;; streamed text, thinking indicator, questions, TODOs, session
;; lifecycle, review rows.  See "Wiring" below.

;;; Code:

(require 'cl-lib)
(require 'efrit-agent-core)
(require 'efrit-agent-render)
(require 'efrit-agent-tools)
(require 'efrit-agent-input)
(require 'efrit-todo)
(require 'efrit-events)

;; TODO struct accessors and state now come from efrit-todo.el
;; Backward-compatible aliases (efrit-do-todo-item-*, efrit-do--current-todos) are provided there.
;; Declare the aliased variable to silence byte-compiler warnings
(defvar efrit-do--current-todos)

;;; Wiring
;;
;; Everything the agent buffer shows comes in through `efrit-events'
;; (`efrit-subscribe').  The loop engines, the progress layer, the
;; session and the TODO store publish at the source; nothing here
;; advises a function.  The subscribers below are installed at load,
;; once, and are idempotent, so an `efrit-reload' re-running this file
;; changes nothing (efrit-subscribe adds a function at most once).
;;
;; Event vocabulary this file consumes (see efrit-events.el):
;;
;;   session-start    :session-id :command          (efrit-do path)
;;   session-end      :session-id :success
;;   status           :session-id :status           (REPL path)
;;   thinking-start   :session-id :label
;;   thinking-stop    :session-id
;;   text-delta       :session-id :text
;;   text-end         :session-id
;;   tool-start       :session-id :tool :input [:tool-id]
;;   tool-result      :session-id :tool :result :success [:tool-id] [:elapsed]
;;   message          :session-id :text :kind
;;   error            :session-id :message [:buffer]
;;   question         :session-id :question :options
;;   question-answered :session-id :response
;;   todos-changed    :todos
;;   review-start / review-verdict / review-skipped

(declare-function efrit-markdown--make-image "efrit-markdown" (file width &optional exact))
(defvar efrit-markdown-image-max-width)

(defmacro efrit-agent--in-agent-buffer (event &rest body)
  "Run BODY in the agent buffer of EVENT's session; otherwise do nothing.
The buffer is the one of the event's :session-id (`efrit-agent-buffer-for');
an event without a session id goes to the default buffer."
  (declare (indent 1))
  `(let ((buffer (efrit-agent-buffer-for (alist-get :session-id ,event))))
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         ,@body))))

;;; TODOs

(defun efrit-agent--convert-todo-item (todo)
  "Convert an efrit-do-todo-item struct TODO to plist format for display.
Uses proper accessor functions to avoid fragility when struct changes."
  (when (vectorp todo)
    (let* ((id (efrit-do-todo-item-id todo))
           (content (efrit-do-todo-item-content todo))
           (status (efrit-do-todo-item-status todo))
           ;; Convert efrit-do status to agent status
           (agent-status (pcase status
                           ('todo 'pending)
                           ('in-progress 'in_progress)
                           ('completed 'completed)
                           (_ status))))
      (list :id id
            :content content
            :status agent-status))))

(defun efrit-agent-sync-todos (&optional session-id)
  "Sync TODOs from `efrit-do--current-todos' to SESSION-ID's agent buffer."
  (when (and (bound-and-true-p efrit-do--current-todos)
             (efrit-agent-buffer-for session-id))
    (let ((converted-todos
           (mapcar #'efrit-agent--convert-todo-item
                   efrit-do--current-todos)))
      (efrit-agent-update-todos converted-todos (efrit-agent-buffer-for session-id)))))

(defun efrit-agent--on-todos-changed (event)
  "Subscriber: the TODO store changed."
  (efrit-agent-sync-todos (alist-get :session-id event)))

;;; Tool rows
;;
;; A row is opened on tool-start and closed on tool-result.  The REPL
;; engine sends the API's tool_use id in :tool-id, which pairs the two
;; exactly.  The efrit-do path (via efrit-progress) has no id, so
;; there the most recent open row for the same tool name is closed,
;; as before.

(defun efrit-agent--on-tool-start (event)
  (efrit-agent--in-agent-buffer event
    (efrit-agent--init-pending-tools)
    (let* ((tool-name (alist-get :tool event))
           (row (efrit-agent--add-tool-call tool-name (alist-get :input event)))
           (key (or (alist-get :tool-id event) tool-name)))
      (puthash key
               (cons (list :id row :start-time (current-time))
                     (gethash key efrit-agent--pending-tools))
               efrit-agent--pending-tools))))

(defun efrit-agent--on-tool-result (event)
  (efrit-agent--in-agent-buffer event
    (efrit-agent--init-pending-tools)
    (let* ((key (or (alist-get :tool-id event) (alist-get :tool event)))
           (pending (gethash key efrit-agent--pending-tools)))
      (when pending
        (let* ((entry (car pending))
               (elapsed (or (alist-get :elapsed event)
                            (when-let* ((start (plist-get entry :start-time)))
                              (float-time (time-subtract (current-time) start))))))
          (efrit-agent--update-tool-result (plist-get entry :id)
                                           (alist-get :result event)
                                           (alist-get :success event)
                                           elapsed)
          (puthash key (cdr pending) efrit-agent--pending-tools))))))

;;; Text, thinking, messages, errors, status

(defun efrit-agent--on-text-delta (event)
  (efrit-agent--in-agent-buffer event
    (efrit-agent--add-claude-message (alist-get :text event))))

(defun efrit-agent--on-text-end (event)
  (efrit-agent--in-agent-buffer event
    (efrit-agent--stream-end-message)))

(defun efrit-agent--on-thinking-start (event)
  (efrit-agent--in-agent-buffer event
    ;; The spinner timer drives both the in-buffer line and the
    ;; mode-line glyph; `efrit-agent--show-thinking' only inserts
    ;; the line.  Without the timer both stand still.
    (efrit-agent--spinner-start (alist-get :label event))
    (efrit-agent--show-thinking (alist-get :label event))))

(defun efrit-agent--on-thinking-stop (event)
  (efrit-agent--in-agent-buffer event
    (efrit-agent--hide-thinking)))

(defun efrit-agent--on-message (event)
  "Subscriber: a progress message (the efrit-do path).
Claude messages are rendered inline; errors with the error face."
  (efrit-agent--in-agent-buffer event
    (let ((message (alist-get :text event)))
      (pcase (alist-get :kind event)
        ('claude (efrit-agent--add-claude-message (or message "")))
        ('error
         (efrit-agent--append-to-conversation
          (concat (propertize (format "%s Error: " (efrit-agent--char 'error-icon))
                              'face 'efrit-agent-error)
                  (propertize (or message "") 'face 'efrit-agent-error)
                  "\n\n")
          (list 'efrit-type 'error-message
                'efrit-id (format "err-%d" (cl-incf efrit-agent--message-counter)))))
        (_ (efrit-agent--add-claude-message (or message "")))))))

(defun efrit-agent--on-error (event)
  "Subscriber: the REPL loop failed a turn; show it in the transcript.
The publisher may name the buffer (:buffer); else the session's."
  (let ((buffer (or (let ((b (alist-get :buffer event))) (and (buffer-live-p b) b))
                    (efrit-agent-buffer-for (alist-get :session-id event)))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (efrit-agent--add-error-message (alist-get :message event))))))

(defvar efrit-agent-media-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'efrit-agent-media-toggle)
    (define-key map (kbd "TAB") #'efrit-agent-media-toggle)
    (define-key map [mouse-1] #'efrit-agent-media-toggle)
    (define-key map (kbd "o") #'efrit-agent-media-open)
    (define-key map (kbd "c") #'efrit-agent-media-copy-to-project)
    (define-key map (kbd "m") #'efrit-agent-media-move-to-project)
    (define-key map (kbd "w") #'efrit-agent-media-copy-path)
    map)
  "Keys on a generated image or video in the transcript.")

(defcustom efrit-agent-media-open-by-default nil
  "When non-nil, a generated image shows in full under its line at once.
Otherwise the line carries a button; RET, TAB or mouse-1 opens it."
  :type 'boolean
  :group 'efrit-agent)

(defun efrit-agent--media-file-at-point ()
  "The generated file under point, or a `user-error'."
  (or (get-text-property (point) 'efrit-media-file)
      (user-error "No generated image or video here")))

(declare-function efrit-media-open "efrit-media" (file))
(declare-function efrit-media-copy-to-project "efrit-media" (file))
(declare-function efrit-media-move-to-project "efrit-media" (file))

(defun efrit-agent-media-open (&optional event)
  "Open the generated file at point (or under the mouse EVENT) in its own window."
  (interactive (list last-nonmenu-event))
  (when (and event (mouse-event-p event))
    (goto-char (posn-point (event-end event))))
  (require 'efrit-media)
  (efrit-media-open (efrit-agent--media-file-at-point)))

(defun efrit-agent-media-copy-to-project ()
  "Copy the generated file at point into the project's media/ directory."
  (interactive)
  (require 'efrit-media)
  (efrit-media-copy-to-project (efrit-agent--media-file-at-point)))

(defun efrit-agent-media-move-to-project ()
  "Move the generated file at point into the project's media/ directory."
  (interactive)
  (require 'efrit-media)
  (efrit-media-move-to-project (efrit-agent--media-file-at-point)))

(defun efrit-agent-media-copy-path ()
  "Put the generated file's path at point in the kill ring."
  (interactive)
  (let ((file (efrit-agent--media-file-at-point)))
    (kill-new file)
    (message "Copied %s" (abbreviate-file-name file))))

(defun efrit-agent--media-image (file)
  "FILE as an image for the transcript, or nil (a video, no display, no decoder).
The note arrives from a process sentinel, where the selected frame can
be the terminal one: ask about the frame that shows this buffer, else
any graphic frame (2026-11-10: the first image rendered as its name)."
  (let ((frame (or (when-let* ((w (get-buffer-window (current-buffer) t))) (window-frame w))
                   (seq-find #'display-graphic-p (frame-list)))))
    (and frame (display-graphic-p frame)
         (require 'efrit-markdown nil t)
         (efrit-markdown--make-image file efrit-markdown-image-max-width))))

(defun efrit-agent--media-line (file kind prompt open)
  "The transcript lines for FILE of KIND made from PROMPT: a button line,
then the image when OPEN and it is one.  All of it carries the media
keymap and the file."
  (let* ((image (and open (eq kind 'image) (efrit-agent--media-image file)))
         (name (file-name-nondirectory file))
         (help (concat (or prompt "") (if prompt "\n" "")
                       "RET/TAB/mouse-1: show or hide · o: open in a window · c: copy to the project · m: move · w: copy the path"))
         (button (concat "  " (efrit-agent--char (if image 'expand-expanded 'expand-collapsed))
                         " " (symbol-name kind) ": "
                         (propertize name 'face 'link)
                         (if (eq kind 'video) (propertize "  (o opens the player)" 'face 'shadow) "")
                         "\n")))
    (propertize
     (concat button
             (when image
               (concat "    " (propertize name 'display image) "\n")))
     'help-echo help
     'efrit-media-file file 'efrit-media-kind kind 'efrit-media-prompt prompt
     'efrit-media-open (and image t)
     'keymap efrit-agent-media-map 'mouse-face 'highlight)))

(defun efrit-agent--media-bounds (pos)
  "(BEG . END) of the media note that contains POS."
  (let ((file (get-text-property pos 'efrit-media-file)))
    (cons (if (and (> pos (point-min)) (equal (get-text-property (1- pos) 'efrit-media-file) file))
              (previous-single-property-change pos 'efrit-media-file)
            pos)
          (or (next-single-property-change pos 'efrit-media-file) (point-max)))))

(defun efrit-agent-media-toggle (&optional event)
  "Show or hide the generated image at point (or under the mouse EVENT).
A video has nothing to unfold: `o' opens it."
  (interactive (list last-nonmenu-event))
  (when (and event (mouse-event-p event))
    (goto-char (posn-point (event-end event))))
  (let* ((file (efrit-agent--media-file-at-point))
         (kind (get-text-property (point) 'efrit-media-kind))
         (prompt (get-text-property (point) 'efrit-media-prompt))
         (open (get-text-property (point) 'efrit-media-open))
         (bounds (efrit-agent--media-bounds (point)))
         (type (get-text-property (car bounds) 'efrit-type))
         (key (get-text-property (car bounds) 'efrit-note-key)))
    (if (eq kind 'video)
        (efrit-agent-media-open)
      (efrit-agent--with-render
        (let ((inhibit-read-only t))
          (goto-char (car bounds))
          (delete-region (car bounds) (cdr bounds))
          (insert (propertize (efrit-agent--media-line file kind prompt (not open))
                              'efrit-type type 'efrit-note-key key))))
      (goto-char (car bounds))
      (forward-char 2))))

(defun efrit-agent--on-note (event)
  "Subscriber: a one-line note from the sandbox, the limits prompt, or review.
A note with a `:replaces' key rewrites the last note that carried the
same key (the sandbox's \"asks to …\" line becomes the outcome line).
A `media' note (a generated image or video) draws the image and gives
the line keys to open, copy or move the file."
  (efrit-agent--in-agent-buffer event
    (let* ((key (alist-get :replaces event))
           (media (eq (alist-get :kind event) 'media))
           (line (if media
                     (efrit-agent--media-line (alist-get :media-file event) (alist-get :media-kind event)
                                              (alist-get :media-prompt event)
                                              efrit-agent-media-open-by-default)
                   (concat (propertize (concat "  " (alist-get :text event)) 'face (alist-get :face event)) "\n")))
           (props (list 'efrit-type (intern (format "%s-note" (alist-get :kind event)))
                        'efrit-note-key key))
           (old (and key (efrit-agent--find-note key))))
      (if old
          (efrit-agent--with-render
            (let ((inhibit-read-only t))
              (save-excursion
                (goto-char (car old))
                (delete-region (car old) (cdr old))
                (insert (apply #'propertize line props)))))
        (efrit-agent--append-to-conversation line props)))))

(defun efrit-agent--find-note (key)
  "Bounds (START . END) of the last note line whose `efrit-note-key' is KEY, or nil."
  (let ((pos (and efrit-agent--conversation-end (marker-position efrit-agent--conversation-end)))
        (found nil))
    (while (and pos (not found)
                (setq pos (previous-single-property-change pos 'efrit-note-key)))
      (let ((v (get-text-property pos 'efrit-note-key)))
        (when (eq v key)
          (setq found (cons pos (or (next-single-property-change pos 'efrit-note-key) pos))))))
    found))

(defun efrit-agent--on-steered (event)
  "Subscriber: the steering text was handed to the model; say so under its line."
  (efrit-agent--in-agent-buffer event
    (efrit-agent--append-to-conversation
     (concat (propertize "  ↳ delivered with the tool results" 'face 'shadow) "\n")
     (list 'efrit-type 'steer-note 'efrit-steer-text (alist-get :text event)))))

(defun efrit-agent--on-steer-queued (event)
  "Subscriber: the turn ended before a tool round could take the steer; it runs next."
  (efrit-agent--in-agent-buffer event
    (efrit-agent--append-to-conversation
     (concat (propertize "  ↳ the turn ended first; sent as the next turn" 'face 'shadow) "\n")
     (list 'efrit-type 'steer-note 'efrit-steer-text (alist-get :text event)))))

(defun efrit-agent--on-status (event)
  (efrit-agent-set-status (alist-get :status event)
                          (efrit-agent-buffer-for (alist-get :session-id event))))

;;; Session lifecycle (the efrit-do path)

(defun efrit-agent--on-session-start (event)
  (efrit-agent-start-session (alist-get :session-id event) (alist-get :command event))
  (setq efrit-agent--activity-counter 0))

(defun efrit-agent--on-session-end (event)
  (efrit-agent-end-session (alist-get :success event) nil nil nil
                           (efrit-agent-buffer-for (alist-get :session-id event))))

;;; Questions

(defun efrit-agent--on-question (event)
  "Subscriber: the model asked the user something; show it and wait."
  (efrit-agent--in-agent-buffer event
    (let* ((question (alist-get :question event))
           (options (alist-get :options event))
           (opts (when options (if (listp options) options (append options nil)))))
      (setq efrit-agent--pending-question
            (list question opts (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
      (setq efrit-agent--status 'waiting)
      (efrit-agent--add-question question opts))))

(defun efrit-agent--on-question-answered (event)
  (efrit-agent--in-agent-buffer event
    (when-let* ((response (alist-get :response event)))
      (efrit-agent--add-user-message (format "%s" response)))
    (setq efrit-agent--pending-question nil
          efrit-agent--pending-question-multi nil)
    (efrit-agent--close-question-menu)
    (setq efrit-agent--status 'working)
    (efrit-agent--reset-input-prompt)))

;;; Subscriptions

(defconst efrit-agent--subscriptions
  '((todos-changed . efrit-agent--on-todos-changed)
    (tool-start . efrit-agent--on-tool-start)
    (tool-result . efrit-agent--on-tool-result)
    (text-delta . efrit-agent--on-text-delta)
    (text-end . efrit-agent--on-text-end)
    (thinking-start . efrit-agent--on-thinking-start)
    (thinking-stop . efrit-agent--on-thinking-stop)
    (message . efrit-agent--on-message)
    (error . efrit-agent--on-error)
    (status . efrit-agent--on-status)
    (note . efrit-agent--on-note)
    (steered . efrit-agent--on-steered)
    (steer-queued . efrit-agent--on-steer-queued)
    (session-start . efrit-agent--on-session-start)
    (session-end . efrit-agent--on-session-end)
    (question . efrit-agent--on-question)
    (question-answered . efrit-agent--on-question-answered)
    (review-start . efrit-agent--on-review-start)
    (review-verdict . efrit-agent--on-review-verdict)
    (review-skipped . efrit-agent--on-review-skipped))
  "What the agent buffer listens to: (EVENT . HANDLER).")

(defun efrit-agent-setup-integration ()
  "Subscribe the agent buffer to the events it renders.  Idempotent."
  (dolist (sub efrit-agent--subscriptions)
    (efrit-subscribe (car sub) (cdr sub))))

(defun efrit-agent-remove-integration ()
  "Unsubscribe the agent buffer from every event."
  (dolist (sub efrit-agent--subscriptions)
    (efrit-unsubscribe (car sub) (cdr sub))))

;;; Public API wrappers (for use by other modules)

(declare-function efrit-agent--add-todos-inline "efrit-agent-render")
(declare-function efrit-agent-start-session "efrit-agent")
(declare-function efrit-agent-end-session "efrit-agent")
(declare-function efrit-agent-set-status "efrit-agent")

(defun efrit-agent-update-todos (todos &optional buffer)
  "Update the TODO list display with TODOS in BUFFER (default the default agent buffer).
TODOS should be a list of plists with :status, :content, :activeForm.
Uses incremental inline update instead of full re-render."
  (let ((buffer (or buffer (get-buffer efrit-agent-buffer-name))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq efrit-agent--todos todos)
        ;; Use incremental inline update instead of full re-render
        (efrit-agent--add-todos-inline todos)))))

;; efrit-agent-start-session and efrit-agent-end-session are defined in
;; efrit-agent.el. Duplicate wrapper versions here were always shadowed
;; (efrit-agent.el requires this file before defining its own) and were
;; removed (ef-d89).

;;; Review rows
;;
;; The second-model review (efrit-review) publishes review-start and
;; review-verdict.  Show it as a tool-style row so the user sees that
;; the turn was judged, by which model, and the outcome -- approvals
;; included, not only rejections.  The row expands (TAB / d) to what
;; the reviewer was shown and what it answered.

(defvar-local efrit-agent--review-row nil
  "Tool-row id of the review in flight, or nil.")

(defun efrit-agent--on-review-start (event)
  (let ((buffer (efrit-agent-buffer-for (alist-get :session-id event))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((input (make-hash-table :test 'equal)))
          (puthash "calls" (alist-get :calls event) input)
          (puthash "model" (alist-get :model event) input)
          (puthash "shown to reviewer" (alist-get :prompt event) input)
          (setq efrit-agent--review-row
                (efrit-agent--add-tool-call "review" input)))))))

(defun efrit-agent--on-review-verdict (event)
  (let ((buffer (efrit-agent-buffer-for (alist-get :session-id event))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when efrit-agent--review-row
          (let* ((verdict (alist-get :verdict event))
                 (reason (alist-get :reason event))
                 (text (pcase verdict
                         ('approve "approved")
                         ('reject (concat "rejected · " (or reason "no reason given")))
                         (_ (format "%s" verdict)))))
            (efrit-agent--update-tool-result efrit-agent--review-row text
                                             (eq verdict 'approve) nil)
            (setq efrit-agent--review-row nil)))))))

(defcustom efrit-agent-show-review-skips t
  "When non-nil, a turn that needs no review gets a one-line dim note.
Reads and efrit's own control tools are not reviewed; the note says
so, for example: not reviewed: read-only turn (fetch_url)."
  :type 'boolean
  :group 'efrit-agent)

(defun efrit-agent--on-review-skipped (event)
  (when efrit-agent-show-review-skips
    (efrit-agent--in-agent-buffer event
      (efrit-agent--append-to-conversation
       (concat (propertize (format "  ⚖ not reviewed: %s" (alist-get :reason event))
                           'face 'efrit-agent-timestamp)
               "\n")
       (list 'efrit-type 'review-note)))))

;; Subscribed at load, not in eval-after-load: efrit-events is already
;; required above, and an eval-after-load body would run again on
;; every efrit-reload.  efrit-agent.el calls this too; both are safe.
(efrit-agent-setup-integration)

(provide 'efrit-agent-integration)

;;; efrit-agent-integration.el ends here
