;;; efrit-tool-media.el --- generate_image / generate_video tools and commands -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.16.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai, multimedia

;;; Commentary:

;; Two faces of `efrit-media':
;;
;; - Tools the model calls: `generate_image' and `generate_video' take
;;   a prompt (and a backend name, extra options), run the request as
;;   a process while the turn waits, and return the file's path with
;;   its provenance.  Both are `exec' class: the sandbox asks for the
;;   endpoint's host under `net', and the reviewer sees the prompt
;;   text.  The result is shown in the agent buffer under the tool
;;   row: the image itself, or a line naming the video with keys to
;;   open it.
;;
;; - Commands for the user: `efrit-generate-image' and
;;   `efrit-generate-video' read a prompt (the region when active) and
;;   generate in the background, then show the result in the agent
;;   buffer when one exists, else open it.  `efrit-media-copy-to-project'
;;   and `efrit-media-move-to-project' carry a result (default the
;;   newest) into the project's media/ directory, with its sidecar.
;;
;; The tools register only when a backend of their kind exists, so a
;; configuration without image models offers the model nothing to
;; call; `efrit-media-ensure-tools' runs at load and after
;; `efrit-media-add-backend'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-media)
(require 'efrit-tool-registry)
(require 'efrit-events)
(require 'efrit-log)

(declare-function efrit-sandbox-check "efrit-sandbox" (cap target &optional tool detail))
(declare-function efrit-agent-buffer-for "efrit-agent-core" (&optional session-id))
(declare-function efrit-markdown--make-image "efrit-markdown" (file width &optional exact))
(defvar efrit-sandbox-enabled)
(defvar efrit-markdown-image-max-width)
(defvar efrit-current-session-id)

;;;; Shared

(defun efrit-tool-media--host (backend)
  "The host of BACKEND's URL, for the sandbox."
  (or (url-host (url-generic-parse-url (efrit-media-backend-url backend))) "unknown-host"))

(defun efrit-tool-media--check (backend tool prompt)
  "Ask the sandbox for BACKEND's host on behalf of TOOL with PROMPT as the detail."
  (when (bound-and-true-p efrit-sandbox-enabled)
    (require 'efrit-sandbox)
    (efrit-sandbox-check 'net (cons 'host (efrit-tool-media--host backend)) tool
                         (format "%s with %s: %s" tool (efrit-media-backend-name backend)
                                 (truncate-string-to-width prompt 120 nil nil "…")))))

(defun efrit-tool-media--options (input)
  "The extra request options from tool INPUT's \"options\" object, as an alist."
  (let ((o (alist-get "options" input nil nil #'equal)))
    (cond ((null o) nil)
          ((hash-table-p o) (let (out) (maphash (lambda (k v) (push (cons k v) out)) o) (nreverse out)))
          ((listp o) o)
          (t nil))))

(defun efrit-tool-media--result-text (file kind backend seconds)
  "The tool result for FILE of KIND made by BACKEND in SECONDS."
  (format "Generated %s saved to %s (%s, %s, %.0f s, %d bytes). The user sees it in the agent buffer; %s. Give the path when you refer to it."
          kind file (efrit-media-backend-name backend) (efrit-media-backend-model backend) seconds
          (file-attribute-size (file-attributes file))
          (if (eq kind 'image)
              "do not read it back with read_image unless you must check a detail"
            "they can open it with M-x efrit-media-open")))

(defun efrit-tool-media--run (kind input tool)
  "Generate KIND for tool INPUT; TOOL is the tool's name.  Returns the result text."
  (let* ((prompt (string-trim (or (alist-get "prompt" input nil nil #'equal) "")))
         (name (alist-get "backend" input nil nil #'equal))
         (options (efrit-tool-media--options input)))
    (when (string-empty-p prompt)
      (signal 'user-error (list (format "%s: prompt is required" tool))))
    (let ((backend (efrit-media-resolve-backend kind name))
          (started (float-time)))
      (efrit-tool-media--check backend tool prompt)
      (let ((file (efrit-media-generate-sync prompt :kind kind :backend backend :options options)))
        (efrit-tool-media-show file kind prompt efrit-current-session-id)
        (efrit-tool-media--result-text file kind backend (- (float-time) started))))))

(defun efrit-tool-media-generate-image (input)
  "The `generate_image' tool over INPUT."
  (efrit-tool-media--run 'image input "generate_image"))

(defun efrit-tool-media-generate-video (input)
  "The `generate_video' tool over INPUT."
  (efrit-tool-media--run 'video input "generate_video"))

;;;; Showing the result

(defun efrit-tool-media-show (file kind prompt &optional session-id)
  "Publish a transcript note for FILE of KIND made from PROMPT, in SESSION-ID's buffer.
The agent buffer draws the image on the line; a video gets a line
with its name.  Without an agent buffer the note goes nowhere, which
is why the commands open the file themselves."
  (efrit-publish 'note
                 `((:text . ,(format "%s %s" (if (eq kind 'image) "🖼" "🎞") (abbreviate-file-name file)))
                   (:face . shadow) (:kind . media)
                   (:media-file . ,file) (:media-kind . ,kind) (:media-prompt . ,prompt)
                   ,@(and session-id `((:session-id . ,session-id))))))

(defun efrit-tool-media--backend-list (kind)
  "The names of KIND's backends for a tool description, the default first."
  (mapconcat #'efrit-media-backend-name (efrit-media-backends-for kind) ", "))

(defun efrit-tool-media--schema (kind)
  "The input schema of the KIND tool."
  `(("type" . "object")
    ("properties" . (("prompt" . (("type" . "string")
                                  ("description" . ,(format "What the %s shows, in plain words: subject, setting, style, mood. Say what must not appear." kind))))
                     ("backend" . (("type" . "string")
                                   ("description" . ,(format "Which model to use: one of %s. Default: the first."
                                                             (efrit-tool-media--backend-list kind)))))
                     ("options" . (("type" . "object")
                                   ("description" . "Extra request fields the endpoint accepts (size, quality, aspect_ratio...), passed through as given.")))))
    ("required" . ["prompt"])))

(defun efrit-tool-media--description (kind)
  "The KIND tool's description for the model."
  (pcase kind
    ('image (format "Generate an image from a text prompt with a configured model (%s). Takes seconds to a minute. The image is saved under the user's efrit data directory and shown to them; you get the path. Use it when the user asks for a picture, diagram, mockup or illustration, not to look at something that exists (read_image does that)."
                    (efrit-tool-media--backend-list 'image)))
    ('video (format "Generate a short video from a text prompt with a configured model (%s). Takes minutes; the turn waits. The video is saved under the user's efrit data directory; you get the path. Only when the user asks for a video."
                    (efrit-tool-media--backend-list 'video)))))

;;;###autoload
(defun efrit-media-ensure-tools ()
  "Register generate_image and generate_video for the kinds that have a backend.
Withdraws a tool whose kind lost its backends.  Idempotent."
  (interactive)
  (dolist (spec '((image "generate_image" efrit-tool-media-generate-image)
                  (video "generate_video" efrit-tool-media-generate-video)))
    (pcase-let ((`(,kind ,name ,fn) spec))
      (if (efrit-media-backends-for kind)
          (efrit-register-tool name :description (efrit-tool-media--description kind)
                               :input-schema (efrit-tool-media--schema kind)
                               :function fn :class 'exec :package 'efrit-media)
        (efrit-unregister-tool name))))
  (when (called-interactively-p 'any)
    (message "efrit-media: %d image, %d video backend(s); tools %s"
             (length (efrit-media-backends-for 'image)) (length (efrit-media-backends-for 'video))
             (mapconcat #'identity
                        (delq nil (list (and (efrit-registered-tool-get "generate_image") "generate_image")
                                        (and (efrit-registered-tool-get "generate_video") "generate_video")))
                        ", "))))

(advice-add 'efrit-media-add-backend :after (lambda (&rest _) (efrit-media-ensure-tools))
            '((name . efrit-tool-media--after-add)))
(advice-add 'efrit-media-remove-backend :after (lambda (&rest _) (efrit-media-ensure-tools))
            '((name . efrit-tool-media--after-remove)))

(efrit-media-ensure-tools)

;;;; Commands

(defun efrit-tool-media--read-prompt (kind)
  "Ask for a KIND prompt; the region is the default."
  (let ((default (and (use-region-p)
                      (string-trim (buffer-substring-no-properties (region-beginning) (region-end))))))
    (read-string (format "Generate %s%s: " kind (if default " (default: the region)" ""))
                 nil 'efrit-media-prompt-history default)))

(defvar efrit-media-prompt-history nil)

(defun efrit-tool-media--read-backend (kind)
  "With a prefix argument, ask which KIND backend; else nil for the default."
  (when current-prefix-arg
    (let ((names (mapcar #'efrit-media-backend-name (efrit-media-backends-for kind))))
      (unless names (user-error "efrit-media: no %s backend configured" kind))
      (completing-read (format "%s backend: " (capitalize (symbol-name kind))) names nil t))))

(defun efrit-tool-media--command (kind prompt backend)
  "Generate KIND from PROMPT with BACKEND in the background; show or open the result."
  (when (string-empty-p (string-trim prompt))
    (user-error "efrit-media: nothing to generate"))
  (let* ((b (efrit-media-resolve-backend kind backend))
         (session (bound-and-true-p efrit-current-session-id)))
    (message "efrit-media: %s is making your %s…" (efrit-media-backend-name b) kind)
    (efrit-media-generate
     prompt
     (lambda (file err)
       (if err
           (message "efrit-media: %s" err)
         (let ((buffer (and (fboundp 'efrit-agent-buffer-for) (efrit-agent-buffer-for session))))
           (if (buffer-live-p buffer)
               (progn (efrit-tool-media-show file kind prompt session)
                      (message "efrit-media: %s ready: %s (shown in %s)" kind
                               (abbreviate-file-name file) (buffer-name buffer)))
             (message "efrit-media: %s ready: %s" kind (abbreviate-file-name file))
             (efrit-media-open file)))))
     :kind kind :backend b)))

;;;###autoload
(defun efrit-generate-image (prompt &optional backend)
  "Generate an image from PROMPT (the region when active) with BACKEND.
C-u asks which backend.  The result shows in the agent buffer, or
opens in a window when there is none."
  (interactive (list (efrit-tool-media--read-prompt 'image) (efrit-tool-media--read-backend 'image)))
  (efrit-tool-media--command 'image prompt backend))

;;;###autoload
(defun efrit-generate-video (prompt &optional backend)
  "Generate a video from PROMPT (the region when active) with BACKEND.
C-u asks which backend.  The file's line shows in the agent buffer, or
the video opens when there is none."
  (interactive (list (efrit-tool-media--read-prompt 'video) (efrit-tool-media--read-backend 'video)))
  (efrit-tool-media--command 'video prompt backend))

(provide 'efrit-tool-media)

;;; efrit-tool-media.el ends here
