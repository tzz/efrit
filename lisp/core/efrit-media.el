;;; efrit-media.el --- Generate images and videos through pluggable backends -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.16.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai, multimedia

;;; Commentary:

;; A prompt in, a file out: `efrit-media-generate' asks a backend for
;; an image or a video and saves what comes back under
;; `efrit-media-directory'.  The request runs as a curl process, so
;; Emacs stays responsive while a video model works for minutes, and
;; `efrit-media-cancel' stops it.
;;
;; Backends.  A backend is an `efrit-media-backend': a name, the kind
;; of media it makes (`image' or `video'), its protocol, the URL, the
;; model, and how it authenticates.  Two protocols ship:
;;
;; - `openai-images': POST {url} with {"model", "prompt", "n", ...},
;;   the answer carries data[].b64_json (OpenAI's images API and the
;;   gateways that mirror it).
;;
;; - `interactions': POST {url} with {"model", "input"}, the answer
;;   has steps[] whose model_output step has content[] blocks with a
;;   "type" and base64 "data" (Google's Interactions API shape).
;;
;; A package adds a protocol with two methods: `efrit-media-request'
;; (the JSON body for a prompt) and `efrit-media-parse' (the response
;; to (BYTES . EXTENSION)); see the two built-in ones.  Authentication
;; is efrit's API key with its auth scheme by default; a backend may
;; name an auth-source host instead, or supply a header function.
;;
;; `efrit-media-backends' holds the configured backends.  There are
;; no defaults: the URLs and model names belong to whoever runs the
;; gateway, so the user adds them (see the docstring).  The first
;; backend of a kind is the default for that kind.
;;
;; Results.  Each file is named by time and the prompt's first words,
;; with its prompt and backend kept in `efrit-media-log' (a small JSON
;; sidecar per file), so a later `efrit-media-copy-to-project' or
;; `efrit-media-move-to-project' can carry the provenance along.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'efrit-log)
(require 'efrit-config)
(require 'efrit-common)
(require 'efrit-api)
(require 'efrit-http)
(require 'browse-url)

(declare-function efrit-tool--get-project-root "efrit-tool-utils")

(defgroup efrit-media nil
  "Image and video generation."
  :group 'efrit
  :prefix "efrit-media-")

(defcustom efrit-media-directory
  (expand-file-name "media" efrit-data-directory)
  "Where generated images and videos are saved."
  :type 'directory)

(defcustom efrit-media-timeout 600
  "Seconds a generation request may take before it is given up."
  :type 'integer)

(defcustom efrit-media-backends nil
  "The generation backends, as `efrit-media-backend' structs.
Add one with `efrit-media-add-backend'; for example, an OpenAI-shaped
image endpoint behind the same gateway as efrit's chat model:

  (efrit-media-add-backend
   :name \"sunburst\" :kind \\='image :protocol \\='openai-images
   :url \"https://gateway.example.com/openai/v1/images/generations\"
   :model \"gpt-image-2.5-sunburst\")

and an Interactions-shaped video endpoint:

  (efrit-media-add-backend
   :name \"omni\" :kind \\='video :protocol \\='interactions
   :url \"https://gateway.example.com/genai/v1/projects/P/locations/global/interactions\"
   :model \"gemini-omni-1.2-flash\")

The first backend of each kind is the default for `efrit-media-generate'."
  :type '(repeat sexp))

(cl-defstruct (efrit-media-backend (:constructor efrit-media-backend--create))
  "How to reach one generation model."
  name        ; string: what the user and the model call it
  kind        ; `image' or `video'
  protocol    ; symbol: the request/response shape (openai-images, interactions, ...)
  url         ; string: the endpoint
  model       ; string: the model name sent in the body
  auth        ; nil: efrit's API key; a string: an auth-source host for `efrit-auth'
  headers     ; function of no arguments returning extra ((NAME . VALUE) ...) or nil
  options)    ; alist merged into the request body (size, aspect ratio...)

(cl-defun efrit-media-add-backend (&key name kind protocol url model auth headers options)
  "Add or replace the backend NAME in `efrit-media-backends'.
KIND is `image' or `video'; PROTOCOL a symbol with `efrit-media-request'
and `efrit-media-parse' methods; URL and MODEL as the gateway wants
them; AUTH nil for efrit's API key or an auth-source host; HEADERS a
function giving extra headers; OPTIONS an alist merged into the body."
  (unless (and (stringp name) (not (string-empty-p name)))
    (error "efrit-media: a backend needs a name"))
  (unless (memq kind '(image video))
    (error "efrit-media: %s: kind must be image or video, not %S" name kind))
  (unless (and (symbolp protocol) protocol)
    (error "efrit-media: %s: protocol must be a symbol" name))
  (unless (and (stringp url) (string-match-p "\\`https?://" url))
    (error "efrit-media: %s: url must be an http(s) URL" name))
  (unless (and (stringp model) (not (string-empty-p model)))
    (error "efrit-media: %s: model is required" name))
  (let ((backend (efrit-media-backend--create
                  :name name :kind kind :protocol protocol :url url :model model
                  :auth auth :headers headers :options options)))
    (setq efrit-media-backends
          (append (cl-remove name efrit-media-backends
                             :key #'efrit-media-backend-name :test #'equal)
                  (list backend)))
    backend))

(defun efrit-media-remove-backend (name)
  "Remove the backend NAME.  Returns non-nil when there was one."
  (prog1 (efrit-media-backend name)
    (setq efrit-media-backends
          (cl-remove name efrit-media-backends :key #'efrit-media-backend-name :test #'equal))))

(defun efrit-media-backend (name)
  "The backend called NAME, or nil."
  (seq-find (lambda (b) (equal (efrit-media-backend-name b) name)) efrit-media-backends))

(defun efrit-media-backends-for (kind)
  "The backends that make KIND, in configuration order."
  (seq-filter (lambda (b) (eq (efrit-media-backend-kind b) kind)) efrit-media-backends))

(defun efrit-media-default-backend (kind)
  "The first backend for KIND, or nil."
  (car (efrit-media-backends-for kind)))

(defun efrit-media-resolve-backend (kind &optional name)
  "The backend NAME when given (it must make KIND), else the default for KIND.
Signals a `user-error' naming the fix when there is none."
  (cond
   ((and name (not (string-empty-p name)))
    (let ((b (efrit-media-backend name)))
      (unless b
        (user-error "efrit-media: no backend named %s (known: %s)" name
                    (or (mapconcat #'efrit-media-backend-name efrit-media-backends ", ") "none")))
      (unless (eq (efrit-media-backend-kind b) kind)
        (user-error "efrit-media: %s makes %ss, not %ss" name (efrit-media-backend-kind b) kind))
      b))
   ((efrit-media-default-backend kind))
   (t (user-error "efrit-media: no %s backend configured; see `efrit-media-backends'" kind))))

;;;; Protocols

(cl-defgeneric efrit-media-request (protocol backend prompt options)
  "The request body (an alist, JSON-encoded by the caller) for PROMPT.
PROTOCOL is the backend's protocol symbol, BACKEND the struct, OPTIONS
an alist of extras from the caller merged over the backend's own.")

(cl-defgeneric efrit-media-parse (protocol kind text)
  "The media in response TEXT: (BYTES . EXTENSION), BYTES a unibyte string.
KIND is what was asked for (`image' or `video').  Signals an error
with a readable message when the response has none.")

(defun efrit-media--merge-options (backend options)
  "BACKEND's options with OPTIONS laid over them."
  (let ((merged (copy-alist (efrit-media-backend-options backend))))
    (dolist (o options)
      (setf (alist-get (car o) merged nil nil #'equal) (cdr o)))
    merged))

(defun efrit-media--json-parse (text)
  "TEXT as nested alists with symbol keys, lists for arrays, nil for null/false."
  (json-parse-string text :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun efrit-media--json-get (obj &rest keys)
  "OBJ (from `efrit-media--json-parse') followed through KEYS; nil when absent.
A string or symbol key reads an object member, an integer an array element."
  (let ((cur obj))
    (dolist (k keys)
      (setq cur (cond ((null cur) nil)
                      ((integerp k) (nth k cur))
                      (t (alist-get (if (stringp k) (intern k) k) cur)))))
    cur))

(defun efrit-media--decode-base64 (data what)
  "DATA base64-decoded as a unibyte string; a readable error names WHAT."
  (condition-case nil
      (base64-decode-string (string-trim data))
    (error (error "efrit-media: the %s data is not valid base64" what))))

;; OpenAI images: {"model","prompt","n",...} -> {"data":[{"b64_json":...}]}

(cl-defmethod efrit-media-request ((_protocol (eql 'openai-images)) backend prompt options)
  (append `(("model" . ,(efrit-media-backend-model backend))
            ("prompt" . ,prompt)
            ("n" . 1))
          (efrit-media--merge-options backend options)))

(cl-defmethod efrit-media-parse ((_protocol (eql 'openai-images)) _kind text)
  (let* ((obj (efrit-media--json-parse text))
         (first (efrit-media--json-get obj "data" 0))
         (b64 (efrit-media--json-get first "b64_json"))
         (url (efrit-media--json-get first "url"))
         (format (or (efrit-media--json-get obj "output_format")
                     (efrit-media--json-get first "output_format")
                     "png")))
    (cond
     (b64 (cons (efrit-media--decode-base64 b64 "image") (format "%s" format)))
     (url (error "efrit-media: the endpoint returned a URL (%s), not the image bytes; ask it for b64_json" url))
     (t (error "efrit-media: no image in the response: %s"
               (or (efrit-media--json-get obj "error" "message")
                   (truncate-string-to-width text 200 nil nil "…")))))))

;; Interactions: {"model","input"} -> {"steps":[{"type":"model_output","content":[{"type":"video","data":...}]}]}

(cl-defmethod efrit-media-request ((_protocol (eql 'interactions)) backend prompt options)
  (append `(("model" . ,(efrit-media-backend-model backend))
            ("input" . ,prompt))
          (efrit-media--merge-options backend options)))

(defconst efrit-media--interactions-extensions
  '(("video" . "mp4") ("image" . "png") ("audio" . "mp3"))
  "File extension per Interactions content type, when the block names no MIME type.")

(defun efrit-media--extension-for-mime (mime)
  "The file extension for MIME, or nil."
  (pcase mime
    ("image/png" "png") ("image/jpeg" "jpg") ("image/webp" "webp") ("image/gif" "gif")
    ("video/mp4" "mp4") ("video/webm" "webm") ("audio/mpeg" "mp3") ("audio/wav" "wav")
    (_ nil)))

(cl-defmethod efrit-media-parse ((_protocol (eql 'interactions)) kind text)
  (let* ((obj (efrit-media--json-parse text))
         (steps (efrit-media--json-get obj "steps"))
         (wanted (symbol-name kind))
         (block (cl-loop for step in steps
                         when (equal (efrit-media--json-get step "type") "model_output")
                         thereis (seq-find (lambda (c) (equal (efrit-media--json-get c "type") wanted))
                                           (efrit-media--json-get step "content")))))
    (unless block
      (error "efrit-media: no %s in the response: %s" wanted
             (or (efrit-media--json-get obj "error" "message")
                 (truncate-string-to-width text 200 nil nil "…"))))
    (let ((data (efrit-media--json-get block "data"))
          (mime (or (efrit-media--json-get block "mime_type")
                    (efrit-media--json-get block "mimeType"))))
      (unless (stringp data)
        (error "efrit-media: the %s block carries no data" wanted))
      (cons (efrit-media--decode-base64 data wanted)
            (or (and mime (efrit-media--extension-for-mime mime))
                (alist-get wanted efrit-media--interactions-extensions "bin" nil #'equal))))))

;;;; The request

(cl-defstruct (efrit-media-job (:constructor efrit-media-job--create))
  "One generation in flight."
  backend prompt kind request started callback)

(defvar efrit-media--active nil
  "The `efrit-media-job's in flight.")

(defvar efrit-media-last nil
  "The file the most recent generation produced.")

(defun efrit-media--slug (prompt)
  "The first words of PROMPT as a file-name fragment."
  (let* ((words (split-string (downcase prompt) "[^[:alnum:]]+" t))
         (slug (string-join (seq-take words 5) "-")))
    (if (string-empty-p slug) "media" (truncate-string-to-width slug 40))))

(defun efrit-media-file-name (prompt extension)
  "A fresh path under `efrit-media-directory' for PROMPT's result with EXTENSION."
  (make-directory efrit-media-directory t)
  (let* ((base (format "%s-%s" (format-time-string "%Y%m%d-%H%M%S") (efrit-media--slug prompt)))
         (path (expand-file-name (concat base "." extension) efrit-media-directory))
         (n 1))
    (while (file-exists-p path)
      (setq path (expand-file-name (format "%s-%d.%s" base (cl-incf n) extension) efrit-media-directory)))
    path))

(defun efrit-media--sidecar (file)
  "The provenance file beside FILE."
  (concat file ".json"))

(defun efrit-media--write-sidecar (file job)
  "Record JOB's prompt and backend beside FILE."
  (let ((backend (efrit-media-job-backend job)))
    (with-temp-file (efrit-media--sidecar file)
      (insert (json-encode
               `((prompt . ,(efrit-media-job-prompt job))
                 (kind . ,(symbol-name (efrit-media-job-kind job)))
                 (backend . ,(efrit-media-backend-name backend))
                 (model . ,(efrit-media-backend-model backend))
                 (created . ,(format-time-string "%FT%T%z"))
                 (seconds . ,(float-time (time-since (efrit-media-job-started job))))))))))

(defun efrit-media-provenance (file)
  "The prompt, backend and model recorded for FILE, as an alist, or nil."
  (let ((side (efrit-media--sidecar file)))
    (when (file-readable-p side)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents side)
          (json-parse-buffer :object-type 'alist :array-type 'list))))))

(defun efrit-media--finish (job response error)
  "Turn JOB's RESPONSE text (or ERROR) into a file for the callback."
  (let ((callback (efrit-media-job-callback job))
        (backend (efrit-media-job-backend job)))
    (setq efrit-media--active (delq job efrit-media--active))
    (condition-case err
        (cond
         (error
          (funcall callback nil (if (equal error "cancelled") error
                                  (format "%s: %s" (efrit-media-backend-name backend) error))))
         (t
          (pcase-let* ((`(,bytes . ,ext) (efrit-media-parse (efrit-media-backend-protocol backend)
                                                            (efrit-media-job-kind job) response))
                       (file (efrit-media-file-name (efrit-media-job-prompt job) ext)))
            (let ((coding-system-for-write 'no-conversion))
              (with-temp-file file
                (set-buffer-multibyte nil)
                (insert bytes)))
            (efrit-media--write-sidecar file job)
            (setq efrit-media-last file)
            (efrit-log 'info "media: %s made %s in %.0fs" (efrit-media-backend-name backend)
                       (file-name-nondirectory file) (float-time (time-since (efrit-media-job-started job))))
            (funcall callback file nil))))
      (error (funcall callback nil (error-message-string err))))))

(cl-defun efrit-media-generate (prompt callback &key kind backend options)
  "Generate KIND (`image' by default) from PROMPT with BACKEND; call CALLBACK.
BACKEND is a name or nil for the kind's default; OPTIONS an alist laid
over the backend's.  CALLBACK is (lambda (FILE ERROR)): FILE the saved
path on success, ERROR a string otherwise.  Returns the job, for
`efrit-media-cancel'."
  (let* ((kind (or kind 'image))
         (backend (if (efrit-media-backend-p backend) backend
                    (efrit-media-resolve-backend kind backend)))
         (body (efrit-media-request (efrit-media-backend-protocol backend) backend prompt options))
         (job (efrit-media-job--create :backend backend :prompt prompt :kind kind
                                       :started (current-time) :callback callback)))
    (efrit-log 'info "media: %s %s: %s" (efrit-media-backend-name backend) kind
               (truncate-string-to-width prompt 80 nil nil "…"))
    (setf (efrit-media-job-request job)
          (efrit-http-post-json (efrit-media-backend-url backend) body
                                (lambda (text err) (efrit-media--finish job text err))
                                :auth (efrit-media-backend-auth backend)
                                :headers (when-let* ((fn (efrit-media-backend-headers backend))) (funcall fn))
                                :label (format "media %s (%s)" kind (efrit-media-backend-name backend))
                                :timeout efrit-media-timeout))
    (push job efrit-media--active)
    job))

(defun efrit-media-cancel (&optional job)
  "Cancel JOB, or every generation in flight when nil."
  (interactive)
  (let ((jobs (if job (list job) (copy-sequence efrit-media--active))))
    (dolist (j jobs)
      (when (efrit-media-job-request j)
        (efrit-http-cancel (efrit-media-job-request j))))
    (when (called-interactively-p 'any)
      (message "efrit-media: %d generation%s cancelled" (length jobs) (if (= 1 (length jobs)) "" "s")))
    (length jobs)))

(defun efrit-media-generate-sync (prompt &rest args)
  "Like `efrit-media-generate' but wait; return the file or signal.
ARGS are the keyword arguments of `efrit-media-generate'.  Emacs keeps
processing while waiting; C-g cancels the request."
  (let ((result nil) (done nil))
    (let ((job (apply #'efrit-media-generate prompt
                      (lambda (file err) (setq result (or file err) done t))
                      args)))
      (efrit-http-wait-while (lambda () (not done))
                             (lambda () (efrit-media-cancel job))))
    (if (and (stringp result) (file-exists-p result))
        result
      (error "%s" (or result "efrit-media: no result")))))

;;;; Keeping results

(defun efrit-media-files ()
  "The generated files, newest first (sidecars and posters left out)."
  (when (file-directory-p efrit-media-directory)
    (sort (seq-remove (lambda (f) (or (string-suffix-p ".json" f) (string-suffix-p ".poster.png" f)))
                      (directory-files efrit-media-directory t "\\`[0-9]"))
          #'string>)))

(defun efrit-media--read-file (prompt)
  "Ask for a generated file with PROMPT; the newest first."
  (let ((files (efrit-media-files)))
    (unless files (user-error "efrit-media: nothing generated yet"))
    (let ((choice (completing-read prompt (mapcar #'file-name-nondirectory files)
                                   nil t nil nil (file-name-nondirectory (car files)))))
      (expand-file-name choice efrit-media-directory))))

(defun efrit-media--project-target (file)
  "Where FILE goes in the project: under media/ of the project root, same name."
  (let* ((root (or (ignore-errors (efrit-tool--get-project-root)) default-directory))
         (dir (expand-file-name "media" root)))
    (make-directory dir t)
    (expand-file-name (file-name-nondirectory file) dir)))

(defun efrit-media--transfer (file move)
  "Copy FILE and its sidecar into the project; MOVE deletes the originals."
  (let ((target (efrit-media--project-target file))
        (side (efrit-media--sidecar file)))
    (when (file-exists-p target)
      (unless (yes-or-no-p (format "%s exists; overwrite? " (abbreviate-file-name target)))
        (user-error "efrit-media: not %s" (if move "moved" "copied"))))
    (dolist (pair (list (cons file target)
                        (cons side (efrit-media--sidecar target))
                        (cons (concat (file-name-sans-extension file) ".poster.png")
                              (concat (file-name-sans-extension target) ".poster.png"))))
      (when (file-exists-p (car pair))
        (if move (rename-file (car pair) (cdr pair) t) (copy-file (car pair) (cdr pair) t))))
    (kill-new target)
    (message "efrit-media: %s %s (path copied)" (if move "moved to" "copied to")
             (abbreviate-file-name target))
    target))

;;;###autoload
(defun efrit-media-copy-to-project (file)
  "Copy generated FILE (default the newest) into the project's media/ directory.
The path lands in the kill ring."
  (interactive (list (or (and (not current-prefix-arg) efrit-media-last)
                         (efrit-media--read-file "Copy to the project: "))))
  (efrit-media--transfer file nil))

;;;###autoload
(defun efrit-media-move-to-project (file)
  "Move generated FILE (default the newest) into the project's media/ directory.
The path lands in the kill ring."
  (interactive (list (or (and (not current-prefix-arg) efrit-media-last)
                         (efrit-media--read-file "Move to the project: "))))
  (efrit-media--transfer file t))

;;;###autoload
(defcustom efrit-media-video-player nil
  "How a generated video is opened.
nil: the system's opener (`open' on macOS, `xdg-open' elsewhere), so
the video starts in whatever plays .mp4 there.  A string: a program
run with the file as its argument (\"mpv\", \"vlc\").  A function:
called with the file.  `browse-url-of-file' went through Emacs's own
`auto-mode-alist' and landed in a media mode that showed a poster
and played nothing (2026-11-10)."
  :type '(choice (const :tag "System opener" nil) string function))

(defcustom efrit-media-poster-program (executable-find "ffmpeg")
  "ffmpeg, for a video's first frame in the transcript; nil shows none."
  :type '(choice (const nil) string))

(defun efrit-media-video-poster (file)
  "A PNG of FILE's first frame beside it, made once with ffmpeg; nil without ffmpeg."
  (let ((poster (concat (file-name-sans-extension file) ".poster.png")))
    (cond
     ((file-exists-p poster) poster)
     ((and efrit-media-poster-program (file-exists-p file))
      (with-temp-buffer
        (if (zerop (call-process efrit-media-poster-program nil t nil
                                 "-v" "error" "-y" "-ss" "0.5" "-i" file
                                 "-frames:v" "1" "-vf" "scale=800:-1" poster))
            poster
          (efrit-log 'warn "media: no poster for %s: %s" (file-name-nondirectory file)
                     (string-trim (buffer-string)))
          nil)))
     (t nil))))

(defun efrit-media--probe (file &rest args)
  "ffprobe's output for FILE with ARGS, or nil without ffprobe or on failure."
  (when-let* ((probe (and efrit-media-poster-program (executable-find "ffprobe"))))
    (with-temp-buffer
      (when (zerop (apply #'call-process probe nil t nil "-v" "error"
                          (append args (list (expand-file-name file)))))
        (string-trim (buffer-string))))))

(defun efrit-media-video-info (file)
  "One line about video FILE: duration, frame size, audio, file size (ffprobe when present).
A silent clip says so: the generated ones carry a track or do not, and
the player's mute button is not where one looks first."
  (let* ((seconds (string-to-number (or (efrit-media--probe file "-show_entries" "format=duration"
                                                            "-of" "default=nw=1:nk=1")
                                        "0")))
         (streams (efrit-media--probe file "-show_entries" "stream=codec_type,width,height"
                                      "-of" "csv=p=0"))
         (video (and streams (seq-find (lambda (l) (string-prefix-p "video," l)) (split-string streams "\n" t))))
         (size (and video (pcase (split-string video ",")
                            (`("video" ,w ,h . ,_) (and (> (length w) 0) (format "%sx%s" w h))))))
         (audio (and streams (string-match-p "^audio" streams))))
    (string-join
     (delq nil (list (and (> seconds 0) (format "%d s" (round seconds)))
                     size
                     (and streams (if audio "with audio" "silent"))
                     (file-size-human-readable (file-attribute-size (file-attributes file)))))
     ", ")))

(defun efrit-media--system-open (file)
  "Open FILE with the operating system's opener."
  (let ((program (cond ((eq system-type 'darwin) "open")
                       ((memq system-type '(windows-nt cygwin)) "start")
                       (t "xdg-open"))))
    (unless (executable-find program)
      (user-error "efrit-media: no %s on this system; set `efrit-media-video-player'" program))
    (start-process "efrit-media-open" nil program (expand-file-name file))))

;;;###autoload
(defun efrit-media-open (file)
  "Open generated FILE (default the newest): images in Emacs, videos per `efrit-media-video-player'."
  (interactive (list (or (and (not current-prefix-arg) efrit-media-last)
                         (efrit-media--read-file "Open: "))))
  (cond
   ((member (downcase (or (file-name-extension file) "")) '("png" "jpg" "jpeg" "gif" "webp"))
    (find-file-other-window file))
   ((functionp efrit-media-video-player) (funcall efrit-media-video-player file))
   ((stringp efrit-media-video-player)
    (start-process "efrit-media-player" nil efrit-media-video-player (expand-file-name file)))
   (t (efrit-media--system-open file))))

(provide 'efrit-media)

;;; efrit-media.el ends here
