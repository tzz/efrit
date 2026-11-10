;;; test-media.el --- efrit-media: backends, protocols, files, tools -*- lexical-binding: t; -*-

;;; Commentary:

;; Runs the generation path end to end over a fake curl
;; (test/scripts/mock-media-curl.py): both protocols, the saved file
;; and its sidecar, errors, cancel, the tools' registration, and the
;; transcript note.  Nothing here touches the network.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-media)
(require 'efrit-tool-media)

(defconst test-media--script
  (expand-file-name "scripts/mock-media-curl.py"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defmacro test-media--with (&rest body)
  "Run BODY with a scratch media directory, the fake curl, and two backends."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "efrit-media-test" t))
          (capture (expand-file-name "capture.json" dir))
          (efrit-media-directory (expand-file-name "media" dir))
          (efrit-media-curl-program test-media--script)
          (efrit-media-backends nil)
          (efrit-media-last nil)
          (efrit-media--active nil)
          (efrit-api-key "sk-test-key-1234567890abcdefghij")
          (efrit-api-auth-scheme 'x-api-key)
          (efrit-api-request-transforms nil)
          (efrit-sandbox-enabled nil)
          (process-environment (cons (concat "MOCK_MEDIA_CAPTURE=" capture) process-environment)))
     (skip-unless (executable-find "python3"))
     (efrit-media-add-backend :name "pic" :kind 'image :protocol 'openai-images
                              :url "https://gw.example.test/v1/images" :model "paint-1"
                              :options '(("size" . "1024x1024")))
     (efrit-media-add-backend :name "clip" :kind 'video :protocol 'interactions
                              :url "https://gw.example.test/v1/interactions" :model "film-2"
                              :headers (lambda () '(("x-client" . "efrit-test"))))
     (unwind-protect (progn ,@body)
       (efrit-media-cancel)
       (delete-directory dir t))))

(defun timer-event-handler-maybe ()
  "Run every timer that is due (batch waits do not fire them)."
  (dolist (timer (copy-sequence timer-list))
    (when (and (memq timer timer-list)
               (time-less-p (timer--time timer) (current-time)))
      (timer-event-handler timer))))

(defun test-media--capture (dir)
  "The request the fake curl recorded under DIR."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "capture.json" dir))
    (json-parse-buffer :object-type 'alist :array-type 'list)))

(ert-deftest test-media-backends-are-added-and-resolved ()
  "Backends register by name, the first of a kind is the default, names
and kinds are checked, removal works."
  (test-media--with
    (should (equal '("pic") (mapcar #'efrit-media-backend-name (efrit-media-backends-for 'image))))
    (should (eq (efrit-media-backend "clip") (efrit-media-default-backend 'video)))
    (should (eq (efrit-media-backend "pic") (efrit-media-resolve-backend 'image nil)))
    (should (eq (efrit-media-backend "pic") (efrit-media-resolve-backend 'image "pic")))
    (should-error (efrit-media-resolve-backend 'image "clip") :type 'user-error)
    (should-error (efrit-media-resolve-backend 'image "nope") :type 'user-error)
    (should-error (efrit-media-add-backend :name "bad" :kind 'audio :protocol 'x :url "https://x" :model "m"))
    (should-error (efrit-media-add-backend :name "bad" :kind 'image :protocol 'x :url "ftp://x" :model "m"))
    ;; replacing keeps one entry; removing leaves the kind without a default
    (efrit-media-add-backend :name "pic" :kind 'image :protocol 'openai-images
                             :url "https://gw.example.test/v2/images" :model "paint-2")
    (should (= 2 (length efrit-media-backends)))
    (should (equal "paint-2" (efrit-media-backend-model (efrit-media-backend "pic"))))
    (should (efrit-media-remove-backend "pic"))
    (should-error (efrit-media-resolve-backend 'image nil) :type 'user-error)))

(ert-deftest test-media-request-bodies-and-parsers ()
  "Each protocol builds its body (backend options under the caller's) and
reads its response shape; a response without media is a readable error."
  (test-media--with
    (let ((pic (efrit-media-backend "pic")) (clip (efrit-media-backend "clip")))
      (should (equal '(("model" . "paint-1") ("prompt" . "a cat") ("n" . 1) ("size" . "512x512"))
                     (efrit-media-request 'openai-images pic "a cat" '(("size" . "512x512")))))
      (should (equal '(("model" . "film-2") ("input" . "a cat"))
                     (efrit-media-request 'interactions clip "a cat" nil))))
    (pcase-let ((`(,bytes . ,ext) (efrit-media-parse 'openai-images 'image
                                                     "{\"data\":[{\"b64_json\":\"aGVsbG8=\"}]}")))
      (should (equal "hello" bytes))
      (should (equal "png" ext)))
    (should (equal "jpeg" (cdr (efrit-media-parse 'openai-images 'image
                                                  "{\"output_format\":\"jpeg\",\"data\":[{\"b64_json\":\"aGk=\"}]}"))))
    (should-error (efrit-media-parse 'openai-images 'image "{\"data\":[{\"url\":\"https://x/y.png\"}]}"))
    (should-error (efrit-media-parse 'openai-images 'image "{\"error\":{\"message\":\"nope\"}}"))
    (pcase-let ((`(,bytes . ,ext)
                 (efrit-media-parse 'interactions 'video
                                    "{\"steps\":[{\"type\":\"model_output\",\"content\":[{\"type\":\"text\",\"text\":\"x\"},{\"type\":\"video\",\"mime_type\":\"video/webm\",\"data\":\"aGk=\"}]}]}")))
      (should (equal "hi" bytes))
      (should (equal "webm" ext)))
    (should (equal "mp4" (cdr (efrit-media-parse 'interactions 'video
                                                 "{\"steps\":[{\"type\":\"model_output\",\"content\":[{\"type\":\"video\",\"data\":\"aGk=\"}]}]}"))))
    (should-error (efrit-media-parse 'interactions 'video "{\"steps\":[]}"))))

(ert-deftest test-media-generate-image-saves-file-and-sidecar ()
  "An image request goes out as curl with the key header and the body the
protocol builds; the PNG lands under the media directory with a
sidecar carrying the prompt, and `efrit-media-last' points at it."
  (test-media--with
    (let ((file (efrit-media-generate-sync "An orange animal doing a backflip" :kind 'image)))
      (should (file-exists-p file))
      (should (string-prefix-p efrit-media-directory file))
      (should (string-match-p "-an-orange-animal-doing-a\\.png\\'" file))
      (should (equal file efrit-media-last))
      ;; the bytes are the PNG the fake returned
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file)
        (should (string-prefix-p "\x89PNG" (buffer-string))))
      (let ((prov (efrit-media-provenance file)))
        (should (equal "An orange animal doing a backflip" (alist-get 'prompt prov)))
        (should (equal "pic" (alist-get 'backend prov)))
        (should (equal "paint-1" (alist-get 'model prov))))
      (let ((sent (test-media--capture dir)))
        (should (equal "https://gw.example.test/v1/images" (alist-get 'url sent)))
        (should (equal "paint-1" (alist-get 'model (alist-get 'body sent))))
        (should (equal "An orange animal doing a backflip" (alist-get 'prompt (alist-get 'body sent))))
        (should (equal "1024x1024" (alist-get 'size (alist-get 'body sent))))
        (should (string-match-p "x-api-key: sk-test" (alist-get 'config sent)))
        (should (member "--fail-with-body" (alist-get 'argv sent))))
      (should (equal (list file) (efrit-media-files))))))

(ert-deftest test-media-generate-video-uses-extra-headers ()
  "A video request through the Interactions protocol: the backend's header
function adds a header, the MP4 is saved with the mime's extension."
  (test-media--with
    (let ((file (efrit-media-generate-sync "waves" :kind 'video)))
      (should (string-suffix-p ".mp4" file))
      (should (string-match-p "x-client: efrit-test" (alist-get 'config (test-media--capture dir))))
      (should (equal "waves" (alist-get 'input (alist-get 'body (test-media--capture dir))))))))

(ert-deftest test-media-errors-are-readable ()
  "A failed request (curl --fail-with-body) reports the endpoint's message
with the backend's name; a non-JSON answer is an error, not a file."
  (test-media--with
    (efrit-media-add-backend :name "bad" :kind 'image :protocol 'openai-images
                             :url "https://gw.example.test/v1/error400" :model "m")
    (let ((err (should-error (efrit-media-generate-sync "x" :kind 'image :backend "bad"))))
      (should (string-match-p "bad: prompt rejected by the safety system" (cadr err))))
    (efrit-media-add-backend :name "junk" :kind 'image :protocol 'openai-images
                             :url "https://gw.example.test/v1/garbage" :model "m")
    (should-error (efrit-media-generate-sync "x" :kind 'image :backend "junk"))
    (should-not (efrit-media-files))))

(ert-deftest test-media-cancel-stops-the-process ()
  "Cancelling a job in flight ends it with \"cancelled\" and no file."
  (test-media--with
    (efrit-media-add-backend :name "slow" :kind 'image :protocol 'openai-images
                             :url "https://gw.example.test/v1/slow" :model "m")
    (let* ((result 'pending)
           (job (efrit-media-generate "x" (lambda (file err) (setq result (or file err)))
                                      :kind 'image :backend "slow")))
      (should (memq job efrit-media--active))
      (should (process-live-p (efrit-media-job-process job)))
      ;; let the script start (a SIGINT before Python installs its
      ;; handler is ignored; the TERM a second later covers it anyway)
      (accept-process-output nil 0.3)
      (efrit-media-cancel job)
      (let ((deadline (+ (float-time) 5)))
        (while (and (eq result 'pending) (< (float-time) deadline))
          (accept-process-output nil 0.05)
          ;; the TERM follow-up is a timer; ERT's batch loop does not
          ;; run timers, so run the due ones by hand
          (timer-event-handler-maybe)))
      (should (equal "cancelled" result))
      (should-not (memq job efrit-media--active))
      (should-not (efrit-media-files)))))

(ert-deftest test-media-copy-and-move-to-project ()
  "copy keeps the original and its sidecar, move removes them; both land
under media/ of the project root and leave the path in the kill ring."
  (test-media--with
    (let* ((root (expand-file-name "proj" dir))
           (file (efrit-media-generate-sync "a cat" :kind 'image)))
      (make-directory root t)
      (cl-letf (((symbol-function 'efrit-tool--get-project-root) (lambda () root))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (let ((copy (efrit-media-copy-to-project file)))
          (should (equal (expand-file-name (file-name-nondirectory file) (expand-file-name "media" root)) copy))
          (should (file-exists-p copy))
          (should (file-exists-p file))
          (should (equal "a cat" (alist-get 'prompt (efrit-media-provenance copy))))
          (should (equal copy (current-kill 0))))
        (let ((moved (efrit-media-move-to-project file)))
          (should (file-exists-p moved))
          (should-not (file-exists-p file))
          (should-not (file-exists-p (efrit-media--sidecar file))))))))

(ert-deftest test-media-tools-follow-the-backends ()
  "generate_image and generate_video register for the kinds that have a
backend, with the backend names in the description, and withdraw when
the last backend of a kind goes; the tool runs the request and returns
the path; the sandbox is asked for the endpoint's host."
  (test-media--with
    (efrit-media-ensure-tools)
    (should (efrit-registered-tool-get "generate_image"))
    (should (efrit-registered-tool-get "generate_video"))
    (should (eq 'exec (efrit-tool-registry-class "generate_image")))
    (should (string-match-p "(pic)" (efrit-registered-tool-description (efrit-registered-tool-get "generate_image"))))
    (efrit-media-remove-backend "clip")
    (should-not (efrit-registered-tool-get "generate_video"))
    (should (efrit-registered-tool-get "generate_image"))
    ;; the tool: result text names the file; the sandbox saw the host
    (let ((asked nil) (shown nil))
      (cl-letf (((symbol-function 'efrit-sandbox-check)
                 (lambda (cap target tool _detail) (setq asked (list cap target tool)) t))
                ((symbol-function 'efrit-tool-media-show)
                 (lambda (file kind &rest _) (setq shown (list file kind)))))
        (let* ((efrit-sandbox-enabled t)
               (result (efrit-tool-registry-dispatch
                        "generate_image"
                        (let ((h (make-hash-table :test 'equal)))
                          (puthash "prompt" "a cat" h)
                          (puthash "options" (let ((o (make-hash-table :test 'equal)))
                                               (puthash "quality" "high" o) o)
                                   h)
                          h))))
          (should (string-match-p "\\`Generated image saved to .*\\.png (pic, paint-1" result))
          (should (equal '(net (host . "gw.example.test") "generate_image") asked))
          (should (eq 'image (cadr shown)))
          (should (file-exists-p (car shown)))
          (should (equal "high" (alist-get 'quality (alist-get 'body (test-media--capture dir))))))))
    ;; a missing prompt is an error result, not a signal
    (should (string-match-p "prompt is required"
                            (efrit-tool-registry-dispatch "generate_image" (make-hash-table :test 'equal))))))

(ert-deftest test-media-note-survives-global-font-lock ()
  "Under `global-font-lock-mode' with a jit function that erases `display'
(what bug-reference did, 2026-11-10), an agent buffer keeps the image:
font-lock is a no-op there, whatever turns it on."
  (require 'efrit-agent)
  (require 'jit-lock)
  (test-media--with
    (let ((efrit-agent-auto-show nil)
          (file (efrit-media-generate-sync "a cat" :kind 'image))
          (eraser (lambda (beg end) (remove-list-of-text-properties beg end '(display)) nil)))
      (with-current-buffer (efrit-agent--get-buffer)
        (let ((inhibit-read-only t)) (erase-buffer))
        (let ((global-font-lock-mode t))
          (efrit-agent-mode)
          ;; what global-font-lock-mode does after the mode body
          (font-lock-mode 1)
          (jit-lock-register eraser))
        (efrit-agent--init-regions)
        (efrit-agent--setup-regions)
        (cl-letf (((symbol-function 'efrit-agent--media-image)
                   (lambda (_file) '(image :type xbm :width 1 :height 1 :data "\0"))))
          (let ((efrit-agent-media-open-by-default t))
            (efrit-tool-media-show file 'image "a cat" nil)))
        (let ((pos (text-property-any (point-min) (point-max) 'efrit-type 'media-note)))
          ;; a fontification pass over the note changes nothing
          (font-lock-ensure pos (point-max))
          ;; what redisplay does: run the fontification functions over
          ;; text not yet marked fontified
          (let ((inhibit-read-only t)) (put-text-property pos (point-max) 'fontified nil))
          (dolist (fn fontification-functions) (funcall fn pos))
          (should (null jit-lock-functions))
          (let ((at (text-property-not-all pos (point-max) 'display nil)))
            (should at)
            (should (equal '(image :type xbm :width 1 :height 1 :data "\0") (get-text-property at 'display))))
          (should (eq font-lock-fontify-region-function #'ignore)))
        (jit-lock-unregister eraser)))))

(ert-deftest test-media-video-poster-info-and-open ()
  "A video gets a first-frame poster beside it (once; ffmpeg), an info line
with duration and size, and opens through the system opener or the
configured player, never through find-file."
  (skip-unless (executable-find "ffmpeg"))
  (test-media--with
    (let* ((video (expand-file-name "clip.mp4" efrit-media-directory)))
      (make-directory efrit-media-directory t)
      (should (zerop (call-process "ffmpeg" nil nil nil "-v" "error" "-y" "-f" "lavfi" "-i" "color=c=red:s=64x48:d=1"
                                   "-pix_fmt" "yuv420p" video)))
      (let ((poster (efrit-media-video-poster video)))
        (should poster)
        (should (file-exists-p poster))
        (should (string-suffix-p ".poster.png" poster))
        ;; made once
        (let ((mtime (file-attribute-modification-time (file-attributes poster))))
          (should (equal poster (efrit-media-video-poster video)))
          (should (equal mtime (file-attribute-modification-time (file-attributes poster))))))
      (should (string-match-p "\\`1 s, 64x48, silent, [0-9.]+[kM]?\\'" (efrit-media-video-info video)))
      ;; posters are not generated files
      (should-not (seq-find (lambda (f) (string-suffix-p ".poster.png" f)) (efrit-media-files)))
      ;; open: a program, never find-file
      (let ((ran nil) (found nil))
        (cl-letf (((symbol-function 'start-process) (lambda (_name _buf prog &rest args) (setq ran (cons prog args)) 'proc))
                  ((symbol-function 'find-file-other-window) (lambda (&rest _) (setq found t))))
          (let ((efrit-media-video-player "mpv"))
            (efrit-media-open video)
            (should (equal "mpv" (car ran)))
            (should (equal (expand-file-name video) (cadr ran))))
          (let ((efrit-media-video-player nil))
            (efrit-media-open video)
            (should (member (car ran) '("open" "xdg-open" "start"))))
          (should-not found)))
      ;; the fold: a video unfolds to the poster and the info line, o plays
      (require 'efrit-agent)
      (let ((efrit-agent-auto-show nil) (played nil))
        (with-current-buffer (efrit-agent--get-buffer)
          (let ((inhibit-read-only t)) (erase-buffer))
          (efrit-agent-mode) (efrit-agent--init-regions) (efrit-agent--setup-regions)
          (efrit-tool-media-show video 'video "red" nil)
          (let ((pos (text-property-any (point-min) (point-max) 'efrit-type 'media-note)))
            (should (string-match-p "video: clip.mp4  (o plays it)" (buffer-substring-no-properties pos (point-max))))
            (goto-char pos)
            (cl-letf (((symbol-function 'efrit-agent--media-image) (lambda (_f) '(image :type xbm :width 1 :height 1 :data "\0")))
                      ((symbol-function 'efrit-media-open) (lambda (f) (setq played f))))
              (efrit-agent-media-toggle)
              (should-not played)
              (let* ((p2 (text-property-any (point-min) (point-max) 'efrit-type 'media-note))
                     (text (buffer-substring-no-properties p2 (point-max))))
                (should (get-text-property p2 'efrit-media-open))
                (should (string-match-p "1 s, " text))
                (should (text-property-not-all p2 (point-max) 'display nil)))
              (efrit-agent-media-open)
              (should (equal video played)))))))))

(ert-deftest test-media-note-renders-in-the-agent-buffer ()
  "A media note puts the file's line in the agent buffer with the file as
a text property and the media keymap; the image is drawn when the
display can (batch: no image, the name shows)."
  (require 'efrit-agent)
  (test-media--with
    (let ((efrit-agent-auto-show nil)
          (file (efrit-media-generate-sync "a cat" :kind 'image)))
      (with-current-buffer (efrit-agent--get-buffer)
        (let ((inhibit-read-only t)) (erase-buffer))
        (efrit-agent-mode)
        (efrit-agent--init-regions)
        (efrit-agent--setup-regions)
        (efrit-tool-media-show file 'image "a cat" nil)
        (let ((pos (text-property-any (point-min) (point-max) 'efrit-type 'media-note)))
          (should pos)
          (should (equal file (get-text-property pos 'efrit-media-file)))
          (should (eq efrit-agent-media-map (get-text-property pos 'keymap)))
          (should (string-match-p (concat "image: " (regexp-quote (file-name-nondirectory file)))
                                  (buffer-substring-no-properties pos (point-max))))
          ;; folded by default: one line, no image
          (should-not (get-text-property pos 'efrit-media-open))
          (goto-char pos)
          (efrit-agent-media-copy-path)
          (should (equal file (current-kill 0)))
          ;; toggling redraws the note open (the image itself needs a
          ;; display; the state and the keys are what batch can check)
          (cl-letf (((symbol-function 'efrit-agent--media-image)
                     (lambda (_file) '(image :type xbm :width 1 :height 1 :data "\0"))))
            (efrit-agent-media-toggle)
            (let ((p2 (text-property-any (point-min) (point-max) 'efrit-type 'media-note)))
              (should (get-text-property p2 'efrit-media-open))
              (should (text-property-any p2 (point-max) 'display (get-text-property p2 'display)))
              (should (string-match-p "▼\\|v" (buffer-substring-no-properties p2 (+ p2 4))))
              (efrit-agent-media-toggle)
              (should-not (get-text-property (text-property-any (point-min) (point-max) 'efrit-type 'media-note)
                                             'efrit-media-open)))))))))

(provide 'test-media)
;;; test-media.el ends here
