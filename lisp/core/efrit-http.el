;;; efrit-http.el --- One JSON POST as a curl process, with cancel and a sync wait -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.17.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, comm

;;; Commentary:

;; The non-chat endpoints efrit talks to (image and video generation,
;; decisions) share one need: POST a JSON body, wait for a JSON
;; answer, do not block Emacs meanwhile, let the user cancel.  The
;; chat transport (efrit-api-stream) is built for server-sent events;
;; `url-retrieve' runs its callback inside a process filter, where a
;; prompt cannot read keys (the 2026-11-10 hang).  So: curl as a
;; process, headers through a private config file, the body through a
;; temp file, the callback from the sentinel.
;;
;; `efrit-http-post-json' is the asynchronous call; it returns an
;; `efrit-http-request' for `efrit-http-cancel'.  `efrit-http-post-json-sync'
;; waits with `accept-process-output' (Emacs stays responsive, C-g
;; cancels).  Authentication: by default efrit's own API headers
;; (`efrit-api-build-headers' with the configured key and scheme); an
;; auth-source host name for `efrit-auth'; or a header list.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-api)

(declare-function efrit-auth-authorization "efrit-auth" (host &optional user))

(defgroup efrit-http nil
  "The curl transport for efrit's non-chat endpoints."
  :group 'efrit
  :prefix "efrit-http-")

(defcustom efrit-http-curl-program "curl"
  "Program used for requests.  Must accept curl's arguments."
  :type 'string)

(defcustom efrit-http-timeout 600
  "Seconds a request may take before curl gives it up."
  :type 'integer)

(cl-defstruct (efrit-http-request (:constructor efrit-http-request--create))
  "One request in flight."
  url process body-file config-file started callback cancelled label)

(defvar efrit-http--active nil
  "The `efrit-http-request's in flight.")

(defun efrit-http-auth-headers (auth)
  "Headers that authenticate per AUTH.
nil: efrit's API key under its scheme and custom headers.  A string: an
auth-source host for `efrit-auth-authorization'.  A list: the headers
themselves ((NAME . VALUE) ...).  A function: called for the list."
  (cond
   ((null auth) (efrit-api-build-headers (efrit-common-get-api-key)))
   ((stringp auth)
    (require 'efrit-auth)
    (list (cons "authorization" (efrit-auth-authorization auth))))
   ((functionp auth) (funcall auth))
   ((listp auth) auth)
   (t (error "efrit-http: bad auth %S" auth))))

(defun efrit-http--write-config (headers)
  "HEADERS in curl config syntax, in a private temp file; return its path."
  (let ((file (make-temp-file "efrit-http-" nil ".cfg")))
    (with-temp-file file
      (dolist (h headers)
        (insert (format "header = %s\n" (prin1-to-string (format "%s: %s" (car h) (cdr h)))))))
    (set-file-modes file #o600)
    file))

(defun efrit-http--cleanup (request)
  "Delete REQUEST's temp files and drop it from the active list."
  (dolist (f (list (efrit-http-request-body-file request) (efrit-http-request-config-file request)))
    (when (and f (file-exists-p f)) (ignore-errors (delete-file f))))
  (setq efrit-http--active (delq request efrit-http--active)))

(defun efrit-http-error-message (response status)
  "A one-line reason from RESPONSE (an error body or curl's stderr) with exit STATUS."
  (or (ignore-errors
        (let* ((obj (json-parse-string response :object-type 'alist :array-type 'list
                                       :null-object nil :false-object nil))
               (err (alist-get 'error obj)))
          (cond ((and (listp err) (alist-get 'message err)))
                ((stringp err) err)
                ((alist-get 'message obj))
                ((alist-get 'detail obj)))))
      (let ((text (string-trim (or response ""))))
        (if (string-empty-p text)
            (format "curl exited %d" status)
          (truncate-string-to-width text 300 nil nil "…")))))

(cl-defun efrit-http-post-json (url body callback &key auth headers label timeout)
  "POST BODY (a Lisp object, JSON-encoded) to URL; CALLBACK gets the answer.
AUTH as in `efrit-http-auth-headers'; HEADERS extra ((NAME . VALUE) ...);
LABEL names the request in the log; TIMEOUT overrides `efrit-http-timeout'.
`efrit-api-request-transforms' apply, as to every efrit request.
CALLBACK is (lambda (TEXT ERROR)): TEXT the response body on success,
ERROR a string otherwise (\"cancelled\" after `efrit-http-cancel').
Returns the request."
  (let* ((all-headers (append '(("content-type" . "application/json")
                                ("accept" . "application/json"))
                              (efrit-http-auth-headers auth)
                              headers))
         (req (efrit-api-apply-transforms url all-headers body))
         (body-file (make-temp-file "efrit-http-body-" nil ".json"))
         (config (efrit-http--write-config (plist-get req :headers)))
         (request (efrit-http-request--create :url (plist-get req :url)
                                              :body-file body-file :config-file config
                                              :started (current-time) :callback callback
                                              :label (or label url)))
         (out (generate-new-buffer " *efrit-http*" t)))
    (with-temp-file body-file
      (set-buffer-multibyte nil)
      (insert (encode-coding-string (json-encode (plist-get req :body)) 'utf-8)))
    (set-file-modes body-file #o600)
    (efrit-log 'debug "http: POST %s" (efrit-http-request-label request))
    (let* ((default-directory (if (file-directory-p default-directory)
                                  default-directory
                                temporary-file-directory))
           (proc (make-process
                  :name "efrit-http"
                  :buffer out
                  :command (list efrit-http-curl-program
                                 "--silent" "--show-error" "--fail-with-body"
                                 "--max-time" (number-to-string (or timeout efrit-http-timeout))
                                 "--config" config
                                 "--data-binary" (concat "@" body-file)
                                 "--request" "POST"
                                 (plist-get req :url))
                  :connection-type 'pipe
                  :coding 'utf-8-unix
                  :noquery t
                  :stderr nil
                  :sentinel (lambda (p _event)
                              (unless (process-live-p p)
                                (let ((response (if (buffer-live-p out)
                                                    (with-current-buffer out (buffer-string))
                                                  "")))
                                  (when (buffer-live-p out) (kill-buffer out))
                                  (efrit-http--finish request (process-exit-status p) response)))))))
      (setf (efrit-http-request-process request) proc)
      (push request efrit-http--active)
      request)))

(defun efrit-http--finish (request status response)
  "Deliver REQUEST's outcome: curl exit STATUS and RESPONSE text."
  (let ((callback (efrit-http-request-callback request))
        (seconds (float-time (time-since (efrit-http-request-started request)))))
    (efrit-http--cleanup request)
    (condition-case err
        (cond
         ((efrit-http-request-cancelled request)
          (efrit-log 'info "http: %s cancelled after %.1fs" (efrit-http-request-label request) seconds)
          (funcall callback nil "cancelled"))
         ((not (zerop status))
          (let ((msg (efrit-http-error-message response status)))
            (efrit-log 'warn "http: %s failed after %.1fs: %s" (efrit-http-request-label request) seconds msg)
            (funcall callback nil msg)))
         (t
          (efrit-log 'debug "http: %s ok in %.1fs, %d bytes" (efrit-http-request-label request)
                     seconds (length response))
          (funcall callback response nil)))
      (error (efrit-log 'warn "http: callback for %s signalled: %s" (efrit-http-request-label request)
                        (error-message-string err))))))

(defun efrit-http-cancel (&optional request)
  "Cancel REQUEST, or every request in flight when nil.  Returns how many."
  (interactive)
  (let ((requests (if request (list request) (copy-sequence efrit-http--active))))
    (dolist (r requests)
      (setf (efrit-http-request-cancelled r) t)
      (when-let* ((p (efrit-http-request-process r)))
        (when (process-live-p p)
          ;; SIGINT first (curl exits cleanly), SIGTERM a second later
          ;; for a process that had not set up its handlers yet
          (interrupt-process p)
          (run-at-time 1 nil (lambda () (when (process-live-p p) (signal-process p 'SIGTERM))))
          (run-at-time 2 nil (lambda () (when (process-live-p p) (delete-process p)))))))
    (when (called-interactively-p 'any)
      (message "efrit-http: %d request%s cancelled" (length requests) (if (= 1 (length requests)) "" "s")))
    (length requests)))

(defun efrit-http-wait-while (pred &optional on-quit)
  "Wait while PRED returns non-nil, keeping Emacs alive; C-g runs ON-QUIT.
`sit-for' handles keyboard, redisplay, timers and every process, so
the spinner turns and other buffers work; `accept-process-output' on
one process did none of that and Emacs looked frozen for the length
of an image generation (2026-11-10).  A keystroke ends a `sit-for'
early, so the loop polls at short intervals."
  (condition-case nil
      (while (funcall pred)
        (unless (sit-for 0.1 t)
          ;; input is pending: let the command loop have it, then look again
          (accept-process-output nil 0.05)))
    (quit (when on-quit (funcall on-quit))
          (signal 'quit nil))))

(defun efrit-http-wait (request)
  "Wait for REQUEST to finish; Emacs keeps processing; C-g cancels it.
Returns nothing: the callback has run by the time this returns."
  (efrit-http-wait-while (lambda () (memq request efrit-http--active))
                         (lambda () (efrit-http-cancel request))))

(cl-defun efrit-http-post-json-sync (url body &rest args)
  "Like `efrit-http-post-json' but wait; return the response text or signal.
ARGS are its keyword arguments."
  (let ((result nil) (err nil))
    (efrit-http-wait
     (apply #'efrit-http-post-json url body
            (lambda (text e) (setq result text err e))
            args))
    (if err (error "%s" err) result)))

(provide 'efrit-http)

;;; efrit-http.el ends here
