;;; efrit-sandbox-ui.el --- Prompt, list and revoke sandbox grants -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; The interactive side of efrit-sandbox:
;;
;; - `efrit-sandbox-ui-prompt' is installed as
;;   `efrit-sandbox-request-function'.  When a tool needs more than the
;;   granted scope it shows, in the echo area (and as a line in the
;;   agent buffer so the transcript records it), what is being asked
;;   and for what, and offers:
;;
;;       [o]nce  [s]ession  [p]roject  [n]o  [?]details
;;
;;   o    this one operation
;;   s    until this Emacs exits
;;   p    saved in <project>/.efrit/sandbox.json for next time
;;   n    deny; the turn ends and the model is told why
;;
;;   The grant is as narrow as the request: allowing a write to
;;   ~/notes/todo.org for the project grants write under ~/notes/, not
;;   under ~.  The request line says exactly what will be granted.
;;
;; - `M-x efrit-sandbox' lists every grant in force for the current
;;   project (project ones marked as saved) in a tabulated-list with
;;   `d' to revoke, `g' to refresh, `s' to add a grant by hand.
;;
;; - The agent buffer gets a one-line record of each grant and denial,
;;   so reading back a transcript shows where the fence moved.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'efrit-sandbox)
(require 'efrit-sandbox-store)

(declare-function efrit-agent--append-to-conversation "efrit-agent-core")
(declare-function efrit-show-preview "efrit-ui-helpers")
(defvar efrit-agent-buffer-name)

(defgroup efrit-sandbox-ui nil
  "Sandbox prompts and listing."
  :group 'efrit-sandbox)

(defface efrit-sandbox-prompt-face
  '((t :inherit warning :weight bold))
  "Face for the sandbox request line in the agent buffer.")

(defface efrit-sandbox-grant-face
  '((t :inherit success))
  "Face for a recorded grant in the agent buffer.")

(defface efrit-sandbox-deny-face
  '((t :inherit error))
  "Face for a recorded denial in the agent buffer.")

;;; Transcript notes

(defun efrit-sandbox-ui--note (text face)
  "Append a one-line TEXT in FACE to the agent conversation, if it exists."
  (when (and (boundp 'efrit-agent-buffer-name)
             (get-buffer efrit-agent-buffer-name)
             (fboundp 'efrit-agent--append-to-conversation))
    (with-current-buffer efrit-agent-buffer-name
      (efrit-agent--append-to-conversation
       (concat (propertize (concat "  ⛨ " text) 'face face) "\n")
       (list 'efrit-type 'sandbox-note)))))

;;; The prompt

(defun efrit-sandbox-ui--scope-word (req)
  "What the grant would cover, in the user's terms."
  (let ((cap (efrit-sandbox-request-cap req))
        (target (efrit-sandbox-request-target req)))
    (pcase cap
      ('read (format "read files under %s" (abbreviate-file-name target)))
      ('write (format "write files under %s" (abbreviate-file-name target)))
      ('elisp "evaluate Emacs Lisp (each file/process it touches is still checked)")
      ('shell "run shell commands (a single grant: any command)")
      ('net "make network requests")
      (_ (format "%s %s" cap target)))))

(defun efrit-sandbox-ui-prompt (req)
  "Ask the user about REQ; return `once', `session', `project', or nil."
  (let* ((tool (or (efrit-sandbox-request-tool req) "a tool"))
         (detail (efrit-sandbox-request-detail req))
         (what (efrit-sandbox-ui--scope-word req))
         (root (abbreviate-file-name (efrit-sandbox-project-root)))
         (header (format "Efrit (%s) wants to %s%s\n" tool what
                         (if (and detail (not (string-empty-p detail)))
                             (format "\n  for: %s" (truncate-string-to-width detail 100 nil nil "…"))
                           "")))
         (legend (format "[o]nce  [s]ession  [p]roject %s  [n]o  [?]details " root)))
    (efrit-sandbox-ui--note (format "%s asks to %s" tool what) 'efrit-sandbox-prompt-face)
    (let ((answer
           (catch 'decided
             (while t
               (pcase (read-char-choice (concat header legend) '(?o ?s ?p ?n ??))
                 (?o (throw 'decided 'once))
                 (?s (throw 'decided 'session))
                 (?p (throw 'decided 'project))
                 (?n (throw 'decided nil))
                 (?? (efrit-sandbox-ui--show-details req)))))))
      (efrit-sandbox-ui--note
       (pcase answer
         ('once (format "granted once: %s" what))
         ('session (format "granted for this session: %s" what))
         ('project (format "granted for this project (saved): %s" what))
         (_ (format "denied: %s" what)))
       (if answer 'efrit-sandbox-grant-face 'efrit-sandbox-deny-face))
      answer)))

(defun efrit-sandbox-ui--show-details (req)
  "Pop up everything known about REQ."
  (require 'efrit-ui-helpers)
  (efrit-show-preview
   "*efrit-sandbox-request*"
   (format "Tool:       %s\nCapability: %s\nTarget:     %s\nProject:    %s\nDetail:\n%s\n\nGrants in force:\n%s"
           (efrit-sandbox-request-tool req)
           (efrit-sandbox-request-cap req)
           (efrit-sandbox-request-target req)
           (efrit-sandbox-project-root)
           (or (efrit-sandbox-request-detail req) "(none)")
           (efrit-sandbox-ui--grants-text))))

(defun efrit-sandbox-ui--grants-text ()
  (let ((gs (efrit-sandbox-grants)))
    (if (null gs) "  (only the default: read inside the project root)"
      (mapconcat (lambda (g) (format "  %-6s %-8s %s" (plist-get g :cap) (plist-get g :scope)
                                     (let ((tg (plist-get g :target)))
                                       (if (eq tg t) "" (abbreviate-file-name tg)))))
                 gs "\n"))))

;; Install as the default prompt
(unless efrit-sandbox-request-function
  (setq efrit-sandbox-request-function #'efrit-sandbox-ui-prompt))

;;; Listing

(defvar efrit-sandbox-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'efrit-sandbox-list-revoke)
    (define-key map (kbd "s") #'efrit-sandbox-list-add)
    (define-key map (kbd "g") #'efrit-sandbox-list-refresh)
    map))

(define-derived-mode efrit-sandbox-list-mode tabulated-list-mode "Efrit-Sandbox"
  "List of sandbox grants for the current efrit project."
  (setq tabulated-list-format [("Cap" 7 t) ("Scope" 9 t) ("Target" 60 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defvar-local efrit-sandbox-list--root nil)

(defun efrit-sandbox-list--entries (root)
  (let ((i 0))
    (append
     (list (list (cl-incf i)
                 (vector "read" "default" (abbreviate-file-name root))))
     (mapcar (lambda (g)
               (list (cons (cl-incf i) g)
                     (vector (symbol-name (plist-get g :cap))
                             (symbol-name (plist-get g :scope))
                             (let ((tg (plist-get g :target)))
                               (if (eq tg t) "—" (abbreviate-file-name tg))))))
             (efrit-sandbox-grants root)))))

(defun efrit-sandbox-list-refresh ()
  "Re-read grants for the buffer's project."
  (interactive)
  (efrit-sandbox-store-forget efrit-sandbox-list--root)
  (efrit-sandbox-store-ensure-loaded efrit-sandbox-list--root)
  (setq tabulated-list-entries (efrit-sandbox-list--entries efrit-sandbox-list--root))
  (tabulated-list-print t))

(defun efrit-sandbox-list-revoke ()
  "Revoke the grant at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if (not (consp id))
        (user-error "The default project read cannot be revoked here; set efrit-sandbox-default-project-grants")
      (let ((g (cdr id)))
        (when (y-or-n-p (format "Revoke %s %s? " (plist-get g :cap)
                                (let ((tg (plist-get g :target))) (if (eq tg t) "" tg))))
          (efrit-sandbox-revoke (plist-get g :cap) (plist-get g :target) efrit-sandbox-list--root)
          (efrit-sandbox-list-refresh))))))

(defun efrit-sandbox-list-add ()
  "Add a grant by hand."
  (interactive)
  (let* ((cap (intern (completing-read "Capability: " '("read" "write" "elisp" "shell" "net") nil t)))
         (target (if (memq cap '(read write))
                     (efrit-sandbox-canonical (read-directory-name "Directory: " efrit-sandbox-list--root))
                   t))
         (scope (intern (completing-read "Scope: " '("session" "project") nil t "session"))))
    (efrit-sandbox-grant cap target scope efrit-sandbox-list--root)
    (efrit-sandbox-list-refresh)))

;;;###autoload
(defun efrit-sandbox (&optional root)
  "Show the sandbox grants for the current project (or ROOT).
d revokes the grant at point, s adds one, g refreshes."
  (interactive)
  (let ((root (or root (efrit-sandbox-project-root))))
    (with-current-buffer (get-buffer-create "*efrit-sandbox*")
      (efrit-sandbox-list-mode)
      (setq efrit-sandbox-list--root root)
      (setq header-line-format (format " Sandbox for %s   (d revoke, s add, g refresh)"
                                       (abbreviate-file-name root)))
      (efrit-sandbox-list-refresh)
      (pop-to-buffer (current-buffer)))))

(provide 'efrit-sandbox-ui)

;;; efrit-sandbox-ui.el ends here
