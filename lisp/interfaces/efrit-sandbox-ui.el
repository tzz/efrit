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
;;   granted scope it opens a transient menu (`efrit-sandbox-ask')
;;   that names the tool, what it wants, and the exact scope each
;;   answer would grant:
;;
;;       o  once      this one operation
;;       s  session   until this Emacs exits
;;       p  project   saved in <project>/.efrit/sandbox.json
;;       n  no        deny (C-g and q do the same)
;;       ?  details   the full request and the grants in force
;;
;;   The menu runs inside a `recursive-edit' so the calling tool gets
;;   an answer synchronously; `transient-post-exit-hook' exits the
;;   recursive edit once the menu closes for any reason.  Without a
;;   display (batch, no transient) it falls back to `read-char-choice'.
;;
;;   The grant is as narrow as the request: allowing a write to
;;   ~/notes/todo.org for the project grants write under ~/notes/, not
;;   under ~.  The menu says exactly what will be granted.
;;
;; - A denial is not the end of the turn.  The model gets a tool
;;   result saying the access was refused and carries on without it.
;;
;; - `M-x efrit-sandbox' lists every grant in force for the current
;;   project in a tabulated-list with `d' to revoke, `g' to refresh,
;;   `s' to add a grant by hand.
;;
;; - The agent buffer gets a one-line record of each grant and denial,
;;   so reading back a transcript shows where the fence moved.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'efrit-sandbox)
(require 'efrit-sandbox-store)

(defvar transient-post-exit-hook)
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

(defvar efrit-sandbox-ui--request nil
  "The request the open `efrit-sandbox-ask' menu is about.")

(defvar efrit-sandbox-ui--answer 'pending
  "Answer chosen in the menu: once/session/project/nil, or `pending'.")

(defvar efrit-sandbox-ui--depth nil
  "Recursion depth of the recursive edit waiting on the menu, or nil.")

(defun efrit-sandbox-ui--menu-description ()
  "Header of the menu: tool, request, detail."
  (let* ((req efrit-sandbox-ui--request)
         (tool (or (efrit-sandbox-request-tool req) "a tool"))
         (detail (efrit-sandbox-request-detail req)))
    (concat
     (propertize (format "Efrit: %s wants to %s" tool (efrit-sandbox-ui--scope-word req))
                 'face 'efrit-sandbox-prompt-face)
     (when (and detail (not (string-empty-p detail)))
       (concat "\n  " (truncate-string-to-width
                       (replace-regexp-in-string "\n" " " detail) (- (frame-width) 6) nil nil "…")))
     "\n")))

(defun efrit-sandbox-ui--project-label ()
  (format "this project (%s, saved)"
          (abbreviate-file-name (directory-file-name (efrit-sandbox-project-root)))))

(defun efrit-sandbox-ui--choose (answer)
  (setq efrit-sandbox-ui--answer answer))

(defun efrit-sandbox-ui--exit-recursive-edit ()
  "Leave the recursive edit that waits on the menu, if we are in it."
  (when (and efrit-sandbox-ui--depth
             (= (recursion-depth) efrit-sandbox-ui--depth))
    (exit-recursive-edit)))

(defun efrit-sandbox-ui--define-menu ()
  "Define `efrit-sandbox-ask' if transient is available.  Return non-nil on success."
  (when (require 'transient nil t)
    (unless (fboundp 'efrit-sandbox-ask)
      (eval
       '(transient-define-prefix efrit-sandbox-ask ()
          "Allow the sandbox request?"
          [:description efrit-sandbox-ui--menu-description
           ["Allow"
            ("o" "once" (lambda () (interactive) (efrit-sandbox-ui--choose 'once)))
            ("s" "for this Emacs session" (lambda () (interactive) (efrit-sandbox-ui--choose 'session)))
            ("p" (lambda () (interactive) (efrit-sandbox-ui--choose 'project))
             :description efrit-sandbox-ui--project-label)]
           ["Refuse"
            ("n" "no, the model continues without it"
             (lambda () (interactive) (efrit-sandbox-ui--choose nil)))]
           ["More"
            ("?" "show the full request" (lambda () (interactive) (efrit-sandbox-ui--show-details efrit-sandbox-ui--request))
             :transient t)
            ("l" "grants in force" (lambda () (interactive) (efrit-sandbox (efrit-sandbox-project-root)))
             :transient t)]])
       t))
    (fboundp 'efrit-sandbox-ask)))

(defun efrit-sandbox-ui--ask-with-menu (req)
  "Open the transient menu for REQ and wait for an answer.
Returns once/session/project or nil.  Closing the menu any other way
\(C-g, q, another command) is a denial."
  (setq efrit-sandbox-ui--request req
        efrit-sandbox-ui--answer 'pending)
  (let ((efrit-sandbox-ui--depth (1+ (recursion-depth))))
    (unwind-protect
        (progn
          (add-hook 'transient-post-exit-hook #'efrit-sandbox-ui--exit-recursive-edit)
          ;; Open the menu from the command loop, not from inside the
          ;; tool's call stack: transient needs to be a command.
          (run-at-time 0 nil (lambda () (call-interactively #'efrit-sandbox-ask)))
          (condition-case nil
              (recursive-edit)
            (quit nil)))
      (remove-hook 'transient-post-exit-hook #'efrit-sandbox-ui--exit-recursive-edit)
      (setq efrit-sandbox-ui--request nil)))
  (if (eq efrit-sandbox-ui--answer 'pending) nil efrit-sandbox-ui--answer))

(defun efrit-sandbox-ui--ask-in-echo-area (req)
  "Fallback prompt in the echo area when no menu can be shown."
  (let* ((tool (or (efrit-sandbox-request-tool req) "a tool"))
         (header (format "Efrit (%s) wants to %s\n" tool (efrit-sandbox-ui--scope-word req)))
         (legend (format "[o]nce  [s]ession  [p]roject %s  [n]o  [?]details "
                         (abbreviate-file-name (efrit-sandbox-project-root)))))
    (catch 'decided
      (while t
        (pcase (read-char-choice (concat header legend) '(?o ?s ?p ?n ??))
          (?o (throw 'decided 'once))
          (?s (throw 'decided 'session))
          (?p (throw 'decided 'project))
          (?n (throw 'decided nil))
          (?? (efrit-sandbox-ui--show-details req)))))))

(defun efrit-sandbox-ui-use-menu-p ()
  "Non-nil when the transient menu can be used for the prompt."
  (and (not noninteractive)
       (efrit-sandbox-ui--define-menu)))

(defun efrit-sandbox-ui-prompt (req)
  "Ask the user about REQ; return `once', `session', `project', or nil."
  (let* ((tool (or (efrit-sandbox-request-tool req) "a tool"))
         (what (efrit-sandbox-ui--scope-word req)))
    (efrit-sandbox-ui--note (format "%s asks to %s" tool what) 'efrit-sandbox-prompt-face)
    (let ((answer (if (efrit-sandbox-ui-use-menu-p)
                      (efrit-sandbox-ui--ask-with-menu req)
                    (efrit-sandbox-ui--ask-in-echo-area req))))
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
