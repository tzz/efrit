;;; efrit-agent-tab-line.el --- The agents in every window's tab line, lit when they need you -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.12.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai, convenience

;;; Commentary:

;; `efrit-agent-tab-line-mode' puts the live agent buffers at the end
;; of every window's tab line, after that window's own buffers, each
;; as a pill in its state colour: amber while working, green idle,
;; violet with a dot when a reply is unread, and red, saturated, when
;; a prompt waits for you.  Clicking one shows that agent (tzz,
;; 2026-10-14: "a tab buffer line display of the active agents in
;; addition to normal buffers, and add a highlight to them if they are
;; waiting for me").
;;
;; It works with any tab-line setup: the agent tabs are appended by
;; filtering the result of `tab-line-tabs-function', and only the
;; agent tabs are drawn here; every other tab still goes through the
;; user's `tab-line-tab-name-format-function'.  A user who draws their
;; own tabs can instead call `efrit-agent-tab-line-pill' from their
;; formatter for the tabs where `efrit-agent-tab-line-agent-p' is
;; true, and `efrit-agent-tab-line-state' to colour them.  The
;; attention state is part of the tab-line cache key, and an
;; `attention-changed' event forces a redraw, so the pill changes the
;; moment a prompt opens.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'tab-line)
(require 'efrit-events)
(require 'efrit-pill)
(require 'efrit-agent-ring)

(declare-function efrit-agent-buffers "efrit-agent-core")
(declare-function efrit-agent-display "efrit-agent-core")
(declare-function efrit-repl-session-status "efrit-repl-session")
(defvar efrit-agent--repl-session)
(defvar efrit-agent--instance)

(defgroup efrit-agent-tab-line nil
  "Agent buffers in the tab line."
  :group 'efrit-agent-ring)

(defcustom efrit-agent-tab-line-exclude-agent-windows t
  "When non-nil, a window showing an agent buffer lists only the agents.
Its own buffer history is the agent conversation; the other agents
are what you switch between there."
  :type 'boolean
  :group 'efrit-agent-tab-line)

(defcustom efrit-agent-tab-line-label-function #'efrit-agent-tab-line-label-default
  "Function from an agent buffer to the short text on its tab."
  :type 'function
  :group 'efrit-agent-tab-line)

;;;; State

(defun efrit-agent-tab-line-agent-p (tab)
  "Non-nil when TAB (a buffer or a tab alist) is a live agent buffer."
  (and (bufferp tab) (buffer-live-p tab)
       (with-current-buffer tab (derived-mode-p 'efrit-agent-mode))))

(defun efrit-agent-tab-line-state (buffer)
  "BUFFER's state for colouring: `waiting', `unread', `working', `failed' or `idle'."
  (with-current-buffer buffer
    (cond
     (efrit-agent--waiting 'waiting)
     (efrit-agent--unread 'unread)
     ((and efrit-agent--repl-session
           (memq (efrit-repl-session-status efrit-agent--repl-session) '(working)))
      'working)
     ((and efrit-agent--repl-session
           (eq (efrit-repl-session-status efrit-agent--repl-session) 'failed))
      'failed)
     (t 'idle))))

(defun efrit-agent-tab-line-label-default (buffer)
  "\"proj\" or \"proj:name\" from the buffer name, without the *efrit[ ]* wrap."
  (let ((name (buffer-name buffer)))
    (if (string-match "\\`\\*efrit\\[\\(.*\\)\\]\\*\\'" name) (match-string 1 name)
      (string-trim name "\\*+" "\\*+"))))

;;;; Drawing

(defun efrit-agent-tab-line-pill (buffer &optional selected)
  "The pill for agent BUFFER: label, state colour, strong when waiting or SELECTED."
  (let* ((state (efrit-agent-tab-line-state buffer))
         (label (funcall efrit-agent-tab-line-label-function buffer))
         (text (pcase state
                 ('waiting (concat "? " label))
                 ('unread (concat "• " label))
                 ('working (concat "… " label))
                 (_ label)))
         (help (pcase state
                 ('waiting (format "%s is waiting for you: %s" (buffer-name buffer)
                                   (buffer-local-value 'efrit-agent--waiting buffer)))
                 ('unread (format "%s has a reply you have not read" (buffer-name buffer)))
                 ('working (format "%s is working" (buffer-name buffer)))
                 (_ (format "%s (%s)" (buffer-name buffer) state)))))
    (efrit-pill text :state state :strong (or selected (eq state 'waiting))
                :scale (if selected 1.1 efrit-pill-scale) :help-echo help)))

(defun efrit-agent-tab-line--format (orig tab tabs)
  "Draw agent tabs as pills; hand every other TAB to ORIG."
  (if (not (efrit-agent-tab-line-agent-p tab))
      (funcall orig tab tabs)
    (let ((selected (eq tab (window-buffer))))
      (apply #'propertize (efrit-agent-tab-line-pill tab selected)
             `(tab ,tab
               ,@(if selected '(selected t))
               keymap ,tab-line-tab-map
               follow-link ignore
               mouse-face tab-line-highlight)))))

;;;; Which tabs

(defun efrit-agent-tab-line--tabs (tabs)
  "TABS with the live agent buffers appended, in ring order, no duplicates.
In a window showing an agent, only the agents when
`efrit-agent-tab-line-exclude-agent-windows'."
  (let ((agents (efrit-agent-ring)))
    (cond
     ((null agents) tabs)
     ((and efrit-agent-tab-line-exclude-agent-windows
           (efrit-agent-tab-line-agent-p (window-buffer)))
      agents)
     (t (append (seq-remove #'efrit-agent-tab-line-agent-p tabs) agents)))))

(defun efrit-agent-tab-line--cache-key (keys)
  "KEYS plus the agents' states, so a state change redraws."
  (append keys (list (mapcar (lambda (b) (cons b (efrit-agent-tab-line-state b))) (efrit-agent-ring)))))

(defun efrit-agent-tab-line--refresh (&rest _)
  (when (fboundp 'tab-line-force-update)
    (tab-line-force-update t)))

;;;; Mode

;;;###autoload
(define-minor-mode efrit-agent-tab-line-mode
  "Show the agent buffers at the end of every tab line, lit when they need you.
Works with the tab-line setup you have: it appends the agents to what
`tab-line-tabs-function' returns and draws only those; `tab-line-mode'
itself is yours to turn on (`global-tab-line-mode')."
  :global t
  :group 'efrit-agent-tab-line
  (cond
   (efrit-agent-tab-line-mode
    (add-function :filter-return tab-line-tabs-function #'efrit-agent-tab-line--tabs)
    (add-function :around tab-line-tab-name-format-function #'efrit-agent-tab-line--format)
    (add-function :filter-return tab-line-cache-key-function #'efrit-agent-tab-line--cache-key)
    (efrit-subscribe 'attention-changed #'efrit-agent-tab-line--refresh)
    (efrit-subscribe 'status #'efrit-agent-tab-line--refresh)
    (add-hook 'efrit-agent-mode-hook #'efrit-agent-tab-line--refresh)
    (add-hook 'kill-buffer-hook #'efrit-agent-tab-line--on-kill))
   (t
    (remove-function tab-line-tabs-function #'efrit-agent-tab-line--tabs)
    (remove-function tab-line-tab-name-format-function #'efrit-agent-tab-line--format)
    (remove-function tab-line-cache-key-function #'efrit-agent-tab-line--cache-key)
    (efrit-unsubscribe 'attention-changed #'efrit-agent-tab-line--refresh)
    (efrit-unsubscribe 'status #'efrit-agent-tab-line--refresh)
    (remove-hook 'efrit-agent-mode-hook #'efrit-agent-tab-line--refresh)
    (remove-hook 'kill-buffer-hook #'efrit-agent-tab-line--on-kill)))
  (efrit-agent-tab-line--refresh))

(defun efrit-agent-tab-line--on-kill ()
  (when (derived-mode-p 'efrit-agent-mode)
    (run-at-time 0 nil #'efrit-agent-tab-line--refresh)))

(provide 'efrit-agent-tab-line)

;;; efrit-agent-tab-line.el ends here
