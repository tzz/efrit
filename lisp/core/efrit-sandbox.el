;;; efrit-sandbox.el --- Scope-based capability sandbox -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; efrit may not read, write, or execute anything the user has not
;; granted.  Grants are *scopes*, decided per project:
;;
;;   read   PREFIX   read files/dirs under PREFIX (search, vcs, read_file)
;;   write  PREFIX   create/edit/delete files under PREFIX, save buffers
;;   exec   elisp    evaluate Lisp (eval_sexp)
;;   exec   shell    run shell commands (one grant; see below)
;;   exec   net      fetch URLs / web search
;;
;; A PREFIX is a directory (or file) path.  It includes the Tramp
;; remote identity, so /ssh:host:/proj is a different scope from
;; /proj.  The project root gets `read' by default and nothing else.
;;
;; When a tool needs more than the current scope allows, it does not
;; run.  `efrit-sandbox-check' signals `efrit-sandbox-denied' carrying
;; a *request* -- the minimal grant that would have let it proceed.
;; The UI (efrit-sandbox-ui) turns that into a prompt offering the
;; grant once / for this session / for this project.  Project grants
;; are persisted as JSON in .efrit/sandbox.json (efrit-sandbox-store);
;; `.efrit/' itself is a hard write deny for every path, including
;; eval_sexp.
;;
;; shell is deliberately a single grant: any shell command can escalate
;; to any other, so pretending to restrict "git" but not "sh" is
;; theatre.  Most of what an agent wants from a shell is available
;; through Emacs primitives, which *are* checked.
;;
;; Pure Executor: this is security filtering, which ARCHITECTURE.md
;; allows.  No task logic lives here.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-log)
(require 'efrit-tool-utils)   ; efrit-project-root, efrit-tool--get-project-root

(declare-function efrit-sandbox-store-save "efrit-sandbox-store")
(declare-function efrit-sandbox-store-ensure-loaded "efrit-sandbox-store")

(defgroup efrit-sandbox nil
  "Scope-based sandbox for tool access."
  :group 'efrit
  :prefix "efrit-sandbox-")

(defcustom efrit-sandbox-enabled t
  "When non-nil, every tool access is checked against the granted scope.
nil restores the pre-0.5 prefix check only (efrit-project-sandbox)."
  :type 'boolean
  :group 'efrit-sandbox)

(defcustom efrit-sandbox-default-project-grants '(read)
  "Capabilities granted on the project root without asking.
A list drawn from `read', `write', `elisp', `shell', `net'.  The
default lets the model read the project it was invoked in; anything
else asks."
  :type '(set (const read) (const write) (const elisp) (const shell) (const net))
  :group 'efrit-sandbox)

(defcustom efrit-sandbox-always-deny
  '("\\.efrit/" "\\.ssh/" "\\.gnupg/" "\\.authinfo" "\\.netrc" "\\.password-store/"
    "id_rsa" "id_ed25519" "id_ecdsa" "\\.pem\\'" "\\.key\\'")
  "Regexps matched against any path; a match is denied regardless of grants.
`.efrit/' holds the sandbox's own persisted state and must never be
model-writable."
  :type '(repeat regexp)
  :group 'efrit-sandbox)

;;; Errors

(define-error 'efrit-sandbox-denied "Sandbox: access denied")

;;; Grant representation
;;
;; A grant is a plist (:cap CAP :target TARGET :scope SCOPE) where
;;   CAP    ∈ read write elisp shell net
;;   TARGET a canonical directory/file path for read/write, or t
;;   SCOPE  ∈ once session project
;; Session and once grants live in memory; project grants are what
;; efrit-sandbox-store persists.  Lookup: a request (cap target) is satisfied by
;; any grant with the same cap whose target is t or a prefix of the
;; request target.

(cl-defstruct (efrit-sandbox-request (:constructor efrit-sandbox-request-create))
  cap target tool detail)

(defvar efrit-sandbox--session-grants (make-hash-table :test 'equal)
  "Project root -> list of grant plists valid for this Emacs session.")

(defvar efrit-sandbox--once-grant nil
  "A grant that applies to the very next check only.")

(defvar efrit-sandbox--project-grants (make-hash-table :test 'equal)
  "Project root -> list of grant plists loaded from the project's store.")

(defvar efrit-sandbox-request-function nil
  "Function called with an `efrit-sandbox-request' when a check fails.
It must return a scope symbol (`once' `session' `project') to grant
the request at that scope, or nil to deny.  efrit-sandbox-ui sets
this to an interactive prompt; nil means every failure is a denial.")

;;; Canonical paths

(defun efrit-sandbox-canonical (path)
  "Canonical form of PATH for prefix comparison.
Expanded, symlinks resolved where the path exists, directories with a
trailing slash.  Remote identity is preserved."
  (let* ((expanded (expand-file-name path))
         (resolved (if (file-exists-p expanded)
                       (condition-case nil (file-truename expanded) (error expanded))
                     ;; resolve the deepest existing parent so a new
                     ;; file under a symlinked dir canonicalizes right
                     (let* ((dir (file-name-directory expanded))
                            (base (file-name-nondirectory expanded)))
                       (if (and dir (not (equal dir expanded)) (file-exists-p dir))
                           (concat (file-name-as-directory
                                    (condition-case nil (file-truename dir) (error dir)))
                                   base)
                         expanded)))))
    (if (file-directory-p resolved)
        (file-name-as-directory resolved)
      resolved)))

(defun efrit-sandbox--under-p (path prefix)
  "Non-nil if canonical PATH is PREFIX or below it, on the same host."
  (and (equal (file-remote-p path) (file-remote-p prefix))
       (or (string-prefix-p (file-name-as-directory prefix) path)
           (string= path prefix)
           (string= (file-name-as-directory path) (file-name-as-directory prefix)))))

(defun efrit-sandbox-project-root ()
  "Canonical project root the sandbox is keyed on."
  (efrit-sandbox-canonical (efrit-tool--get-project-root)))

;;; Grant queries

(defun efrit-sandbox-grants (&optional root)
  "All grants in force for ROOT (default current project): project then session."
  (let ((root (or root (efrit-sandbox-project-root))))
    (append (gethash root efrit-sandbox--project-grants)
            (gethash root efrit-sandbox--session-grants))))

(defun efrit-sandbox--grant-covers-p (grant cap target)
  (and (eq (plist-get grant :cap) cap)
       (let ((gt (plist-get grant :target)))
         (or (eq gt t)
             (and (stringp target) (stringp gt)
                  (efrit-sandbox--under-p target gt))))))

(defun efrit-sandbox--always-denied-p (target)
  (and (stringp target)
       (cl-some (lambda (re) (string-match-p re target)) efrit-sandbox-always-deny)))

(defun efrit-sandbox-allowed-p (cap &optional target root)
  "Non-nil if CAP on TARGET is covered by the scope for ROOT, without asking."
  (let* ((root (or root (efrit-sandbox-project-root)))
         (target (if (stringp target) (efrit-sandbox-canonical target) target)))
    (cond
     ((efrit-sandbox--always-denied-p target) nil)
     ;; default project grants
     ((and (memq cap efrit-sandbox-default-project-grants)
           (or (memq cap '(elisp shell net))
               (and (stringp target) (efrit-sandbox--under-p target root))))
      t)
     ;; explicit grants
     ((cl-some (lambda (g) (efrit-sandbox--grant-covers-p g cap target))
               (efrit-sandbox-grants root))
      t)
     ;; the one-shot grant
     ((and efrit-sandbox--once-grant
           (efrit-sandbox--grant-covers-p efrit-sandbox--once-grant cap target))
      (setq efrit-sandbox--once-grant nil)
      t)
     (t nil))))

;;; Granting

(defun efrit-sandbox--suggest-target (cap target root)
  "The target a grant should carry for CAP on TARGET: narrow, not wide.
For a path outside the root, suggest its directory (so the next file
alongside is covered) but never anything above the user's home for
write."
  (cond
   ((memq cap '(elisp shell net)) t)
   ((not (stringp target)) t)
   ((efrit-sandbox--under-p target root) root)
   (t (let ((dir (if (directory-name-p target) target (file-name-directory target))))
        (if (and (eq cap 'write)
                 (let ((home (efrit-sandbox-canonical "~")))
                   (or (string= dir home) (string= dir "/"))))
            target                       ; a lone file in $HOME or /: just that file
          dir)))))

(defun efrit-sandbox-grant (cap target scope &optional root)
  "Record a grant of CAP on TARGET at SCOPE for ROOT.
SCOPE is `once', `session' or `project'.  Project grants are also
persisted via `efrit-sandbox-store-save'."
  (let* ((root (or root (efrit-sandbox-project-root)))
         (grant (list :cap cap :target target :scope scope)))
    (pcase scope
      ('once (setq efrit-sandbox--once-grant grant))
      ('session (push grant (gethash root efrit-sandbox--session-grants)))
      ('project
       (push grant (gethash root efrit-sandbox--project-grants))
       (efrit-sandbox-store-save root))
      (_ (error "Unknown grant scope %S" scope)))
    (efrit-log 'info "sandbox: granted %s %s (%s) for %s" cap target scope root)
    (when (fboundp 'efrit-publish)
      (efrit-publish 'sandbox-grant `((:cap . ,cap) (:target . ,target)
                                      (:scope . ,scope) (:root . ,root))))
    grant))

(defun efrit-sandbox-revoke (cap target &optional root)
  "Remove every session and project grant matching CAP and TARGET for ROOT."
  (let ((root (or root (efrit-sandbox-project-root))))
    (dolist (table (list efrit-sandbox--session-grants efrit-sandbox--project-grants))
      (puthash root
               (cl-remove-if (lambda (g) (and (eq (plist-get g :cap) cap)
                                              (equal (plist-get g :target) target)))
                             (gethash root table))
               table))
    (efrit-sandbox-store-save root)))

(defun efrit-sandbox-reset-session (&optional root)
  "Forget session grants for ROOT, or all projects when nil."
  (interactive)
  (if root
      (remhash root efrit-sandbox--session-grants)
    (clrhash efrit-sandbox--session-grants))
  (setq efrit-sandbox--once-grant nil))

;;; The check

(defun efrit-sandbox-check (cap target &optional tool detail)
  "Ensure CAP on TARGET is allowed, asking to widen the scope if not.
TOOL and DETAIL describe the caller for the prompt.  Returns t when
allowed.  Signals `efrit-sandbox-denied' with the request when the
user declines or no prompt function is installed; the data is the
`efrit-sandbox-request' struct.

With `efrit-sandbox-enabled' nil this is a no-op that returns t."
  (if (not efrit-sandbox-enabled)
      t
    (let* ((root (efrit-sandbox-project-root))
           (ctarget (if (stringp target) (efrit-sandbox-canonical target) target)))
      (efrit-sandbox-store-ensure-loaded root)
      (cond
       ((efrit-sandbox--always-denied-p ctarget)
        (efrit-log 'warn "sandbox: %s on %s is always denied" cap ctarget)
        (signal 'efrit-sandbox-denied
                (list (efrit-sandbox-request-create
                       :cap cap :target ctarget :tool tool
                       :detail (format "%s is protected and can never be granted" ctarget)))))
       ((efrit-sandbox-allowed-p cap ctarget root) t)
       (t
        (let* ((req (efrit-sandbox-request-create
                     :cap cap
                     :target (efrit-sandbox--suggest-target cap ctarget root)
                     :tool tool :detail detail))
               (scope (and efrit-sandbox-request-function
                           (condition-case err
                               (funcall efrit-sandbox-request-function req)
                             (quit nil)
                             (error
                              (efrit-log 'warn "sandbox request function: %s"
                                         (error-message-string err))
                              nil)))))
          (if (memq scope '(once session project))
              (progn
                (efrit-sandbox-grant cap (efrit-sandbox-request-target req) scope root)
                ;; The grant carries the suggested (possibly wider)
                ;; target, so the re-check passes; a once grant is
                ;; consumed by this re-check.
                (or (efrit-sandbox-allowed-p cap ctarget root)
                    (signal 'efrit-sandbox-denied (list req))))
            (efrit-log 'info "sandbox: denied %s %s (%s)" cap ctarget tool)
            (when (fboundp 'efrit-publish)
              (efrit-publish 'sandbox-denied `((:cap . ,cap) (:target . ,ctarget) (:tool . ,tool))))
            (signal 'efrit-sandbox-denied (list req)))))))))

(defun efrit-sandbox-describe-request (req)
  "One-line human description of REQ for prompts and tool results."
  (let ((cap (efrit-sandbox-request-cap req))
        (target (efrit-sandbox-request-target req)))
    (pcase cap
      ('read (format "read %s" target))
      ('write (format "write under %s" target))
      ('elisp "evaluate Emacs Lisp")
      ('shell "run shell commands")
      ('net "access the network")
      (_ (format "%s %s" cap target)))))

(defconst efrit-sandbox-denied-prefix "Error sandbox denied: "
  "Prefix of a denied tool result.  `efrit-loop' matches it to end the turn.")

(defun efrit-sandbox-denied-tool-result (req)
  "The tool_result text (after `efrit-sandbox-denied-prefix') for a denied REQ.
Says what was refused and that the turn is over, so the model asks
instead of retrying."
  (format "%s%s. The user declined to widen the sandbox; the turn ends here. Do not retry or route around it. If the task needs this, explain to the user exactly what access is required and why, and wait."
          (efrit-sandbox-describe-request req)
          (if (efrit-sandbox-request-detail req)
              (format " (%s)" (efrit-sandbox-request-detail req))
            "")))

(provide 'efrit-sandbox)

;; The store needs the tables above; load it after providing so its
;; (require 'efrit-sandbox) is satisfied without recursion.
(require 'efrit-sandbox-store)

;;; efrit-sandbox.el ends here
