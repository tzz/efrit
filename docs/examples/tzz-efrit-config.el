;;; tzz-efrit-config.el --- efrit against Anthropic / LiteLLM / Bifrost -*- lexical-binding: t; -*-

;; Example configuration selecting an LLM endpoint per environment.
;; Direct Anthropic is the default; LiteLLM and Bifrost are proxies
;; that expose the Anthropic Messages API behind a bearer token.
;;
;; Requires efrit with `efrit-api-auth-scheme' (tzz/wip branch).

;;; Backend selection ------------------------------------------------------

(defvar tzz-efrit-backend
  (cond ((eq tzz-environment 'hrt) 'bifrost)
        (t 'anthropic))
  "Which endpoint efrit talks to: `anthropic', `litellm', or `bifrost'.")

;;; Bifrost key (per-user, minted by locksmith; body is plain text) -------

(defconst tzz-bifrost-root "https://bifrost.hudson-trading.com")
(defconst tzz-bifrost-locksmith
  "https://bifrost-locksmith.algo.rd1-prod-1.k8s.hudson-trading.com/key")

(defvar tzz-bifrost--key nil "Cached Bifrost key; nil until first mint.")

(defun tzz-bifrost-key (&optional refresh)
  "Return a Bifrost key, minting one from locksmith unless cached.
With REFRESH non-nil (interactively, a prefix arg) mint a new one --
do this after a 401."
  (interactive "P")
  (when (or refresh (null tzz-bifrost--key))
    (let* ((url-request-method "POST")
           (url-request-extra-headers
            '(("Content-Type" . "application/json")
              ("X-Bifrost-Environment" . "prod")))
           (url-request-data
            (encode-coding-string
             (json-encode `(("username" . ,(user-login-name)))) 'utf-8)))
      (with-current-buffer (url-retrieve-synchronously tzz-bifrost-locksmith t nil 30)
        (goto-char (point-min))
        (unless (re-search-forward "^HTTP/[0-9.]+ 2" (line-end-position) t)
          (error "Bifrost locksmith refused the key request"))
        (re-search-forward "\n\n")
        (setq tzz-bifrost--key
              (string-trim (buffer-substring-no-properties (point) (point-max))))
        (kill-buffer))))
  tzz-bifrost--key)

;;; LiteLLM key (legacy HRT proxy) ----------------------------------------

(defvar tzz-litellm--key nil)
(defun tzz-litellm-key ()
  (or tzz-litellm--key
      (setq tzz-litellm--key
            (string-trim-right
             (shell-command-to-string "ssh shauser-tzz2 /ubin/get_llmproxy_key.py")))))

;;; efrit ------------------------------------------------------------------

(tzz-use-package efrit
  ;; Local checkout while iterating; switch back to :vc when done.
  :load-path "~/ai_workspace/source/efrit-upstream/lisp"
  ;; :vc (:url "https://github.com/steveyegge/efrit" :rev :newest
  ;;      :files ("lisp/*.el" "lisp/core/*.el" "lisp/interfaces/*.el"
  ;;              "lisp/support/*.el" "lisp/tools/*.el" "lisp/dev/*.el"))
  :init
  (setq efrit-data-directory (expand-file-name "~/efrit-data"))
  :commands (efrit efrit-agent efrit-do efrit-chat efrit-resume)
  :bind (("C-c e" . efrit-do)
         ("C-c E" . efrit))            ; REPL agent buffer is the primary surface
  :config
  (pcase tzz-efrit-backend
    ('bifrost
     (setq efrit-api-base-url (concat tzz-bifrost-root "/anthropic")
           efrit-api-auth-scheme 'bearer
           efrit-api-key #'tzz-bifrost-key
           efrit-default-model "claude-sonnet-4-5"
           ;; attribution only; harmless if dropped
           efrit-api-custom-headers `(("x-bf-dim-unix" . ,(user-login-name))
                                      ("x-bf-dim-host" . ,(system-name))
                                      ("x-bf-dim-client" . "efrit"))))
    ('litellm
     (setq efrit-api-base-url "https://litellm.hudson-trading.com"
           efrit-api-auth-scheme 'bearer
           efrit-api-key #'tzz-litellm-key
           efrit-default-model "claude-sonnet-4-20250514"))
    (_
     ;; Direct Anthropic: key from ~/.authinfo
     ;; machine api.anthropic.com login personal password sk-ant-...
     (setq efrit-api-auth-scheme 'x-api-key
           efrit-api-key nil)))
  ;; Flip to nil if a proxy rejects cache_control blocks.
  (setq efrit-api-prompt-caching t)
  (setq efrit-enable-global-keymap t)
  (efrit-setup-keybindings))

;;; tzz-efrit-config.el ends here
