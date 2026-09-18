;;; test-reload.el --- efrit-reload -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-reload)
(require 'efrit-log)
(require 'efrit-sandbox)

(ert-deftest test-reload-features-are-efrit-oldest-first ()
  (let ((fs (efrit-reload-features)))
    (should (cl-every (lambda (f) (string-prefix-p "efrit" (symbol-name f))) fs))
    (should-not (memq 'efrit-reload fs))
    ;; efrit-log was required before efrit-sandbox above, and sandbox
    ;; requires log, so log must come first in reload order
    (should (< (cl-position 'efrit-log fs) (cl-position 'efrit-sandbox fs)))))

(ert-deftest test-reload-reloads-and-keeps-settings ()
  "A reload re-evaluates definitions but a customized value survives."
  (let ((efrit-sandbox-default-project-grants '(read write)))
    (defvar test-reload--marker nil)
    (setq test-reload--marker 'before)
    ;; a defvar in a reloaded file does not clobber a bound variable
    (should (> (efrit-reload) 0))
    (should (equal efrit-sandbox-default-project-grants '(read write)))
    (should (fboundp 'efrit-sandbox-check))
    (should (featurep 'efrit-sandbox))))

(ert-deftest test-reload-refreshes-keymaps ()
  "A binding added to the source reaches a live buffer after reload.
Keymap defvars are skipped on plain `load'; efrit-reload unbinds them
first and re-points the minor-mode alist and mode buffers."
  (require 'efrit-agent)
  (require 'efrit-agent-input)
  (efrit)
  (with-current-buffer (efrit-agent--get-buffer)
    ;; simulate a map defined before these keys existed
    (define-key efrit-agent-input-mode-map (kbd "<up>") nil)
    (define-key efrit-agent-mode-map (kbd "C-c C-m") nil)
    (goto-char (point-max)) (efrit-agent--maybe-enable-input-mode)
    (should-not (eq (key-binding (kbd "<up>")) 'efrit-agent-input-up))
    (efrit-reload)
    (efrit-agent--maybe-enable-input-mode)
    (should (eq (key-binding (kbd "<up>")) 'efrit-agent-input-up))
    (should (eq (key-binding (kbd "C-c C-m")) 'efrit-menu))
    (should (eq (cdr (assq 'efrit-agent-input-mode minor-mode-map-alist))
                efrit-agent-input-mode-map))))

(provide 'test-reload)
;;; test-reload.el ends here
