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

(provide 'test-reload)
;;; test-reload.el ends here
