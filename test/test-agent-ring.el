;;; test-agent-ring.el --- Next/previous/attention between agent buffers -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-agent)
(require 'efrit-agent-ring)
(require 'efrit-repl-session)

(defun test-ring--buffer (name)
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer))
      (efrit-agent-mode)
      (efrit-agent--init-regions)
      (efrit-agent--setup-regions)
      (setq default-directory temporary-file-directory)
      (setq efrit-agent--repl-session (efrit-repl-session-create temporary-file-directory))
      (setf (efrit-repl-session-buffer efrit-agent--repl-session) buf))
    buf))

(defmacro test-ring--with-three (a b c &rest body)
  (declare (indent 3))
  `(let* ((efrit-agent-auto-show nil)
          (efrit-agent-read-delay 5)
          (,a (test-ring--buffer " *ring-a*"))
          (,b (test-ring--buffer " *ring-b*"))
          (,c (test-ring--buffer " *ring-c*")))
     (unwind-protect (progn ,@body)
       (efrit-agent-ring--stop-heartbeat)
       (kill-buffer ,a) (kill-buffer ,b) (kill-buffer ,c))))

(defun test-ring--id (buf)
  (efrit-repl-session-id (buffer-local-value 'efrit-agent--repl-session buf)))

(ert-deftest test-ring-order-is-creation-not-recency ()
  (test-ring--with-three a b c
    (should (equal (list a b c) (seq-filter (lambda (x) (memq x (list a b c))) (efrit-agent-ring))))
    ;; touching c does not move it
    (with-current-buffer c (bury-buffer))
    (set-buffer a)
    (should (equal (list a b c) (seq-filter (lambda (x) (memq x (list a b c))) (efrit-agent-ring))))
    (should (eq b (efrit-agent-ring--target (list a b c) a nil)))
    (should (eq c (efrit-agent-ring--target (list a b c) a t)))
    (should (eq a (efrit-agent-ring--target (list a b c) c nil)))
    (should (eq a (efrit-agent-ring--target (list a b c) nil nil)))))

(ert-deftest test-ring-attention-queue-waiting-first-then-unread ()
  (test-ring--with-three a b c
    (should (null (efrit-agent-ring-attention-queue (list a b c))))
    (efrit-publish 'turn-complete (list (cons :session-id (test-ring--id c)) (cons :stop-reason "end_turn")))
    (efrit-publish 'prompt-open (list (cons :label "sandbox: write x") (cons :session-id (test-ring--id b))))
    (should (buffer-local-value 'efrit-agent--unread c))
    (should (equal "sandbox: write x" (buffer-local-value 'efrit-agent--waiting b)))
    (should (equal (list b c) (efrit-agent-ring-attention-queue (list a b c))))
    (should (string-match-p "1 waiting" (efrit-agent-ring-summary)))
    (should (string-match-p "1 unread" (efrit-agent-ring-summary)))
    (with-current-buffer b (should (string-match-p "●" (efrit-agent-ring-mode-line-string))))
    (with-current-buffer c (should (string-match-p "●" (efrit-agent-ring-mode-line-string))))
    (with-current-buffer a (should (string-match-p "●" (efrit-agent-ring-mode-line-string))))
    (efrit-publish 'prompt-closed (list (cons :label "sandbox: write x") (cons :session-id (test-ring--id b))))
    (should-not (buffer-local-value 'efrit-agent--waiting b))
    (should (equal (list c) (efrit-agent-ring-attention-queue (list a b c))))
    (efrit-agent-mark-read c)
    (should (null (efrit-agent-ring-attention-queue (list a b c))))))

(ert-deftest test-ring-dwell-marks-read-after-continuous-viewing ()
  "Five continuous seconds looking at the end mark it read; a switch or
a gap over a second restarts the count."
  (test-ring--with-three a b c
    (efrit-publish 'turn-complete (list (cons :session-id (test-ring--id a)) (cons :stop-reason "end_turn")))
    (should (buffer-local-value 'efrit-agent--unread a))
    (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) t)))
      (with-selected-window (selected-window)
        (set-window-buffer (selected-window) a)
        (set-window-point (selected-window) (point-max))
        (cl-letf (((symbol-function 'pos-visible-in-window-p) (lambda (&rest _) t)))
          (efrit-agent-ring-check-dwell 100.0)
          (efrit-agent-ring-check-dwell 102.0)
          (efrit-agent-ring-check-dwell 104.0)
          (should (buffer-local-value 'efrit-agent--unread a))
          ;; a gap of 3 s is not reading: the clock restarts
          (efrit-agent-ring-check-dwell 107.5)
          (efrit-agent-ring-check-dwell 108.5)
          (should (buffer-local-value 'efrit-agent--unread a))
          (efrit-agent-ring-check-dwell 109.5)
          (efrit-agent-ring-check-dwell 110.5)
          (efrit-agent-ring-check-dwell 111.5)
          (efrit-agent-ring-check-dwell 112.5)
          (should-not (buffer-local-value 'efrit-agent--unread a))
          ;; a view of another buffer counts nothing
          (efrit-publish 'turn-complete (list (cons :session-id (test-ring--id a)) (cons :stop-reason "end_turn")))
          (set-window-buffer (selected-window) b)
          (dotimes (i 8) (efrit-agent-ring-check-dwell (+ 200.0 i)))
          (should (buffer-local-value 'efrit-agent--unread a)))))))

(ert-deftest test-ring-viewing-means-any-part-of-the-last-turn ()
  "A long reply read from its top counts as viewing even with the end off
screen; a window that starts before the last turn with the end off
screen does not.  `window-end' is not consulted: it ran short of the
buffer end in a large frame and kept a buffer at Bot unread."
  (test-ring--with-three a b c
    (with-current-buffer a
      (cl-letf (((symbol-function 'efrit-agent--auto-save-session) #'ignore))
        (efrit-agent--add-user-message "first")
        (efrit-agent-stream-content "short answer")
        (efrit-agent-stream-end)
        (efrit-agent--add-user-message "second")
        (efrit-agent-stream-content (mapconcat (lambda (i) (format "line %d" i)) (number-sequence 1 400) "\n"))
        (efrit-agent-stream-end)))
    (let ((turn (efrit-agent-ring--last-turn-start a)))
      (should turn)
      (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) t))
                ((symbol-function 'pos-visible-in-window-p) (lambda (&rest _) nil)))
        (save-window-excursion
          (set-window-buffer (selected-window) a)
          ;; window starts in the first turn: not reading the reply
          (set-window-start (selected-window) (point-min))
          (should-not (efrit-agent-ring--viewing-p a))
          ;; window starts inside the last turn, end off screen: reading
          (set-window-start (selected-window) (with-current-buffer a
                                                (save-excursion (goto-char turn) (forward-line 3) (point))))
          (should (efrit-agent-ring--viewing-p a))))
      ;; the end on screen is reading wherever the window starts
      (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) t))
                ((symbol-function 'pos-visible-in-window-p) (lambda (&rest _) t)))
        (save-window-excursion
          (set-window-buffer (selected-window) a)
          (set-window-start (selected-window) (point-min))
          (should (efrit-agent-ring--viewing-p a)))))))

(ert-deftest test-ring-heartbeat-restores-the-post-command-hook ()
  "When the view hook was dropped from `post-command-hook' (Emacs evicts
a function that signals), the heartbeat puts it back; and the hook
function itself swallows errors so it is never evicted again."
  (test-ring--with-three a b c
    (efrit-publish 'turn-complete (list (cons :session-id (test-ring--id a)) (cons :stop-reason "end_turn")))
    (let ((saved (default-value 'post-command-hook)))
      (unwind-protect
          (progn
            (remove-hook 'post-command-hook #'efrit-agent-ring--view-changed)
            (should-not (memq #'efrit-agent-ring--view-changed (default-value 'post-command-hook)))
            (efrit-agent-ring--heartbeat-tick)
            (should (memq #'efrit-agent-ring--view-changed (default-value 'post-command-hook)))
            (cl-letf (((symbol-function 'efrit-agent-ring-check-dwell) (lambda (&rest _) (error "boom"))))
              (set-window-buffer (selected-window) a)
              (efrit-agent-ring--view-changed)
              (efrit-agent-ring--heartbeat-tick))
            (should (memq #'efrit-agent-ring--view-changed (default-value 'post-command-hook))))
        (setq-default post-command-hook saved)))))

(ert-deftest test-ring-commands-move-in-place ()
  (test-ring--with-three a b c
    (cl-letf (((symbol-function 'efrit-agent-display) (lambda (buf &rest _) (set-buffer buf) (selected-window))))
      (with-selected-window (selected-window)
        (set-window-buffer (selected-window) a)
        (set-buffer a)
        (efrit-agent-next)
        (should (eq b (window-buffer (selected-window))))
        (efrit-agent-next)
        (should (eq c (window-buffer (selected-window))))
        (efrit-agent-next)
        (should (eq a (window-buffer (selected-window))))
        (efrit-agent-previous)
        (should (eq c (window-buffer (selected-window))))
        (efrit-agent-switch-to-number 2)
        (should (eq b (window-buffer (selected-window))))
        (should-error (efrit-agent-switch-to-number 40) :type 'user-error)
        ;; attention: nothing waiting is a message, not an error
        (efrit-agent-next-attention)
        (should (eq b (window-buffer (selected-window))))
        (efrit-publish 'prompt-open (list (cons :label "q") (cons :session-id (test-ring--id c))))
        (efrit-agent-next-attention)
        (should (eq c (window-buffer (selected-window))))
        (efrit-publish 'prompt-closed (list (cons :label "q") (cons :session-id (test-ring--id c))))))))

(provide 'test-agent-ring)

;;; test-agent-ring.el ends here
