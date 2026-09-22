;;; test-efrit-prompts.el --- Tests for efrit-prompts -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-prompts)

(defmacro test-prompts--with-library (&rest body)
  "Run BODY with a scratch prompt file and two built-ins."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "efrit-prompts-test" t))
          (efrit-prompts-file (expand-file-name "prompts.json" dir))
          (efrit-prompts-builtin nil)
          (efrit-prompts--user nil)
          (efrit-prompts--loaded nil)
          (efrit-prompts-last nil)
          (efrit-prompts-change-hook nil))
     (efrit-prompts-define "one" "Item one." "Summary one." "first")
     (efrit-prompts-define "two" "Item two." "Summary two.")
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

(ert-deftest test-efrit-prompts-library-merges-user-and-builtin ()
  "User prompts come first, override built-ins by name, persist to disk,
and deleting the override restores the built-in."
  (test-prompts--with-library
    (should (equal '("one" "two") (mapcar (lambda (p) (plist-get p :name)) (efrit-prompts))))
    (should (plist-get (efrit-prompts-get "one") :builtin))
    ;; A prompt of the user's own.
    (efrit-prompts-put "mine" "Item mine." "Summary mine." "why")
    (should (equal '("mine" "one" "two") (mapcar (lambda (p) (plist-get p :name)) (efrit-prompts))))
    (should-not (plist-get (efrit-prompts-get "mine") :builtin))
    ;; Overriding a built-in keeps one entry, marked changed.
    (efrit-prompts-put "one" "Item one, better." "Summary one." "first")
    (should (= 3 (length (efrit-prompts))))
    (let ((p (efrit-prompts-get "one")))
      (should (equal "Item one, better." (plist-get p :item)))
      (should (plist-get p :builtin))
      (should (plist-get p :changed)))
    ;; Persisted: a fresh load reads the same.
    (should (file-exists-p efrit-prompts-file))
    (efrit-prompts-reload)
    (should (equal "Item one, better." (plist-get (efrit-prompts-get "one") :item)))
    (should (equal "why" (plist-get (efrit-prompts-get "mine") :description)))
    ;; Delete the override: the built-in is back.
    (efrit-prompts-delete "one")
    (should-not (plist-get (efrit-prompts-get "one") :changed))
    (should (equal "Item one." (plist-get (efrit-prompts-get "one") :item)))
    (should-error (efrit-prompts-delete "two") :type 'user-error)
    ;; Rename works on the user's prompts only.
    (efrit-prompts-rename "mine" "ours")
    (should (efrit-prompts-get "ours"))
    (should-not (efrit-prompts-get "mine"))
    (should-error (efrit-prompts-rename "two" "deux") :type 'user-error)
    ;; Validation.
    (should-error (efrit-prompts-put "" "x" "y") :type 'user-error)
    (should-error (efrit-prompts-put "blank" "  " "y") :type 'user-error)))

(ert-deftest test-efrit-prompts-pair-and-names ()
  "Pairs resolve from a name, a plist, a pair or a question; the last
used name is offered first."
  (test-prompts--with-library
    (should (equal '("Item one." . "Summary one.") (efrit-prompts-pair "one")))
    (should (equal '("Item one." . "Summary one.") (efrit-prompts-pair (efrit-prompts-get "one"))))
    (should (equal '("a" . "b") (efrit-prompts-pair '("a" . "b"))))
    (should (equal '("What now?" . nil) (efrit-prompts-pair "What now?")))
    (should (equal "two" (efrit-prompts-name-of "Item two.")))
    (should-not (efrit-prompts-name-of "nope"))
    (should (equal '("one" "two") (efrit-prompts-names)))
    (setq efrit-prompts-last "two")
    (should (equal '("two" "one") (efrit-prompts-names)))))

(ert-deftest test-efrit-prompts-read-falls-back-to-completion ()
  "In batch the chooser is `completing-read'; a name gives its pair and is
remembered, free text gives a question, \"edit\" opens the manager."
  (test-prompts--with-library
    (let ((managed nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "two")))
        (should (equal '("Item two." . "Summary two.") (efrit-prompts-read "Test")))
        (should (equal "two" efrit-prompts-last)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Who is blocked?")))
        (should (equal '("Who is blocked?" . nil) (efrit-prompts-read))))
      (let ((answers '("edit" "one")))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) (pop answers)))
                  ((symbol-function 'efrit-prompts-manage)
                   (lambda (&optional _wait) (setq managed t))))
          (should (equal '("Item one." . "Summary one.") (efrit-prompts-read)))
          (should managed))))))

(ert-deftest test-efrit-prompts-parse-suggestion ()
  "The model's two-block answer parses; anything else is nil."
  (should (equal '("Per item text." . "Summary text.")
                 (efrit-prompts-parse-suggestion
                  "=== ITEM ===\nPer item text.\n=== SUMMARY ===\nSummary text.\n")))
  (should (equal '("A\nB" . "C")
                 (efrit-prompts-parse-suggestion "=== ITEM ===\nA\nB\n\n=== SUMMARY ===\n\nC")))
  (should-not (efrit-prompts-parse-suggestion "Here is a better prompt: ..."))
  (should-not (efrit-prompts-parse-suggestion nil)))

(ert-deftest test-efrit-prompts-suggest-uses-api ()
  "A suggestion is one API call with the system instructions and both
parts; the callback gets the parsed pair, or the failure as a string."
  (test-prompts--with-library
    (let ((request nil) (got nil))
      (cl-letf (((symbol-function 'efrit-api-request-async)
                 (lambda (req callback &optional _err)
                   (setq request req)
                   (let ((resp (make-hash-table :test 'equal))
                         (block (make-hash-table :test 'equal)))
                     (puthash "type" "text" block)
                     (puthash "text" "=== ITEM ===\nBetter item.\n=== SUMMARY ===\nBetter summary." block)
                     (puthash "content" (vector block) resp)
                     (funcall callback resp)))))
        (let ((efrit-default-model "test-model"))
          (efrit-prompts-suggest "one" "Item one." "Summary one." "find blockers"
                                 (lambda (r) (setq got r))))
        (should (equal '("Better item." . "Better summary.") got))
        (should (equal "test-model" (alist-get "model" request nil nil #'equal)))
        (let ((user (alist-get "content" (aref (alist-get "messages" request nil nil #'equal) 0)
                               nil nil #'equal)))
          (should (string-match-p "Item one\\." user))
          (should (string-match-p "find blockers" user))))
      (cl-letf (((symbol-function 'efrit-api-request-async)
                 (lambda (_req _callback &optional err) (funcall err "boom"))))
        (efrit-prompts-suggest "one" "i" "s" nil (lambda (r) (setq got r)))
        (should (equal "boom" got))))))

(ert-deftest test-efrit-prompts-editor-round-trip ()
  "The editor buffer reads back what was inserted, saves through
`efrit-prompts-put', and a suggestion replaces the two parts."
  (test-prompts--with-library
    (let ((buf (get-buffer-create "*efrit prompt: test*")))
      (unwind-protect
          (with-current-buffer buf
            (efrit-prompts-edit-mode)
            (efrit-prompts-edit--insert '(:name "one" :description "first"
                                          :item "Item one." :summary "Summary one."))
            (should (equal '(:name "one" :description "first" :item "Item one." :summary "Summary one.")
                           (efrit-prompts-edit--read)))
            (efrit-prompts-edit--replace-section :item "New item.")
            (efrit-prompts-edit--replace-section :summary "New summary.")
            (should (equal "New item." (plist-get (efrit-prompts-edit--read) :item)))
            (should (equal "New summary." (plist-get (efrit-prompts-edit--read) :summary)))
            (setq efrit-prompts-edit--original (efrit-prompts-get "one"))
            (cl-letf (((symbol-function 'quit-window) #'ignore))
              (efrit-prompts-edit-save))
            (should (equal "New item." (plist-get (efrit-prompts-get "one") :item)))
            (should (plist-get (efrit-prompts-get "one") :changed)))
        (kill-buffer buf)))))

(ert-deftest test-efrit-prompts-manager-lists-everything ()
  "The manager shows one row per prompt with its badge and hints."
  (test-prompts--with-library
    (efrit-prompts-put "mine" "Item mine." "Summary mine.")
    (with-temp-buffer
      (efrit-prompts-manage-mode)
      (efrit-prompts-manage-refresh)
      (should (= 3 (length tabulated-list-entries)))
      (goto-char (point-min))
      (should (equal "mine" (tabulated-list-get-id)))
      (should (string-match-p "yours" (buffer-string)))
      (should (string-match-p "built-in" (buffer-string)))
      (should (string-match-p "Item mine" (buffer-string))))))

(provide 'test-efrit-prompts)
;;; test-efrit-prompts.el ends here
