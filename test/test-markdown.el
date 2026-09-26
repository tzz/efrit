;;; test-markdown.el --- efrit-markdown, the in-place renderer -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-markdown)

(defun test-md--faces (text)
  "The faces of the rendered TEXT, as ((SUBSTRING . FACE) ...) per run."
  (let ((s (efrit-markdown-render-string text)) (out nil) (pos 0))
    (while (< pos (length s))
      (let ((next (or (next-single-property-change pos 'face s) (length s))))
        (push (cons (substring-no-properties s pos next) (get-text-property pos 'face s)) out)
        (setq pos next)))
    (nreverse out)))

(defun test-md--has-face (s face &optional substring)
  "Non-nil when S carries FACE (on SUBSTRING when given)."
  (let ((start (if substring (string-search substring s) 0)))
    (and start
         (let ((f (get-text-property start 'face s)))
           (if (listp f) (memq face f) (eq f face))))))

(ert-deftest test-markdown-inline-constructs ()
  "Headers, bold, italic, strike, inline code, links, bullets, rules render
in place; the markup is gone and faces are on the right characters."
  (let ((s (efrit-markdown-render-string
            "# Title\n\nSome **bold** and *italic* and ~~gone~~ and `code *x*`.\n\n- item one\n- item two\n\n---\n\nSee [efrit](https://example.org/x) or https://example.org/y.")))
    ;; Markup gone, except the literal * inside the code span
    (should (string-prefix-p "Title\n\nSome bold and italic and gone and code *x*.\n\n• item one\n• item two\n\n"
                             (substring-no-properties s)))
    (should-not (string-match-p "[#~`]" (substring-no-properties s)))
    (should (test-md--has-face s 'efrit-markdown-header "Title"))
    (should (test-md--has-face s 'efrit-markdown-bold "bold"))
    (should (test-md--has-face s 'efrit-markdown-italic "italic"))
    (should (test-md--has-face s 'efrit-markdown-strike "gone"))
    (should (test-md--has-face s 'efrit-markdown-inline-code "code *x*"))
    ;; the * inside the code span stayed frozen: no italic face there
    (should-not (test-md--has-face s 'efrit-markdown-italic "x"))
    (should (string-match-p "• item one\n• item two" (substring-no-properties s)))
    (should (test-md--has-face s 'efrit-markdown-bullet "•"))
    (should (test-md--has-face s 'efrit-markdown-rule "    "))
    (should (equal "https://example.org/x"
                   (get-text-property (string-search "efrit" s) 'efrit-markdown-target s)))
    (should (equal "https://example.org/y"
                   (get-text-property (string-search "https://example.org/y" s) 'efrit-markdown-target s)))
    ;; the sentence's final dot is not part of the bare url
    (should-not (get-text-property (1- (length s)) 'efrit-markdown-target s))
    ;; faces are mirrored for font-lock, and the range is fontified
    (should (equal (get-text-property (string-search "bold" s) 'face s)
                   (get-text-property (string-search "bold" s) 'font-lock-face s)))
    (should (get-text-property 0 'fontified s))))

(ert-deftest test-markdown-code-block ()
  "A fenced block loses its fences, keeps its body on a block background,
fontified per language, with a label; its content is frozen."
  (let ((s (efrit-markdown-render-string
            "Before\n\n```elisp\n(defun f () \"str\" *not-italic*)\n```\n\nAfter **b**")))
    (should-not (string-match-p "```" (substring-no-properties s)))
    (should (string-match-p "Before\n\nelisp\n(defun f () \"str\" \\*not-italic\\*)\n\nAfter b" (substring-no-properties s)))
    (should (test-md--has-face s 'efrit-markdown-code-label "elisp"))
    (should (test-md--has-face s 'efrit-markdown-code-block "(defun"))
    ;; emacs-lisp-mode fontified the string
    (should (test-md--has-face s 'font-lock-doc-face "\"str\""))
    (should (get-text-property (string-search "(defun" s) 'efrit-markdown-frozen s))
    (should-not (test-md--has-face s 'efrit-markdown-italic "not-italic"))
    (should (test-md--has-face s 'efrit-markdown-bold "b"))))

(ert-deftest test-markdown-streaming-watermark ()
  "Rendered in chunks: text before the watermark is not touched again, the
last line and an open fence are held back until complete, and the
result equals rendering the whole text at once."
  (let ((whole "# Head\n\nA **bold** word.\n\n```sh\necho *hi*\n```\n\nTail *it*")
        (chunks '("# He" "ad\n\nA **bo" "ld** word.\n\n```sh\necho *hi*\n" "```\n\nTail *it" "*")))
    (with-temp-buffer
      (let ((start (point-min-marker)) (end (point-max-marker)))
        (set-marker-insertion-type end t)
        (dolist (c chunks)
          (goto-char end)
          (insert c)
          (efrit-markdown-render start end nil))
        ;; Before the final pass: the open fence was rendered only when
        ;; its close arrived; the last line "Tail *it*" is still raw
        (should (string-match-p "Tail \\*it\\*" (buffer-string)))
        (should (string-match-p "\necho \\*hi\\*\n" (buffer-string)))
        (should (test-md--has-face (buffer-string) 'efrit-markdown-code-block "echo"))
        (should (test-md--has-face (buffer-string) 'efrit-markdown-bold "bold"))
        (efrit-markdown-render start end t)
        (should (test-md--has-face (buffer-string) 'efrit-markdown-italic "it"))
        (should (equal (substring-no-properties (buffer-string))
                       (substring-no-properties (efrit-markdown-render-string whole))))))))

(ert-deftest test-markdown-file-references ()
  "Paths that exist become links, with line and column, also inside code
spans; paths that do not exist stay text."
  (let* ((dir (make-temp-file "efrit-md-" t))
         (file (expand-file-name "thing.el" dir))
         (default-directory (file-name-as-directory dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "1\n2\n3\n"))
          (cl-letf (((symbol-function 'efrit-tool--get-project-root) (lambda () dir)))
            (let ((s (efrit-markdown-render-string
                      "Fix `thing.el:2:1` and thing.el:3, not missing.el:9.")))
              (should (equal (list file 2 1)
                             (get-text-property (string-search "thing.el:2" s) 'efrit-markdown-target s)))
              (should (equal (list file 3 nil)
                             (get-text-property (string-search "thing.el:3" s) 'efrit-markdown-target s)))
              (should-not (get-text-property (string-search "missing" s) 'efrit-markdown-target s))
              (should (test-md--has-face s 'efrit-markdown-link "thing.el:3"))
              ;; following opens the file at the line
              (let* ((opened nil)
                     (efrit-markdown-open-file-function
                      (lambda (f) (setq opened f) (set-buffer (find-file-noselect f)))))
                (progn
                  (with-temp-buffer
                    (insert s)
                    (goto-char (1+ (string-search "thing.el:3" s)))
                    (efrit-markdown-follow-link)
                    (should (equal file opened))
                    (should (= 3 (line-number-at-pos)))))))))
      (delete-directory dir t))))

(ert-deftest test-markdown-disabled-and-plain ()
  "Disabled, nothing changes; text without markup is returned as is."
  (let ((efrit-markdown-enabled nil))
    (should (equal "**raw**" (substring-no-properties (efrit-markdown-render-string "**raw**")))))
  (should (equal "just words 2*3=6 and a_b_c"
                 (substring-no-properties (efrit-markdown-render-string "just words 2*3=6 and a_b_c")))))

(provide 'test-markdown)
;;; test-markdown.el ends here
