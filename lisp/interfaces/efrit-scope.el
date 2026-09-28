;;; efrit-scope.el --- Run a prompt over the region, the defun, or the buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai, convenience

;;; Commentary:

;; `efrit-scope-run': pick a prompt from the library (`efrit-prompts')
;; and run it over the text at hand, as a turn in the agent buffer.
;; The scope is the active region; else, in a `prog-mode' buffer, the
;; defun at point; else the whole buffer.  The `prog-mode' guard
;; matters: `bounds-of-thing-at-point' for `defun' returns nonsense in
;; prose (copilot-chat-task's rule, 2026-09-28).
;;
;; Prompts use mustache-style placeholders (after minuet): {{{:text}}}
;; is the scoped text, {{{:file}}} its file, {{{:mode}}} the major mode
;; name, {{{:scope}}} the word region/defun/buffer, {{{:lines}}} the
;; line range.  A prompt without {{{:text}}} gets the text appended in
;; a fenced block.  The built-in scoped prompts (explain, fix, document,
;; tests, review, simplify) are `single' prompts in the library, so the
;; prompt editor and manager apply to them.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-prompts)
(require 'efrit-ui-helpers)
(require 'efrit-agent-input)

(defgroup efrit-scope nil
  "Prompts over the region, the defun or the buffer."
  :group 'efrit)

(defcustom efrit-scope-max-chars 40000
  "Longest scoped text sent in full; longer ones are cut with a note."
  :type 'integer
  :group 'efrit-scope)

;;;; Placeholders

(defun efrit-scope-fill (template values)
  "TEMPLATE with each {{{:key}}} replaced from VALUES, an alist of (key . value).
A value may be a string, a symbol whose value is used, or a function
called with no arguments.  Unknown keys are left as they are."
  (replace-regexp-in-string
   "{{{:\\([a-z-]+\\)}}}"
   (lambda (m)
     (let* ((key (intern (match-string 1 m)))
            (cell (assq key values)))
       (if (null cell)
           m
         (let ((v (cdr cell)))
           (format "%s" (cond ((functionp v) (funcall v))
                              ((and (symbolp v) (boundp v)) (symbol-value v))
                              (t (or v ""))))))))
   template t t))

;;;; Scope

(defun efrit-scope-bounds ()
  "The (START END KIND) to work on: region, defun in prog-mode, else buffer."
  (cond
   ((use-region-p) (list (region-beginning) (region-end) 'region))
   ((and (derived-mode-p 'prog-mode)
         (bounds-of-thing-at-point 'defun))
    (let ((b (bounds-of-thing-at-point 'defun)))
      (list (car b) (cdr b) 'defun)))
   (t (list (point-min) (point-max) 'buffer))))

(defun efrit-scope--text (start end)
  "The text START..END, cut to `efrit-scope-max-chars'."
  (let ((text (buffer-substring-no-properties start end)))
    (if (> (length text) efrit-scope-max-chars)
        (concat (substring text 0 efrit-scope-max-chars)
                (format "\n[… %d more characters omitted]" (- (length text) efrit-scope-max-chars)))
      text)))

(defun efrit-scope-values (start end kind)
  "The placeholder values for START..END of KIND in the current buffer."
  (let ((text (efrit-scope--text start end)))
    `((text . ,text)
      (file . ,(or (and buffer-file-name (abbreviate-file-name buffer-file-name)) (buffer-name)))
      (mode . ,(string-remove-suffix "-mode" (symbol-name major-mode)))
      (scope . ,(symbol-name kind))
      (lines . ,(format "%d-%d" (line-number-at-pos start) (line-number-at-pos (max start (1- end))))))))

(defun efrit-scope-build (prompt values)
  "PROMPT filled from VALUES.
The text is appended in a fence when PROMPT does not place it."
  (let ((filled (efrit-scope-fill prompt values)))
    (if (string-match-p "{{{:text}}}" prompt)
        filled
      (let ((text (alist-get 'text values)))
        (concat filled "\n\n" (efrit-fence-for text) (alist-get 'mode values) "\n"
                text "\n" (efrit-fence-for text))))))

;;;; Built-in scoped prompts

(dolist (p '(("explain" "Explain what this {{{:scope}}} of {{{:file}}} ({{{:mode}}}, lines {{{:lines}}}) does, for someone who knows the language but not this code. Name the inputs, the outputs, the side effects, and anything surprising. Keep it short; no restating the code line by line."
              "What the code at hand does")
             ("fix" "Find the bugs in this {{{:scope}}} of {{{:file}}} ({{{:mode}}}, lines {{{:lines}}}). For each: the line, what goes wrong, and the smallest fix. If it is correct, say so and stop. Do not restyle or refactor."
              "Bugs in the code at hand, with minimal fixes")
             ("document" "Write the documentation for this {{{:scope}}} of {{{:file}}} ({{{:mode}}}): the docstring or comment block in the language's convention, saying what it does, its arguments and return value, and any caveat. Return only the documentation text, ready to paste."
              "A docstring or comment block for the code at hand")
             ("tests" "Write tests for this {{{:scope}}} of {{{:file}}} ({{{:mode}}}) in the project's test framework (look at the project if you need to). Cover the normal path, the edge cases, and one failure. Return the test code only."
              "Tests for the code at hand")
             ("review" "Review this {{{:scope}}} of {{{:file}}} ({{{:mode}}}, lines {{{:lines}}}) as a careful colleague: correctness first, then clarity, then performance. One line per finding with the line number, ordered by importance. Say what is good in one sentence at the end. No rewrites unless asked."
              "A code review of the code at hand")
             ("simplify" "Simplify this {{{:scope}}} of {{{:file}}} ({{{:mode}}}) without changing what it does: fewer branches, clearer names, standard idioms of the language. Return the new code only, then one line on what changed."
              "A simpler version of the code at hand")))
  (efrit-prompts-define (nth 0 p) (nth 1 p) "" (nth 2 p) 'single))

;;;; The command

;;;###autoload
(defun efrit-scope-run (prompt)
  "Run PROMPT (a library prompt name, or a question) over the text at hand.
The scope is the active region, else the defun at point in a
`prog-mode' buffer, else the buffer.  The turn runs in the agent
buffer; the line shown there names the scope, the model gets the
filled prompt with the text."
  (interactive
   (list (efrit-prompts-read
          (pcase-let ((`(,_ ,_ ,kind) (efrit-scope-bounds)))
            (format "Run over the %s" kind)))))
  (pcase-let* ((`(,start ,end ,kind) (efrit-scope-bounds))
               (pair (efrit-prompts-pair prompt))
               (values (efrit-scope-values start end kind))
               (api (efrit-scope-build (car pair) values))
               (shown (format "%s: %s %s of %s"
                              (or (efrit-prompts-name-of (car pair))
                                  (truncate-string-to-width (car pair) 40 nil nil "…"))
                              kind (alist-get 'lines values) (alist-get 'file values))))
    (deactivate-mark)
    (unless (efrit-submit shown api)
      (user-error "efrit is busy with another turn; try again when it is idle"))))

(provide 'efrit-scope)

;;; efrit-scope.el ends here
