;;; efrit-tool-contract.el --- library_contract: how to call a function, from the loaded code -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Steve Yegge

;; This file is part of efrit.

;;; Commentary:
;;
;; One call that answers "what do I need to know before I call or
;; rebind this?" from the definitions loaded in this Emacs, not from
;; reading the library file in slices.
;;
;; On 2026-11-11 the model wrote `(magit-status top)' in a helper and
;; learned it was `interactive-only' from a byte-compile warning six
;; calls later; it learned that `git-link-commit' reads the hash from
;; the word at point by reading git-link.el in 20-line pieces; and it
;; learned Embark's target-injection rule from embark.el the same way.
;; Each fact was one property lookup or one docstring away.
;;
;; The contract of a function, as efrit reports it:
;;
;; - signature and whether it is a command, macro, special form,
;;   autoload (and what it would load), alias (and of what);
;; - the declarations the author made: `interactive-only' (and the
;;   replacement), obsolete (since, replacement), `pure',
;;   `side-effect-free', `important-return-value', `indent', `doc-string';
;; - the user options and variables the body reads or sets, found by
;;   walking the macroexpanded source form for free symbols that are
;;   bound variables, split into options (defcustom) and others;
;; - the hooks it runs (`run-hooks', `run-hook-with-args' arguments);
;; - the advice on it now;
;; - its callers among the loaded functions (`load-history' walked for
;;   definitions that name it: no grep, no project needed), capped, the
;;   user's own files first;
;; - the first paragraph of the docstring, and a note when the
;;   docstring mentions point, the region, the mark or the minibuffer,
;;   since those decide how an Embark action or a probe must set up.
;;
;; Read-only, not reviewed.  Nothing is called; the source form is
;; read from the file the symbol was loaded from.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'help-fns)
(require 'find-func)
(require 'nadvice)
(require 'efrit-tool-utils)
(require 'efrit-tool-introspect)

(declare-function efrit-tool--get-project-root "efrit-tool-utils")

(defgroup efrit-tool-contract nil
  "The contract of a function, from the loaded code."
  :group 'efrit-tool-utils)

(defcustom efrit-tool-contract-max-callers 12
  "Callers `library_contract' lists before saying how many more there are."
  :type 'integer
  :group 'efrit-tool-contract)

(defcustom efrit-tool-contract-max-variables 25
  "Variables `library_contract' lists per group before cutting."
  :type 'integer
  :group 'efrit-tool-contract)

(defconst efrit-tool-contract--declaration-props
  '((interactive-only . "interactive_only")
    (byte-obsolete-info . "obsolete")
    (pure . "pure")
    (side-effect-free . "side_effect_free")
    (important-return-value . "important_return_value")
    (lisp-indent-function . "indent")
    (doc-string-elt . "doc_string_elt")
    (completion-predicate . "completion_predicate"))
  "Symbol properties `declare' and `define-obsolete-function-alias' leave, and their JSON names.")

(defun efrit-tool-contract--declarations (symbol)
  "The declarations on SYMBOL's function as an alist of JSON name -> value."
  (delq nil
        (mapcar (lambda (entry)
                  (let ((value (get symbol (car entry))))
                    (when value
                      (cons (intern (cdr entry))
                            (pcase (car entry)
                              ('byte-obsolete-info
                               `((replacement . ,(format "%s" (or (nth 0 value) "none")))
                                 (since . ,(format "%s" (or (nth 2 value) "?")))))
                              ('interactive-only
                               (if (eq value t) t (format "%s" value)))
                              (_ (format "%s" value)))))))
                efrit-tool-contract--declaration-props)))

(defun efrit-tool-contract--kind (symbol definition)
  "One word for what SYMBOL's function DEFINITION is."
  (cond ((autoloadp definition) "autoload")
        ((and (symbolp definition) definition) "alias")
        ((macrop symbol) "macro")
        ((special-form-p symbol) "special-form")
        ((commandp symbol) "command")
        (t "function")))

(defun efrit-tool-contract--source-form (symbol)
  "The definition form of SYMBOL read from its source file, or nil."
  (when-let* ((loc (efrit-tool-introspect--file-line symbol 'function))
              (file (car loc)) (line (cdr loc))
              ((file-readable-p file)))
    (let ((buf (find-buffer-visiting file)))
      (with-current-buffer (or buf (find-file-noselect file t))
        (prog1
            (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-min))
                (forward-line (1- line))
                (condition-case nil (read (current-buffer)) (error nil))))
          (unless buf (kill-buffer)))))))

(defun efrit-tool-contract--walk (form fn &optional values-only)
  "Call FN on every cons and symbol in FORM, quoted data included.
With VALUES-ONLY, a symbol in function position (the car of a list
whose car is a symbol) is not visited: `(buffer-name)' is a call, not
a read of a variable named `buffer-name'.  Quoted symbols still are
\(a quoted name can be a variable passed to `symbol-value')."
  (cond ((consp form)
         (funcall fn form)
         (let ((rest form) (first t))
           (while (consp rest)
             (unless (and values-only first (symbolp (car rest))
                          (not (memq (car rest) '(quote function))))
               (efrit-tool-contract--walk (car rest) fn values-only))
             (setq first nil rest (cdr rest)))
           (when rest (efrit-tool-contract--walk rest fn values-only))))
        ((symbolp form) (funcall fn form))
        ((vectorp form) (mapc (lambda (x) (efrit-tool-contract--walk x fn values-only)) form))))

(defun efrit-tool-contract--bound-parameters (form)
  "Symbols FORM binds as parameters or `let' variables, best effort.
Those are not free: a `(let ((buffer …)) buffer)' does not read the
global `buffer'.  Walks arglists of defun/lambda/cl-defun and the
binding lists of let/let*/cl-letf*/dolist/dotimes/pcase-let/when-let*."
  (let ((bound nil))
    (efrit-tool-contract--walk
     form
     (lambda (x)
       (when (consp x)
         (pcase (car x)
           ((or 'defun 'defmacro 'cl-defun 'cl-defmacro 'defsubst 'cl-defsubst 'iter-defun)
            (dolist (p (and (listp (nth 2 x)) (nth 2 x)))
              (when (symbolp p) (push p bound))))
           ((or 'lambda 'cl-function)
            (dolist (p (and (listp (nth 1 x)) (nth 1 x)))
              (when (symbolp p) (push p bound))))
           ((or 'let 'let* 'dlet 'if-let 'if-let* 'when-let 'when-let* 'and-let* 'while-let 'pcase-let 'pcase-let* 'cl-letf*)
            (dolist (b (and (listp (nth 1 x)) (nth 1 x)))
              (cond ((symbolp b) (push b bound))
                    ((and (consp b) (symbolp (car b))) (push (car b) bound)))))
           ((or 'dolist 'dotimes 'cl-dolist 'cl-dotimes 'pcase-dolist)
            (when (and (consp (nth 1 x)) (symbolp (car (nth 1 x))))
              (push (car (nth 1 x)) bound)))
           ((or 'condition-case 'with-current-buffer 'save-match-data) nil)))))
    bound))

(defun efrit-tool-contract--set-targets (form)
  "Symbols FORM assigns with setq, setq-default, setq-local or set-default."
  (let ((set nil))
    (efrit-tool-contract--walk
     form
     (lambda (x)
       (when (and (consp x) (memq (car x) '(setq setq-default setq-local set-default)))
         (let ((rest (cdr x)))
           (while (consp rest)
             (when (symbolp (car rest)) (cl-pushnew (car rest) set))
             (setq rest (cddr rest)))))))
    set))

(defun efrit-tool-contract--variables (form)
  "Options and other variables FORM reads or sets: (OPTIONS . OTHERS), symbol lists.
A symbol counts when it is `boundp' now, is not a parameter or local
of FORM, is not a keyword or constant, and is not in function
position (`confirm-nonexistent-file-or-buffer' is both a function and
an option; `find-file' reads the option)."
  (let* ((locals (efrit-tool-contract--bound-parameters form))
         (set (efrit-tool-contract--set-targets form))
         (seen nil))
    (efrit-tool-contract--walk
     form
     (lambda (x)
       (when (and (symbolp x) x (not (keywordp x)) (not (memq x '(t nil)))
                  (boundp x) (not (memq x locals)))
         (cl-pushnew x seen)))
     'values-only)
    ;; a setq of a parameter or a let-bound local is not a global write;
    ;; a hook named only to be run is reported under runs_hooks
    (let ((all (cl-set-difference (cl-union seen (cl-set-difference set locals))
                                  (efrit-tool-contract--hooks form))))
      (cons (cl-remove-if-not #'custom-variable-p all)
            (cl-remove-if #'custom-variable-p all)))))

(defun efrit-tool-contract--hooks (form)
  "Hook variables FORM runs, as symbols.
`run-hooks' takes any number of hooks; the other runners take one
hook and then arguments, so only their first argument counts."
  (let ((hooks nil))
    (efrit-tool-contract--walk
     form
     (lambda (x)
       (when (and (consp x)
                  (memq (car x) '(run-hooks run-hook-with-args run-hook-with-args-until-success
                                            run-hook-with-args-until-failure run-hook-wrapped run-mode-hooks)))
         (dolist (arg (if (eq (car x) 'run-hooks) (cdr x) (list (cadr x))))
           (let ((sym (if (and (consp arg) (eq (car arg) 'quote)) (cadr arg) arg)))
             (when (and (symbolp sym) sym (string-suffix-p "-hook" (symbol-name sym)))
               (cl-pushnew sym hooks)))))))
    (nreverse hooks)))

(defconst efrit-tool-contract--position-regexp
  "\\b\\(?:at point\\|point\\b\\|the region\\|region\\b\\|the mark\\|mark\\b\\|minibuffer\\|current buffer\\|selected window\\)"
  "Docstring words that mean the function depends on where it is called.")

(defun efrit-tool-contract--depends-on (docstring)
  "Positional dependencies DOCSTRING admits to, as a list of strings."
  (when (stringp docstring)
    (let ((found nil) (start 0))
      (while (string-match efrit-tool-contract--position-regexp docstring start)
        (cl-pushnew (downcase (string-trim (match-string 0 docstring))) found :test #'equal)
        (setq start (match-end 0)))
      (nreverse found))))

(defun efrit-tool-contract--definition-mentions-p (definition symbol)
  "Non-nil when function DEFINITION's body or constants name SYMBOL."
  (let ((found nil))
    (cl-labels ((walk (x depth)
                  (unless (or found (> depth 40))
                    (cond ((eq x symbol) (setq found t))
                          ((consp x) (walk (car x) (1+ depth)) (walk (cdr x) (1+ depth)))
                          ((vectorp x) (mapc (lambda (e) (walk e (1+ depth))) x))
                          ((byte-code-function-p x)
                           (walk (aref x 2) (1+ depth)))
                          ((and (fboundp 'interpreted-function-p) (interpreted-function-p x))
                           (walk (aref x 1) (1+ depth)))))))
      (walk definition 0))
    found))

(defun efrit-tool-contract--callers (symbol)
  "Loaded functions whose definitions name SYMBOL, the user's own first.
From `load-history' (what this Emacs loaded, file by file), not a
grep: a caller in the user's configuration or in an installed package
is found whatever project is current.  Each is (FILE . CALLER-NAME);
files under the project root or the user's Emacs directory sort
before installed packages and Emacs itself.  Native-compiled
definitions carry no readable body and are not seen; their source
file is still the place to look."
  (let* ((root (ignore-errors (efrit-tool--get-project-root)))
         (own (lambda (file)
                (and (stringp file)
                     (or (and root (string-prefix-p (file-truename root) (file-truename file)))
                         (string-prefix-p (file-truename user-emacs-directory) (file-truename file))))))
         (hits nil))
    (dolist (entry load-history)
      (let ((file (car entry)))
        (when (stringp file)
          (dolist (item (cdr entry))
            (let ((fn (cond ((and (consp item) (eq (car item) 'defun)) (cdr item))
                            ((symbolp item) item))))
              (when (and fn (not (eq fn symbol)) (fboundp fn)
                         ;; an alias's cell is the symbol itself: not a call
                         (not (symbolp (symbol-function fn)))
                         (not (autoloadp (symbol-function fn)))
                         (efrit-tool-contract--definition-mentions-p (symbol-function fn) symbol))
                (push (cons file fn) hits)))))))
    (sort (nreverse hits)
          (lambda (a b) (and (funcall own (car a)) (not (funcall own (car b))))))))

(defun efrit-tool-contract--names (symbols &optional set)
  "SYMBOLS as a vector of names, a \"!\" after those in SET, cut to the option."
  (let* ((names (mapcar (lambda (s) (concat (symbol-name s) (if (memq s set) "!" ""))) symbols))
         (sorted (sort names #'string<)))
    (vconcat (if (> (length sorted) efrit-tool-contract-max-variables)
                 (append (seq-take sorted efrit-tool-contract-max-variables)
                         (list (format "… +%d more" (- (length sorted) efrit-tool-contract-max-variables))))
               sorted))))

(defun efrit-tool-library-contract (args)
  "What to know before calling or rebinding the function ARGS's `symbol' names.
See the Commentary for the fields.  ARGS: symbol (required);
include_callers (default t)."
  (efrit-tool-execute library_contract args
    (let* ((name (or (alist-get 'symbol args) ""))
           (symbol (intern-soft name))
           (include-callers (let ((v (alist-get 'include_callers args))) (if (null v) t (not (eq v :json-false))))))
      (when (string-empty-p name) (signal 'user-error (list "symbol is required")))
      (unless (and symbol (fboundp symbol))
        (signal 'user-error
                (list (if symbol
                          (format "%s is not a function in this Emacs (use symbol_info for a variable)" name)
                        (format "%s is not interned in this Emacs: nothing defines it (yet); is its library loaded?" name)))))
      (let* ((raw (symbol-function symbol))
             (definition (indirect-function symbol))
             (kind (efrit-tool-contract--kind symbol raw))
             (loc (efrit-tool-introspect--file-line symbol 'function))
             (doc (ignore-errors (documentation symbol t)))
             (first-paragraph (and doc (car (split-string doc "\n\n" t))))
             ;; an autoload's body is on disk already: `symbol-file'
             ;; knows the library, the form is read from there
             (form (unless (subrp definition)
                     (efrit-tool-contract--source-form symbol)))
             (vars (and form (efrit-tool-contract--variables form)))
             (set (and form (cl-set-difference (efrit-tool-contract--set-targets form)
                                               (efrit-tool-contract--bound-parameters form))))
             (callers (and include-callers (efrit-tool-contract--callers symbol)))
             (shown-callers (seq-take callers efrit-tool-contract-max-callers)))
        (efrit-tool-success
         `((symbol . ,name)
           (kind . ,kind)
           ,@(when (and (symbolp raw) raw) `((alias_of . ,(symbol-name raw))))
           ,@(when (autoloadp raw) `((autoloads_from . ,(format "%s" (cadr raw)))))
           (signature . ,(ignore-errors (format "%s" (help-function-arglist symbol t))))
           (interactive . ,(if (commandp symbol) t :json-false))
           ,@(when (commandp symbol)
               (let ((spec (ignore-errors (cadr (interactive-form symbol)))))
                 (when spec `((interactive_spec . ,(format "%S" spec))))))
           (file . ,(car loc))
           (line . ,(cdr loc))
           (declarations . ,(or (efrit-tool-contract--declarations symbol) :json-null))
           (advice . ,(vconcat (efrit-tool-introspect--advice symbol)))
           (doc . ,(or first-paragraph :json-null))
           (depends_on . ,(vconcat (efrit-tool-contract--depends-on doc)))
           ,@(if form
                 `((reads_options . ,(efrit-tool-contract--names (car vars) set))
                   (reads_variables . ,(efrit-tool-contract--names (cdr vars) set))
                   (runs_hooks . ,(vconcat (mapcar #'symbol-name (efrit-tool-contract--hooks form)))))
               `((source . ,(cond ((subrp definition) "primitive (C)")
                                  ((autoloadp raw) "not loaded and the source was not found; (require …) its library")
                                  (t "not found")))))
           ,@(when include-callers
               `((callers . ,(vconcat (mapcar (lambda (c) (format "%s in %s" (cdr c) (abbreviate-file-name (car c)))) shown-callers)))
                 (callers_omitted . ,(max 0 (- (length callers) (length shown-callers))))))))))))

(provide 'efrit-tool-contract)

;;; efrit-tool-contract.el ends here
