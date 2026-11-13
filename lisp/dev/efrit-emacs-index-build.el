;;; efrit-emacs-index-build.el --- Build the Emacs library index  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.22.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; Reads every library of an Emacs Lisp tree WITHOUT loading it and
;; writes what each one touches on its own: the programs it runs, the
;; state and configuration files it reads or writes, the hosts it
;; contacts, whether it prompts, what it requires, the options that
;; name those things.  The result is `efrit-emacs-index.eld', read by
;; efrit-emacs-index.el (see there for who uses it and why).
;;
;; Static, by `read': a form is data here.  Defaults are classified
;; by the option's name and :type and by the shape of the default
;; (`locate-user-emacs-file', `expand-file-name' under
;; `user-emacs-directory', a literal path, a literal program name).
;; What cannot be told statically is left out; the runtime side reads
;; the live value of the option when it needs the real path.
;;
;; The Info manuals are indexed too: every index entry (concept,
;; variable, function, key, program, type), with the manual and node it
;; points to, from the Texinfo sources of an Emacs checkout (doc/,
;; with the implicit entries of @defun, @defopt, @deffn…) or from the
;; installed .info files (their index nodes).  Functions and variables
;; in those indices are linked back to the library that defines them,
;; so a library's entry says where the manual documents it.
;;
;; The index is a build artifact, pinned to one Emacs: `make
;; emacs-index' (see the Makefile) indexes the $(EMACS) binary's own
;; installation, EMACS_SRC=DIR a source checkout, EMACS_SRC=DIR
;; EMACS_REF=REF one release or commit of it without touching the
;; checkout.  CI builds one per Emacs it tests.  M-x
;; efrit-emacs-index-rebuild builds one for the running Emacs into the
;; data directory.  Batch use:
;;
;;   emacs -Q --batch -L lisp/dev -l efrit-emacs-index-build \
;;         -f efrit-emacs-index-build-batch KEY=VALUE...
;;
;; Keys: lisp=DIR doc=DIR info=DIR (repeatable) installed (this
;; Emacs's lisp and Info directories) configure=FILE (the version from
;; AC_INIT) revision=SHA ref=NAME out=FILE.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst efrit-emacs-index-build-version 3
  "Format version of the .eld file; bump when the entry shape changes.
Must equal `efrit-emacs-index-format' in efrit-emacs-index.el (a test
checks).")

(defconst efrit-emacs-index-build--definers
  '(defun defmacro defsubst cl-defun cl-defmacro cl-defgeneric define-inline
    defcustom defvar defvar-local defconst defface
    define-minor-mode define-globalized-minor-mode define-derived-mode
    transient-define-prefix transient-define-suffix transient-define-infix)
  "Forms whose second element names what the library defines.")

(defconst efrit-emacs-index-build--read-ops
  '(insert-file-contents insert-file-contents-literally file-exists-p file-readable-p
    file-attributes directory-files directory-files-recursively find-file-noselect
    file-has-changed-p file-newer-than-file-p load-file load))
(defconst efrit-emacs-index-build--write-ops
  '(write-region write-file delete-file rename-file copy-file make-directory
    delete-directory make-symbolic-link set-file-modes with-temp-file))
(defconst efrit-emacs-index-build--process-ops
  '(call-process call-process-region start-process make-process process-file
    start-file-process shell-command shell-command-to-string async-shell-command
    process-lines executable-find))
(defconst efrit-emacs-index-build--network-ops
  '(open-network-stream make-network-process url-retrieve url-retrieve-synchronously
    url-insert-file-contents open-gnutls-stream url-http))
(defconst efrit-emacs-index-build--prompt-ops
  '(read-string read-from-minibuffer yes-or-no-p y-or-n-p read-multiple-choice
    completing-read read-passwd read-char-choice read-file-name read-key read-answer))
(defconst efrit-emacs-index-build--auth-ops
  '(auth-source-search auth-source-pick-first-password auth-info-password))

(defconst efrit-emacs-index-build--skip-regexp
  "/\\(?:leim/\\(?:quail\\|ja-dic\\)\\|language/\\(?:ja\\|ind\\|tib\\|eth\\|thai\\|lao\\|viet\\|khmer\\|burmese\\|sinhala\\|cham\\|tai\\|misc\\|utf\\|cyril\\|chin\\|kore\\|europ\\|greek\\|hebr\\|ceb\\|geor\\|slov\\|czech\\|roman\\|engl\\|japan\\)\\)"
  "Input-method tables and language environments: data, not libraries.")

;;;; Walking forms

(defun efrit-emacs-index-build--walk (form fn)
  "Call FN on FORM and every subform (conses, vectors), depth first."
  (funcall fn form)
  (cond
   ((consp form)
    (while (consp form)
      (efrit-emacs-index-build--walk (car form) fn)
      (setq form (cdr form)))
    (when form (funcall fn form)))
   ((vectorp form) (mapc (lambda (x) (efrit-emacs-index-build--walk x fn)) form))))

(defun efrit-emacs-index-build--head-symbols (form table)
  "Record in TABLE every symbol in function position (and bare symbols) of FORM."
  (efrit-emacs-index-build--walk
   form (lambda (x)
          (cond ((and (consp x) (symbolp (car x))) (puthash (car x) t table))
                ((symbolp x) (puthash x t table))))))

(defun efrit-emacs-index-build--strings (form)
  "Every string literal in FORM."
  (let (out)
    (efrit-emacs-index-build--walk form (lambda (x) (when (stringp x) (push x out))))
    (nreverse out)))

;;;; Classifying defaults

(defun efrit-emacs-index-build--program-option-p (name)
  (string-match-p "-\\(?:program\\|executable\\|command-name\\|program-name\\|binary\\)\\'" (symbol-name name)))

(defun efrit-emacs-index-build--file-option-p (name type)
  "Non-nil when option NAME (with custom TYPE) names a file or directory."
  (or (string-match-p "-\\(?:file\\|file-name\\|filename\\|directory\\|dir\\|save-file\\|history-file\\|cache-file\\|data-file\\)\\'"
                      (symbol-name name))
      (efrit-emacs-index-build--type-file-p type)))

(defun efrit-emacs-index-build--type-file-p (type)
  "Non-nil when the custom TYPE is or contains `file' or `directory'."
  (let ((type (if (and (consp type) (eq (car type) 'quote)) (cadr type) type)))
    (cond ((memq type '(file directory)) t)
          ((consp type)
           (or (memq (car type) '(file directory))
               (and (memq (car type) '(choice radio))
                    (cl-some #'efrit-emacs-index-build--type-file-p (cdr type))))))))

(defun efrit-emacs-index-build--render-path (default)
  "DEFAULT (a form) as a path template string, or nil when it is not one.
`user-emacs-directory' renders as ~/.emacs.d/ and `temporary-file-directory'
as $TMPDIR/; the runtime reads the live option for the real value."
  (pcase default
    ((pred stringp)
     (and (string-match-p "\\`\\(?:~\\|/\\|\\$\\)" default) default))
    (`(locate-user-emacs-file ,(and (pred stringp) new) . ,_)
     (concat "~/.emacs.d/" new))
    (`(locate-user-emacs-file (quote (,(and (pred stringp) new) . ,_)) . ,_)
     (concat "~/.emacs.d/" new))
    (`(expand-file-name ,(and (pred stringp) name) user-emacs-directory)
     (concat "~/.emacs.d/" name))
    (`(expand-file-name ,(and (pred stringp) name) temporary-file-directory)
     (concat "$TMPDIR/" name))
    (`(expand-file-name ,(and (pred stringp) name))
     (and (string-match-p "\\`~" name) name))
    (`(expand-file-name ,(and (pred stringp) name) ,(and (pred stringp) dir))
     (and (string-match-p "\\`\\(?:~\\|/\\)" dir) (expand-file-name name dir)))
    (`(concat user-emacs-directory ,(and (pred stringp) name))
     (concat "~/.emacs.d/" name))
    (`(convert-standard-filename ,inner)
     (efrit-emacs-index-build--render-path inner))
    (`(file-name-concat user-emacs-directory ,(and (pred stringp) name) . ,_)
     (concat "~/.emacs.d/" name))
    (`(or ,a . ,_) (efrit-emacs-index-build--render-path a))
    (`(if ,_ ,a ,b) (or (efrit-emacs-index-build--render-path a)
                        (efrit-emacs-index-build--render-path b)))
    (`(cond . ,clauses)
     (cl-some (lambda (c) (and (consp c) (efrit-emacs-index-build--render-path (car (last c))))) clauses))
    (`(purecopy ,inner) (efrit-emacs-index-build--render-path inner))
    (`(quote ,(and (pred stringp) s)) (efrit-emacs-index-build--render-path s))
    (_ nil)))

(defun efrit-emacs-index-build--program-names (default)
  "Program names a program option's DEFAULT can take: its string literals' first words."
  (cl-remove-duplicates
   (delq nil (mapcar (lambda (s)
                       (let ((word (car (split-string s "[ \t]+" t))))
                         (and word (string-match-p "\\`[A-Za-z][A-Za-z0-9+_.-]*\\'" word)
                              (not (string-match-p "\\`\\(?:nil\\|t\\|%s\\)\\'" word))
                              word)))
                     (efrit-emacs-index-build--strings default)))
   :test #'equal))

(defun efrit-emacs-index-build--call-programs (form)
  "Programs FORM starts by literal name: executable-find, call-process, start-process…"
  (let (out)
    (efrit-emacs-index-build--walk
     form
     (lambda (x)
       (when (consp x)
         (let ((prog (pcase x
                       (`(executable-find ,(and (pred stringp) p) . ,_) p)
                       (`(,(or 'call-process 'process-file) ,(and (pred stringp) p) . ,_) p)
                       (`(,(or 'start-process 'start-file-process) ,_ ,_ ,(and (pred stringp) p) . ,_) p)
                       (`(,(or 'process-lines 'process-lines-ignore-status) ,(and (pred stringp) p) . ,_) p)
                       (`(,(or 'shell-command-to-string 'shell-command 'async-shell-command) ,(and (pred stringp) line) . ,_)
                        (car (split-string line "[ \t]+" t)))
                       (_ nil))))
           (when (and prog (string-match-p "\\`[A-Za-z][A-Za-z0-9+_.-]*\\'" prog)
                      (not (member prog '("sh" "cmd" "cmdproxy" "emacs"))))
             (push prog out))))))
    (cl-remove-duplicates (nreverse out) :test #'equal)))

(defun efrit-emacs-index-build--hosts (strings)
  "Host names of the URL literals in STRINGS."
  (cl-remove-duplicates
   (delq nil (mapcar (lambda (s)
                       (and (string-match "\\`[a-z]+://\\([A-Za-z0-9.-]+\\.[a-z]+\\)" s)
                            (match-string 1 s)))
                     strings))
   :test #'equal))

(defun efrit-emacs-index-build--literal-paths (strings)
  "Absolute or home-relative path literals in STRINGS, minus noise."
  (cl-remove-duplicates
   (cl-remove-if-not
    (lambda (s)
      (and (< (length s) 80) (> (length s) 2)
           (string-match-p "\\`\\(?:~/\\.?[A-Za-z]\\|/\\(?:etc\\|usr\\|var\\|opt\\|dev\\|proc\\|sys\\|Library\\|Applications\\|bin\\|sbin\\)/\\)" s)
           (not (string-match-p "[%*?\n]" s))))
    strings)
   :test #'equal))

;;;; One file

(defun efrit-emacs-index-build--header (file)
  "The (SUMMARY KEYWORDS OBSOLETE) of FILE from its first lines."
  (with-temp-buffer
    ;; a compressed file cannot be read by byte range; the whole of it
    ;; is small enough
    (if (string-suffix-p ".gz" file)
        (insert-file-contents file)
      (insert-file-contents file nil 0 4000))
    (let ((summary (and (goto-char (point-min))
                        (re-search-forward "\\`;;;[ \t]+[^ \t]+\\.el[ \t]+---[ \t]+\\(.*?\\)[ \t]*\\(?:-\\*-.*\\)?$" nil t)
                        (string-trim (match-string 1))))
          (keywords (and (goto-char (point-min))
                         (re-search-forward "^;;[ \t]+Keywords:[ \t]*\\(.*\\)$" nil t)
                         (split-string (match-string 1) "[ \t,]+" t)))
          (obsolete (and (goto-char (point-min))
                         (re-search-forward "^;;[ \t]+Obsolete-since:" nil t) t)))
      (list summary keywords obsolete))))

(defun efrit-emacs-index-build-file (file root)
  "The index entry of FILE under ROOT, a plist, or nil when it is not a library."
  (let* ((syms (make-hash-table :test 'eq))
         (strings nil) (provides nil) (requires nil) (hooks nil)
         (options nil) (programs nil) (files nil) (defines nil)
         (autoloads 0) (commands 0) (forms 0))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (progn (skip-chars-forward " \t\n") (not (eobp)))
            (let ((cookie (looking-at ";;;###autoload")))
              (when cookie (cl-incf autoloads))
              (forward-comment (buffer-size))
              (unless (eobp)
                (condition-case nil
                    (let ((form (read (current-buffer))))
                      (cl-incf forms)
                      (efrit-emacs-index-build--head-symbols form syms)
                      (setq strings (nconc (efrit-emacs-index-build--strings form) strings))
                      (setq programs (nconc (efrit-emacs-index-build--call-programs form) programs))
                      (pcase form
                        (`(,(pred (lambda (h) (memq h efrit-emacs-index-build--definers)))
                           ,(and (pred symbolp) name) . ,_)
                         (push name defines))
                        (`(defalias (quote ,(and (pred symbolp) name)) . ,_)
                         (push name defines)))
                      (pcase form
                        (`(provide (quote ,f) . ,_) (push f provides))
                        (`(require (quote ,f) . ,_) (push f requires))
                        (`(,(or 'eval-and-compile 'eval-when-compile 'progn) . ,body)
                         (dolist (b body)
                           (pcase b (`(require (quote ,f) . ,_) (push f requires)))))
                        (`(defcustom ,(and (pred symbolp) name) ,default . ,rest)
                         (let ((type (plist-get (cdr rest) :type)))
                           (cond
                            ((string-match-p "-\\(?:hook\\|functions\\)\\'" (symbol-name name))
                             (push name hooks))
                            ((efrit-emacs-index-build--program-option-p name)
                             (let ((names (efrit-emacs-index-build--program-names default)))
                               (push (list name (car names) 'program) options)
                               (setq programs (nconc names programs))))
                            ((efrit-emacs-index-build--file-option-p name type)
                             (let ((path (efrit-emacs-index-build--render-path default)))
                               ;; a bare ~/ or / is a base directory, not a
                               ;; file the library owns: kept as the option,
                               ;; not as a file
                               (push (list name path 'file) options)
                               (when (and path (not (member (file-name-as-directory path) '("~/" "/"))))
                                 (push path files)))))))
                        (`(defvar ,(and (pred symbolp) name) . ,_)
                         (when (string-match-p "-\\(?:hook\\|functions\\)\\'" (symbol-name name))
                           (push name hooks)))
                        (`(,(or 'defun 'cl-defun 'defmacro) ,_ ,_ . ,body)
                         (when (cl-some (lambda (b) (and (consp b) (eq (car b) 'interactive))) (seq-take body 3))
                           (cl-incf commands)))))
                  ((invalid-read-syntax end-of-file) (forward-line 1)))))))
      (error nil))
    (when (> forms 0)
      (let* ((hit (lambda (set) (cl-remove-if-not (lambda (s) (gethash s syms)) set)))
             (header (efrit-emacs-index-build--header file))
             ;; an installed Emacs ships foo.el.gz: the name is foo
             (plain (string-remove-suffix ".gz" file))
             (rel (file-relative-name plain root))
             (name (file-name-base plain)))
        (list :name name
               :file rel
               :summary (nth 0 header)
               :keywords (nth 1 header)
               :obsolete (or (nth 2 header) (string-prefix-p "obsolete/" rel))
               :provides (cl-remove-duplicates (nreverse provides))
               :requires (cl-remove-duplicates (nreverse requires))
               :commands commands
               :autoloads autoloads
               :options (nreverse options)
               :programs (cl-remove-duplicates (nreverse programs) :test #'equal)
               :files (cl-remove-duplicates (nreverse files) :test #'equal)
               :paths (efrit-emacs-index-build--literal-paths strings)
               :hosts (efrit-emacs-index-build--hosts strings)
               :hooks (cl-remove-duplicates (nreverse hooks))
               :reads (funcall hit efrit-emacs-index-build--read-ops)
               :writes (funcall hit efrit-emacs-index-build--write-ops)
               :processes (funcall hit efrit-emacs-index-build--process-ops)
               :network (funcall hit efrit-emacs-index-build--network-ops)
               :prompts (funcall hit efrit-emacs-index-build--prompt-ops)
               :auth (funcall hit efrit-emacs-index-build--auth-ops)
               ;; used to link manual entries to libraries, then dropped
               :defines defines)))))

(defun efrit-emacs-index-build--tidy (entry)
  "ENTRY without nil, empty and zero values (the file stays small)."
  (let (out)
    (while entry
      (let ((k (pop entry)) (v (pop entry)))
        (unless (or (null v) (and (numberp v) (zerop v)))
          (push k out) (push v out))))
    (nreverse out)))

;;;; The Info manuals

(defconst efrit-emacs-index-build--texi-index-kinds
  '(("cindex" . c) ("vindex" . v) ("findex" . f) ("kindex" . k) ("pindex" . p) ("tindex" . t))
  "Texinfo index commands and the kind of entry each makes.")

(defconst efrit-emacs-index-build--texi-def-kinds
  '(("defun" . f) ("defunx" . f) ("defmac" . f) ("defmacx" . f) ("defspec" . f) ("defspecx" . f)
    ("defcmd" . f) ("defcmdx" . f) ("defopt" . v) ("defoptx" . v) ("defvar" . v) ("defvarx" . v)
    ("deffn" . f*) ("deffnx" . f*) ("defvr" . v*) ("defvrx" . v*) ("deftp" . t*) ("deftpx" . t*))
  "Texinfo definition commands and the index entry each makes implicitly.
A starred kind takes a category word (or {braced words}) before the name.")

(defun efrit-emacs-index-build--texi-plain (text)
  "TEXT with the common Texinfo markup taken off: @code{x} is x."
  (let ((s text))
    (dotimes (_ 3)
      (setq s (replace-regexp-in-string
               "@\\(?:code\\|var\\|samp\\|kbd\\|key\\|file\\|env\\|command\\|option\\|asis\\|emph\\|strong\\|dfn\\|acronym\\|abbr\\|t\\|r\\|i\\|b\\|sc\\|w\\|url\\|uref\\|email\\|indicateurl\\|cite\\|var\\|math\\|verb\\){\\([^{}]*\\)}"
               "\\1" s t)))
    (setq s (replace-regexp-in-string "@\\(?:comma\\){}" "," s t t))
    (setq s (replace-regexp-in-string "@\\(?:dots\\|enddots\\){}" "..." s t t))
    (setq s (replace-regexp-in-string "@\\([@{}]\\)" "\\1" s t))
    (string-trim s)))

(defun efrit-emacs-index-build--texi-values (file)
  "NAME -> VALUE of the @set lines in FILE and the files it @includes, first wins."
  (let ((values (make-hash-table :test 'equal)) (dir (file-name-directory file)))
    (cl-labels ((scan (f depth)
                  (when (and (file-readable-p f) (< depth 3))
                    (with-temp-buffer
                      (insert-file-contents f)
                      (goto-char (point-min))
                      (while (re-search-forward "^@\\(set\\|include\\)[ \t]+\\([^ \t\n]+\\)[ \t]*\\(.*\\)$" nil t)
                        (let ((cmd (match-string 1)) (a (match-string 2)) (b (match-string 3)))
                          (if (equal cmd "set")
                              (unless (gethash a values) (puthash a (string-trim b) values))
                            (scan (expand-file-name a dir) (1+ depth)))))))))
      (scan file 0))
    values))

(defun efrit-emacs-index-build--texi-manual-header (file)
  "(MANUAL . TITLE) when FILE starts a manual (an @setfilename near the top), else nil."
  (with-temp-buffer
    (insert-file-contents file nil 0 3000)
    (goto-char (point-min))
    (when (re-search-forward "^@setfilename[ \t]+\\(.+\\)$" nil t)
      (let ((manual (file-name-sans-extension (file-name-nondirectory (string-trim (match-string 1)))))
            (title (and (re-search-forward "^@settitle[ \t]+\\(.+\\)$" nil t)
                        (match-string 1))))
        (cons manual
              (and title
                   (let ((values (and (string-match-p "@value{" title)
                                      (efrit-emacs-index-build--texi-values file))))
                     (efrit-emacs-index-build--texi-plain
                      (replace-regexp-in-string
                       "@value{\\([^}]+\\)}"
                       (lambda (m) (or (and values (gethash (match-string 1 m) values)) ""))
                       title t t)))))))))

(defun efrit-emacs-index-build--texi-scan (file state &optional depth)
  "Add FILE's nodes and index entries to STATE, following @include of files
that are not manuals of their own.  STATE is a plist (:node :nodes :entries)."
  (let ((dir (file-name-directory file)) (depth (or depth 0)))
    (when (and (file-readable-p file) (< depth 6))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (cond
             ((string-match "\\`@node[ \t]+\\([^,]+\\)" line)
              (let ((node (efrit-emacs-index-build--texi-plain (match-string 1 line))))
                (plist-put state :node node)
                (push node (plist-get state :nodes))
                (plist-put state :nodes (plist-get state :nodes))))
             ((string-match "\\`@include[ \t]+\\(.+\\)$" line)
              (let ((inc (expand-file-name (string-trim (match-string 1 line)) dir)))
                (unless (or (not (file-readable-p inc)) (efrit-emacs-index-build--texi-manual-header inc))
                  (efrit-emacs-index-build--texi-scan inc state (1+ depth)))))
             ((string-match "\\`@\\([cvfkpt]index\\)[ \t]+\\(.+\\)$" line)
              (when-let* ((node (plist-get state :node))
                          (kind (cdr (assoc (match-string 1 line) efrit-emacs-index-build--texi-index-kinds)))
                          (term (efrit-emacs-index-build--texi-plain (match-string 2 line))))
                (unless (string-empty-p term)
                  (plist-put state :entries (cons (list term kind node) (plist-get state :entries))))))
             ((string-match "\\`@\\(def[a-z]+\\)[ \t]+\\(.+\\)$" line)
              (when-let* ((node (plist-get state :node))
                          (spec (cdr (assoc (match-string 1 line) efrit-emacs-index-build--texi-def-kinds))))
                (let* ((rest (match-string 2 line))
                       (rest (if (memq spec '(f* v* t*))
                                 ;; skip the category: {Interactive Command} or one word
                                 (if (string-match "\\`{[^}]*}[ \t]+\\(.*\\)" rest) (match-string 1 rest)
                                   (and (string-match "\\`[^ \t]+[ \t]+\\(.*\\)" rest) (match-string 1 rest)))
                               rest))
                       (name (and rest (car (split-string (efrit-emacs-index-build--texi-plain rest) "[ \t]+" t))))
                       (kind (pcase spec ('f* 'f) ('v* 'v) ('t* 't) (k k))))
                  (when (and name (not (string-empty-p name)))
                    (plist-put state :entries (cons (list name kind node) (plist-get state :entries))))))))
            (forward-line 1)))))
    state))

(defun efrit-emacs-index-build--texi-manuals (doc-dir)
  "The manuals under DOC-DIR (Texinfo sources) as plists (:name :title :from :entries)."
  (let (manuals)
    (dolist (f (directory-files-recursively doc-dir "\\.texi\\'"))
      (when-let* ((head (efrit-emacs-index-build--texi-manual-header f)))
        (let ((state (efrit-emacs-index-build--texi-scan f (list :node nil :nodes nil :entries nil))))
          (when (plist-get state :entries)
            (push (list :name (car head) :title (cdr head) :from 'texi
                        :entries (nreverse (plist-get state :entries)))
                  manuals)))))
    (nreverse manuals)))

(defconst efrit-emacs-index-build--info-entry-regexp
  "\n\\* +\\([^\n]*?\\):[ \t]+\\([^\n]+?\\)\\.\\(?:[ \t\n]*(line +[0-9]+)\\)?"
  "An index menu line of an .info file (what `Info-apropos-matches' reads).")

(defun efrit-emacs-index-build--info-kind (node)
  "The kind of the entries in index NODE, from its name."
  (cond ((string-match-p "Variable\\|Option" node) 'v)
        ((string-match-p "Function\\|Command\\|Macro" node) 'f)
        ((string-match-p "Key" node) 'k)
        ((string-match-p "Program" node) 'p)
        ((string-match-p "Type" node) 't)
        (t 'c)))

(defun efrit-emacs-index-build--info-files (dirs)
  "MANUAL -> list of its .info files (main and split parts) found in DIRS, first dir wins."
  (let ((by-manual (make-hash-table :test 'equal)) (seen-in (make-hash-table :test 'equal)))
    (dolist (dir dirs)
      (when (and (stringp dir) (file-directory-p dir))
        (dolist (f (directory-files dir t "\\.info\\(?:-[0-9]+\\)?\\(?:\\.gz\\|\\.bz2\\|\\.xz\\)?\\'"))
          (let ((manual (replace-regexp-in-string
                         "\\.info\\(?:-[0-9]+\\)?\\(?:\\.gz\\|\\.bz2\\|\\.xz\\)?\\'" "" (file-name-nondirectory f))))
            (unless (and (gethash manual seen-in) (not (equal (gethash manual seen-in) dir)))
              (puthash manual dir seen-in)
              (push f (gethash manual by-manual)))))))
    by-manual))

(defun efrit-emacs-index-build--info-manual (manual files)
  "The plist of installed MANUAL read from its FILES, or nil when it has no index."
  (let ((entries nil) (title nil))
    (dolist (f files)
      (with-temp-buffer
        (condition-case nil
            (insert-file-contents f)
          (error nil))
        (goto-char (point-min))
        (unless title
          (when (re-search-forward "^START-INFO-DIR-ENTRY\n\\* [^:]+: ([^)]+)[^ \t\n]*\\.[ \t]+\\(.+\\)$" nil t)
            (setq title (string-trim (match-string 1))))
          (goto-char (point-min)))
        (while (search-forward "\0\b[index\0\b]" nil t)
          (let* ((here (point))
                 (node (save-excursion
                         (and (re-search-backward "^\^_\nFile: [^,]+,[ \t]+Node: \\([^,\n]+\\)" nil t)
                              (match-string-no-properties 1))))
                 (kind (and node (efrit-emacs-index-build--info-kind node)))
                 (end (save-excursion (if (search-forward "\^_" nil t) (point) (point-max)))))
            (goto-char here)
            (while (re-search-forward efrit-emacs-index-build--info-entry-regexp end t)
              (let ((term (replace-regexp-in-string "[ \t]+<[0-9]+>\\'" "" (match-string-no-properties 1)))
                    (target (string-trim (match-string-no-properties 2))))
                (unless (string-empty-p term)
                  (push (list term kind target) entries))))
            (goto-char end)))))
    (when entries
      (list :name manual :title title :from 'info :entries (nreverse entries)))))

(defun efrit-emacs-index-build--info-manuals (dirs)
  "The installed manuals in DIRS that have index nodes, as plists."
  (let ((by-manual (efrit-emacs-index-build--info-files dirs)) out)
    (maphash (lambda (manual files)
               (when-let* ((m (efrit-emacs-index-build--info-manual manual (sort files #'string<))))
                 (push m out)))
             by-manual)
    (sort out (lambda (a b) (string< (plist-get a :name) (plist-get b :name))))))

(defun efrit-emacs-index-build--compact-manual (manual)
  "MANUAL with its node names in a vector and entries as (TERM KIND NODE-INDEX), duplicates gone."
  (let ((nodes (make-hash-table :test 'equal)) (order nil) (seen (make-hash-table :test 'equal)) (entries nil))
    (dolist (e (plist-get manual :entries))
      (pcase-let ((`(,term ,kind ,node) e))
        (unless (gethash node nodes)
          (puthash node (hash-table-count nodes) nodes)
          (push node order))
        (let ((key (list term kind node)))
          (unless (gethash key seen)
            (puthash key t seen)
            (push (list term kind (gethash node nodes)) entries)))))
    (list :name (plist-get manual :name) :title (plist-get manual :title) :from (plist-get manual :from)
          :nodes (vconcat (nreverse order)) :entries (nreverse entries))))

(defconst efrit-emacs-index-build-info-links-max 6
  "Most manual nodes recorded per library.")

(defun efrit-emacs-index-build--link-manuals (entries manuals)
  "Give each library in ENTRIES an :info list of \"(MANUAL)NODE\" where MANUALS
document a function or variable it defines.  Returns ENTRIES, without :defines."
  (let ((owner (make-hash-table :test 'equal)) (links (make-hash-table :test 'equal)))
    (dolist (e entries)
      (dolist (sym (plist-get e :defines))
        (puthash (symbol-name sym) (plist-get e :name) owner)))
    (dolist (m manuals)
      (let ((nodes (plist-get m :nodes)) (name (plist-get m :name)))
        (dolist (x (plist-get m :entries))
          (pcase-let ((`(,term ,kind ,idx) x))
            ;; an installed manual's one "Index" node mixes the kinds:
            ;; a concept entry that is exactly a defined name counts
            (when (or (memq kind '(f v))
                      (and (eq kind 'c) (string-match-p "\\`[a-z][a-z0-9]*-[^ ]+\\'" term)))
              (when-let* ((lib (gethash term owner)))
                (let ((ref (format "(%s)%s" name (aref nodes idx)))
                      (have (gethash lib links)))
                  (unless (or (member ref have) (>= (length have) efrit-emacs-index-build-info-links-max))
                    (puthash lib (append have (list ref)) links)))))))))
    (mapcar (lambda (e)
              (let ((out nil) (rest e))
                (while rest
                  (let ((k (pop rest)) (v (pop rest)))
                    (unless (eq k :defines) (push k out) (push v out))))
                (setq out (nreverse out))
                (if-let* ((refs (gethash (plist-get e :name) links)))
                    (append out (list :info refs))
                  out)))
            entries)))

;;;; The tree

(defun efrit-emacs-index-build (lisp-dir)
  "Index every library under LISP-DIR; return the list of entries (with :defines)."
  (let* ((root (file-name-as-directory (expand-file-name lisp-dir)))
         (files (cl-remove-if (lambda (f) (string-match-p efrit-emacs-index-build--skip-regexp f))
                              (directory-files-recursively root "\\.el\\(?:\\.gz\\)?\\'")))
         (entries nil) (n 0))
    (dolist (f files)
      (cl-incf n)
      (when (zerop (% n 200)) (message "efrit-emacs-index: %d/%d libraries" n (length files)))
      (when-let* ((e (efrit-emacs-index-build-file f root)))
        (push (efrit-emacs-index-build--tidy e) entries)))
    (sort (nreverse entries) (lambda (a b) (string< (plist-get a :file) (plist-get b :file))))))

(defun efrit-emacs-index-build--configure-version (file)
  "The version in AC_INIT of an Emacs configure.ac FILE, or nil."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (and (re-search-forward "^AC_INIT(\\[GNU Emacs\\],[ \t]*\\[\\([0-9.]+\\)\\]" nil t)
           (match-string 1)))))

(defvar Info-directory-list)
(defvar Info-default-directory-list)
(declare-function info-initialize "info")

(defun efrit-emacs-index-build-installed-dirs ()
  "(LISP-DIR . INFO-DIRS) of the running Emacs."
  (require 'info)
  (info-initialize)
  (cons (file-name-directory (locate-library "simple"))
        (delete-dups (mapcar #'file-name-as-directory
                             (cl-remove-if-not #'file-directory-p
                                               (append Info-directory-list Info-default-directory-list))))))

(cl-defun efrit-emacs-index-build-write (out-file &key lisp-dir doc-dir info-dirs version revision ref)
  "Index LISP-DIR, the Texinfo manuals under DOC-DIR and the .info files in
INFO-DIRS into OUT-FILE.  VERSION is the Emacs version the sources are
(default `emacs-version'), REVISION and REF what they were built from.
A manual found both as Texinfo and as .info is taken from the Texinfo.
Returns (LIBRARIES . MANUALS), the counts."
  (let* ((entries (and lisp-dir (efrit-emacs-index-build lisp-dir)))
         (texi (and doc-dir (file-directory-p doc-dir) (efrit-emacs-index-build--texi-manuals doc-dir)))
         (info (cl-remove-if (lambda (m) (cl-find (plist-get m :name) texi
                                                  :key (lambda (x) (plist-get x :name)) :test #'equal))
                             (and info-dirs (efrit-emacs-index-build--info-manuals info-dirs))))
         (manuals (mapcar #'efrit-emacs-index-build--compact-manual (append texi info)))
         (entries (efrit-emacs-index-build--link-manuals entries manuals)))
    (make-directory (file-name-directory (expand-file-name out-file)) t)
    (with-temp-file out-file
      (let ((print-length nil) (print-level nil) (print-circle nil) (print-escape-newlines t))
        (insert ";;; efrit-emacs-index.eld --- what each Emacs library touches, and where the manuals say so  -*- lisp-data -*-\n")
        (insert ";; Generated by efrit-emacs-index-build.el; a build artifact, do not edit or commit.\n")
        (prin1 (list :version efrit-emacs-index-build-version
                     :emacs (or version emacs-version)
                     :revision revision
                     :ref ref
                     :source (and lisp-dir (abbreviate-file-name (file-name-as-directory (expand-file-name lisp-dir))))
                     :built (format-time-string "%F")
                     :entries entries
                     :manuals manuals)
               (current-buffer))
        (insert "\n")))
    (cons (length entries) (length manuals))))

(defun efrit-emacs-index-build--args (args)
  "ARGS (KEY=VALUE strings, `installed') as a plist for `efrit-emacs-index-build-write'."
  (let (plist infos)
    (dolist (a args)
      (cond
       ((equal a "installed")
        (let ((dirs (efrit-emacs-index-build-installed-dirs)))
          (setq plist (plist-put plist :lisp-dir (or (plist-get plist :lisp-dir) (car dirs))))
          (setq infos (append infos (cdr dirs)))))
       ((string-match "\\`\\([a-z]+\\)=\\(.*\\)\\'" a)
        (let ((k (match-string 1 a)) (v (match-string 2 a)))
          (pcase k
            ("lisp" (setq plist (plist-put plist :lisp-dir v)))
            ("doc" (setq plist (plist-put plist :doc-dir v)))
            ("info" (push v infos))
            ("configure" (setq plist (plist-put plist :version (efrit-emacs-index-build--configure-version v))))
            ("version" (setq plist (plist-put plist :version v)))
            ("revision" (setq plist (plist-put plist :revision (and (not (string-empty-p v)) v))))
            ("ref" (setq plist (plist-put plist :ref (and (not (string-empty-p v)) v))))
            ("out" (setq plist (plist-put plist :out v)))
            (_ (error "efrit-emacs-index: unknown key %s" k)))))
       (t (error "efrit-emacs-index: argument %S is not KEY=VALUE" a))))
    (when infos (setq plist (plist-put plist :info-dirs (delete-dups infos))))
    plist))

(defun efrit-emacs-index-build-batch ()
  "Batch entry: KEY=VALUE arguments (see the Commentary); with none, this Emacs's installation."
  (let* ((args (or command-line-args-left '("installed")))
         (plist (efrit-emacs-index-build--args args))
         (out (or (plist-get plist :out) "lisp/core/efrit-emacs-index.eld")))
    (setq command-line-args-left nil)
    (unless (or (plist-get plist :lisp-dir) (plist-get plist :doc-dir) (plist-get plist :info-dirs))
      (error "efrit-emacs-index: nothing to index; give lisp=, doc=, info= or installed"))
    (let ((counts (efrit-emacs-index-build-write
                   out :lisp-dir (plist-get plist :lisp-dir) :doc-dir (plist-get plist :doc-dir)
                   :info-dirs (plist-get plist :info-dirs) :version (plist-get plist :version)
                   :revision (plist-get plist :revision) :ref (plist-get plist :ref))))
      (message "efrit-emacs-index: %d libraries, %d manuals, Emacs %s%s -> %s"
               (car counts) (cdr counts) (or (plist-get plist :version) emacs-version)
               (if (plist-get plist :revision) (format " (%s)" (plist-get plist :revision)) "")
               out))))

(provide 'efrit-emacs-index-build)

;;; efrit-emacs-index-build.el ends here
