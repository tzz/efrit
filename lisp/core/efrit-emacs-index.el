;;; efrit-emacs-index.el --- What each Emacs library touches on its own -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.22.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; On 2026-11-12 one `(mailcap-mime-info "application/pdf")' in an
;; eval asked the user four times about files the form never named:
;; mailcap's first call parses ~/.mailcap, /etc/mailcap and the
;; mime.types list.  Nobody in efrit knew that until it happened; the
;; fix was a hand-written list for one library.  There are 1500
;; libraries, and the same question comes up for every one the model
;; calls: what will this read, write, start, contact, or ask, on its
;; own, before the sandbox sees it?
;;
;; `efrit-emacs-index.eld' answers that for the whole tree, and says
;; where the manuals document each thing.  efrit-emacs-index-build.el
;; (lisp/dev) reads every library without loading it: the options that
;; name files and programs (with their default paths rendered), the
;; programs started by literal name, the configuration and state files
;; mentioned, the hosts in URL literals, which classes of primitive the
;; library calls (file reads/writes, processes, network, prompts,
;; auth).  It reads every index of the Info manuals too (concepts,
;; variables, functions, keys, programs), so `info_lookup' answers
;; "which node documents X" without opening Info, and each library
;; names the nodes that document what it defines.
;;
;; The file is a build artifact for one Emacs, never committed: `make
;; emacs-index' (pinned to an Emacs checkout, release or the installed
;; binary), CI for each Emacs it tests, or M-x efrit-emacs-index-rebuild
;; for the running Emacs into the data directory.  It is advisory:
;; everything in it is in Emacs already, it only takes time to find.
;; An index for another Emacs version is still read (most of it holds)
;; and the doctor says so; an index of another format is not read.
;;
;; Who reads it:
;;
;; - the sandbox: a file a library's state or configuration option
;;   names is an expected read or write when that library is on the
;;   stack (`efrit-emacs-index-expected-files'), so `savehist',
;;   `recentf', `mailcap', `tramp-cache', `transient' and the rest no
;;   longer need a hand-written list each;
;; - the sandbox prompt: when a read is a library's, the detail says
;;   which library and what else it touches;
;; - library_contract and the new `library_profile' tool: the model
;;   asks before calling and sees the programs, files, hosts and
;;   prompts a library brings, instead of finding out from a denial;
;; - the reviewer: a FLAG line when a proposed eval or edit names a
;;   library that starts programs, contacts hosts or prompts.
;;
;; The index is static knowledge about defaults.  The live value of an
;; option is what counts at run time; the index tells efrit WHICH
;; option to read.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-log)
(require 'efrit-config)

(defgroup efrit-emacs-index nil
  "The index of what Emacs libraries touch on their own."
  :group 'efrit)

(defconst efrit-emacs-index-file-name "efrit-emacs-index.eld")

(defconst efrit-emacs-index-format 3
  "The .eld format this file reads (`efrit-emacs-index-build-version').")

(defcustom efrit-emacs-index-file nil
  "The index file to read; nil means the rebuilt one in the data directory
when it exists, else the copy shipped with efrit."
  :type '(choice (const nil) file)
  :group 'efrit-emacs-index)

(defvar efrit-emacs-index--data nil
  "The parsed index plist (:version :emacs :source :built :entries), or nil until read.")

(defvar efrit-emacs-index--by-name nil
  "Hash: library name (string) -> entry plist.")

(defvar efrit-emacs-index--by-feature nil
  "Hash: feature symbol -> entry plist.")

(defvar efrit-emacs-index--option-owner nil
  "Hash: option symbol -> entry plist of the library that defines it.")

(defvar efrit-emacs-index--terms nil
  "Hash: downcased index term -> list of (TERM KIND MANUAL NODE), from every manual.")

(defvar efrit-emacs-index--problem nil
  "Why no index is loaded (a string), or nil.  For the doctor and the tools.")

(defun efrit-emacs-index-shipped-file ()
  "The index shipped beside this file."
  (expand-file-name efrit-emacs-index-file-name
                    (file-name-directory (or load-file-name buffer-file-name
                                             (locate-library "efrit-emacs-index")))))

(defun efrit-emacs-index-user-file ()
  "The rebuilt index in the data directory."
  (efrit-config-data-file efrit-emacs-index-file-name))

(defun efrit-emacs-index-current-file ()
  "The file the index is read from, or nil when none exists.
`efrit-emacs-index-file' when set; else the one built for this Emacs
in the data directory; else the one `make emacs-index' wrote beside
this file."
  (cl-find-if (lambda (f) (and f (file-readable-p f)))
              (list efrit-emacs-index-file (efrit-emacs-index-user-file) (efrit-emacs-index-shipped-file))))

(defun efrit-emacs-index--read (file)
  "Parse FILE; nil on any error or when its format is not `efrit-emacs-index-format'."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (forward-comment (buffer-size))
        (let ((data (read (current-buffer))))
          (cond
           ((not (eql (plist-get data :version) efrit-emacs-index-format))
            (setq efrit-emacs-index--problem
                  (format "%s is format %s, this efrit reads %d: rebuild it (make emacs-index, or M-x efrit-emacs-index-rebuild)"
                          (abbreviate-file-name file) (plist-get data :version) efrit-emacs-index-format))
            nil)
           ((or (plist-get data :entries) (plist-get data :manuals)) data))))
    (error (setq efrit-emacs-index--problem (format "%s: unreadable (%s)" (abbreviate-file-name file)
                                                    (error-message-string err)))
           nil)))

(defun efrit-emacs-index-load (&optional force)
  "Read the index into memory once (again when FORCE); return the library entries, or nil."
  (when (or force (null efrit-emacs-index--data))
    (setq efrit-emacs-index--problem nil)
    (if-let* ((file (efrit-emacs-index-current-file))
              (data (efrit-emacs-index--read file)))
        (progn
          (setq efrit-emacs-index--data data
                efrit-emacs-index--by-name (make-hash-table :test 'equal)
                efrit-emacs-index--by-feature (make-hash-table :test 'eq)
                efrit-emacs-index--option-owner (make-hash-table :test 'eq)
                efrit-emacs-index--terms (make-hash-table :test 'equal))
          (dolist (e (plist-get data :entries))
            (puthash (plist-get e :name) e efrit-emacs-index--by-name)
            (dolist (f (plist-get e :provides)) (puthash f e efrit-emacs-index--by-feature))
            (dolist (o (plist-get e :options)) (puthash (car o) e efrit-emacs-index--option-owner)))
          (dolist (m (plist-get data :manuals))
            (let ((nodes (plist-get m :nodes)) (manual (plist-get m :name)))
              (dolist (x (plist-get m :entries))
                (pcase-let ((`(,term ,kind ,idx) x))
                  (push (list term kind manual (aref nodes idx))
                        (gethash (downcase term) efrit-emacs-index--terms))))))
          (efrit-log 'debug "emacs index: %d libraries, %d manuals from %s (Emacs %s%s, built %s)"
                     (length (plist-get data :entries)) (length (plist-get data :manuals))
                     (abbreviate-file-name file) (plist-get data :emacs)
                     (if (plist-get data :revision) (format " %s" (plist-get data :revision)) "")
                     (plist-get data :built)))
      (unless efrit-emacs-index--problem
        (setq efrit-emacs-index--problem
              "no index built: make emacs-index in the efrit checkout, or M-x efrit-emacs-index-rebuild"))
      (setq efrit-emacs-index--data nil)))
  (and efrit-emacs-index--data (plist-get efrit-emacs-index--data :entries)))

(defun efrit-emacs-index-status ()
  "A plist describing the loaded index: :file :emacs :revision :ref :built
:libraries :manuals :matches (non-nil when it was built for this Emacs's
version), or (:problem TEXT) when none is loaded."
  (if (efrit-emacs-index-load)
      (let ((d efrit-emacs-index--data))
        (list :file (efrit-emacs-index-current-file)
              :emacs (plist-get d :emacs) :revision (plist-get d :revision) :ref (plist-get d :ref)
              :built (plist-get d :built)
              :libraries (length (plist-get d :entries)) :manuals (length (plist-get d :manuals))
              :matches (equal (plist-get d :emacs) emacs-version)))
    (list :problem efrit-emacs-index--problem)))

(defun efrit-emacs-index-available-p ()
  "Non-nil when an index is loaded or loadable."
  (and (efrit-emacs-index-load) t))

;;;; Lookup

(defun efrit-emacs-index-entry (library)
  "The entry for LIBRARY: a feature symbol, a library name, or a file name; nil if unknown."
  (when (efrit-emacs-index-load)
    (cond
     ((symbolp library)
      (or (gethash library efrit-emacs-index--by-feature)
          (gethash (symbol-name library) efrit-emacs-index--by-name)))
     ((stringp library)
      (or (gethash (file-name-base library) efrit-emacs-index--by-name)
          (gethash (intern (file-name-base library)) efrit-emacs-index--by-feature))))))

(defun efrit-emacs-index-entry-for-function (symbol)
  "The entry of the library that defines function SYMBOL; nil if unknown.
From `symbol-file' when the library is loaded, from the autoload's
file when it is not, and failing both from the symbol's prefix
\(`mailcap-parse-mailcaps' is mailcap's)."
  (when (and (symbolp symbol) symbol)
    (or (when-let* ((file (and (fboundp symbol) (ignore-errors (symbol-file symbol 'defun)))))
          (efrit-emacs-index-entry (file-name-base file)))
        (when-let* ((def (and (fboundp symbol) (symbol-function symbol)))
                    ((autoloadp def)))
          (efrit-emacs-index-entry (file-name-base (cadr def))))
        (let ((name (symbol-name symbol)))
          (cl-loop for i from (1- (length name)) downto 1
                   when (eq (aref name i) ?-)
                   thereis (efrit-emacs-index-entry (substring name 0 i)))))))

(defun efrit-emacs-index-entry-for-option (option)
  "The entry of the library that defines OPTION, or nil."
  (when (efrit-emacs-index-load)
    (gethash option efrit-emacs-index--option-owner)))

(defun efrit-emacs-index--file-options (entry)
  "ENTRY's options that name files or directories: (NAME DEFAULT) lists."
  (cl-remove-if-not (lambda (o) (eq (nth 2 o) 'file)) (plist-get entry :options)))

(defun efrit-emacs-index--program-options (entry)
  "ENTRY's options that name programs: (NAME DEFAULT) lists."
  (cl-remove-if-not (lambda (o) (eq (nth 2 o) 'program)) (plist-get entry :options)))

(defun efrit-emacs-index--base-directory-p (path)
  "Non-nil when PATH is the home directory, the root, or another base no
library owns (`~/', `/', `~/.emacs.d/' itself, the temporary directory)."
  (let ((dir (file-name-as-directory (expand-file-name path))))
    (member dir (list (file-name-as-directory (expand-file-name "~/")) "/"
                      (file-name-as-directory (expand-file-name user-emacs-directory))
                      (file-name-as-directory (expand-file-name temporary-file-directory))))))

(defun efrit-emacs-index-option-paths (entry)
  "The live paths of ENTRY's file options: ((OPTION . PATH) ...), from the
running Emacs when the option is bound, else from the rendered default.
Base directories (~/, /, the Emacs directory) are left out: an option
whose value is one of those names a parent, not a file the library owns."
  (delq nil
        (mapcar (lambda (o)
                  (pcase-let ((`(,name ,default) o))
                    (let* ((live (and (boundp name) (symbol-value name)))
                           (path (cond
                                  ((and (stringp live) (not (string-empty-p live))) live)
                                  ((and (listp live) live (cl-every #'stringp live)) (car live))
                                  ((and (stringp default) (not (string-prefix-p "$" default))) default))))
                      (and path (not (efrit-emacs-index--base-directory-p path))
                           (cons name (expand-file-name path))))))
                (efrit-emacs-index--file-options entry))))

(defun efrit-emacs-index-expected-files (entry)
  "The files ENTRY touches on its own that a sandbox check may wave through:
the live paths of its file options and the paths it names by literal."
  (let ((concrete (lambda (f) (and (stringp f) (not (string-prefix-p "$" f)) (expand-file-name f)))))
    (cl-remove-duplicates
     (cl-remove-if #'efrit-emacs-index--base-directory-p
                   (append (mapcar #'cdr (efrit-emacs-index-option-paths entry))
                           ;; a $TMPDIR template names no file until the
                           ;; option is read live (option-paths did that)
                           (delq nil (mapcar concrete (plist-get entry :files)))
                           (delq nil (mapcar concrete (plist-get entry :paths)))))
     :test #'equal)))

(defun efrit-emacs-index-programs (entry)
  "The programs ENTRY may start: live values of its program options, then literals."
  (cl-remove-duplicates
   (append (delq nil (mapcar (lambda (o)
                               (let ((live (and (boundp (car o)) (symbol-value (car o)))))
                                 (and (stringp live) (car (split-string live "[ \t]+" t)))))
                             (efrit-emacs-index--program-options entry)))
           (plist-get entry :programs))
   :test #'equal))

;;;; For the prompt and the model

(defun efrit-emacs-index-describe (entry &optional short)
  "ENTRY as text.  SHORT gives one line for a prompt detail."
  (let* ((name (plist-get entry :name))
         (programs (efrit-emacs-index-programs entry))
         (files (mapcar #'abbreviate-file-name (efrit-emacs-index-expected-files entry)))
         (hosts (plist-get entry :hosts))
         (prompts (plist-get entry :prompts))
         (net (plist-get entry :network))
         (auth (plist-get entry :auth)))
    (if short
        (string-join
         (delq nil (list (and files (format "reads/writes %s" (string-join (seq-take files 4) ", ")))
                         (and programs (format "runs %s" (string-join (seq-take programs 4) ", ")))
                         (and hosts (format "contacts %s" (string-join (seq-take hosts 2) ", ")))
                         (and (or net (plist-get entry :processes)) (not programs) (not hosts)
                              (format "uses %s" (string-join (mapcar #'symbol-name (append net (plist-get entry :processes))) ", ")))
                         (and prompts "may prompt")
                         (and auth "reads auth-source")))
         "; ")
      (concat
       (format "%s (%s)%s\n" name (plist-get entry :file) (if (plist-get entry :obsolete) " OBSOLETE" ""))
       (when (plist-get entry :summary) (format "  %s\n" (plist-get entry :summary)))
       (when (efrit-emacs-index--file-options entry)
         (format "  file options: %s\n"
                 (mapconcat (lambda (o) (if (cadr o) (format "%s=%s" (car o) (cadr o)) (symbol-name (car o))))
                            (efrit-emacs-index--file-options entry) ", ")))
       (when (efrit-emacs-index--program-options entry)
         (format "  program options: %s\n"
                 (mapconcat (lambda (o) (if (cadr o) (format "%s=%s" (car o) (cadr o)) (symbol-name (car o))))
                            (efrit-emacs-index--program-options entry) ", ")))
       (when files (format "  files on its own: %s\n" (string-join files ", ")))
       (when programs (format "  programs: %s\n" (string-join programs ", ")))
       (when hosts (format "  hosts: %s\n" (string-join hosts ", ")))
       (when net (format "  network via: %s\n" (mapconcat #'symbol-name net ", ")))
       (when (plist-get entry :processes)
         (format "  processes via: %s\n" (mapconcat #'symbol-name (plist-get entry :processes) ", ")))
       (when prompts (format "  prompts via: %s\n" (mapconcat #'symbol-name prompts ", ")))
       (when auth "  reads credentials through auth-source\n")
       (when (plist-get entry :hooks)
         (format "  hooks: %s\n" (mapconcat #'symbol-name (seq-take (plist-get entry :hooks) 12) ", ")))
       (when (plist-get entry :requires)
         (format "  requires: %s\n" (mapconcat #'symbol-name (seq-take (plist-get entry :requires) 15) ", ")))
       (when (plist-get entry :commands) (format "  commands: %d\n" (plist-get entry :commands)))
       (when (plist-get entry :info) (format "  documented in: %s\n" (string-join (plist-get entry :info) ", ")))))))

(defun efrit-emacs-index-search (query &optional limit)
  "Entries whose name, summary, keywords, programs, files or hosts match QUERY (a regexp)."
  (when (efrit-emacs-index-load)
    (let ((case-fold-search t) (hits nil) (limit (or limit 20)))
      (cl-loop for e in (plist-get efrit-emacs-index--data :entries)
               while (< (length hits) limit)
               when (cl-some (lambda (s) (and (stringp s) (string-match-p query s)))
                             (append (list (plist-get e :name) (plist-get e :summary))
                                     (plist-get e :keywords) (plist-get e :programs)
                                     (plist-get e :files) (plist-get e :paths) (plist-get e :hosts)
                                     (mapcar (lambda (o) (symbol-name (car o))) (plist-get e :options))))
               do (push e hits))
      (nreverse hits))))

(defun efrit-emacs-index-libraries-touching (path)
  "Entries whose files or paths cover PATH (exact or as a directory prefix)."
  (when (efrit-emacs-index-load)
    (let ((p (expand-file-name path)))
      (cl-remove-if-not
       (lambda (e)
         (cl-some (lambda (f) (or (equal f p) (string-prefix-p (file-name-as-directory f) p)))
                  (efrit-emacs-index-expected-files e)))
       (plist-get efrit-emacs-index--data :entries)))))

;;;; The manuals

(defconst efrit-emacs-index-kind-names
  '((c . "concept") (v . "variable") (f . "function") (k . "key") (p . "program") (t . "type"))
  "Index kinds as words.")

(defun efrit-emacs-index-manual-hits (term &optional kind exact)
  "The manual index entries for TERM: ((TERM KIND MANUAL NODE) ...).
Exact entries (case-insensitive) first; then, unless EXACT, entries
containing TERM.  KIND (c v f k p t) narrows them."
  (when (efrit-emacs-index-load)
    (let* ((key (downcase term))
           (exact-hits (gethash key efrit-emacs-index--terms))
           (more (unless exact
                   (let (acc)
                     (maphash (lambda (k v) (when (and (not (equal k key)) (string-search key k))
                                              (setq acc (append v acc))))
                              efrit-emacs-index--terms)
                     (sort acc (lambda (a b) (< (length (car a)) (length (car b))))))))
           (all (append exact-hits more)))
      (if kind (cl-remove-if-not (lambda (h) (eq (nth 1 h) kind)) all) all))))

(defun efrit-emacs-index-manuals ()
  "The indexed manuals: ((NAME TITLE ENTRY-COUNT) ...)."
  (when (efrit-emacs-index-load)
    (mapcar (lambda (m) (list (plist-get m :name) (plist-get m :title) (length (plist-get m :entries))))
            (plist-get efrit-emacs-index--data :manuals))))

(defvar Info-directory-list)
(declare-function info-initialize "info")
(declare-function Info-find-file "info" (filename &optional noerror no-pop-to-dir))
(declare-function info-insert-file-contents "info" (filename &optional visit))

(defun efrit-emacs-index-node-text (manual node &optional max-chars)
  "The text of NODE in the installed Info MANUAL, at most MAX-CHARS; nil when
the manual is not installed or has no such node.  Read from the .info
file (and its split parts) directly: no Info buffer, no history."
  (require 'info)
  (info-initialize)
  (when-let* ((file (ignore-errors (Info-find-file manual t))))
    (let ((files (cons file
                       (let ((dir (file-name-directory file))
                             (base (replace-regexp-in-string "\\.info\\(?:\\.gz\\|\\.bz2\\|\\.xz\\)?\\'" "" (file-name-nondirectory file))))
                         (sort (directory-files dir t (concat "\\`" (regexp-quote base) "\\.info-[0-9]+")) #'string<))))
          (found nil))
      (while (and files (not found))
        (with-temp-buffer
          (ignore-errors (info-insert-file-contents (pop files)))
          (goto-char (point-min))
          (when (re-search-forward (concat "^\^_\nFile: [^,]+,[ \t]+Node: " (regexp-quote node) "[,\n\t]") nil t)
            (forward-line 1)
            (let* ((start (point))
                   (end (if (search-forward "\^_" nil t) (1- (point)) (point-max)))
                   (text (string-trim (buffer-substring-no-properties start end))))
              (setq found (if (and max-chars (> (length text) max-chars))
                              (concat (substring text 0 max-chars) "\n[… more in (" manual ")" node "]")
                            text))))))
      found)))

;;;; Rebuilding

(declare-function efrit-emacs-index-build-write "efrit-emacs-index-build")
(declare-function efrit-emacs-index-build-installed-dirs "efrit-emacs-index-build")

;;;###autoload
(defun efrit-emacs-index-rebuild ()
  "Index the running Emacs's libraries and installed manuals into the data directory, then reload.
A few seconds.  For a source checkout or a release, use `make
emacs-index' in the efrit repository instead."
  (interactive)
  (require 'efrit-emacs-index-build)
  (let* ((dirs (efrit-emacs-index-build-installed-dirs))
         (out (efrit-emacs-index-user-file))
         (counts (efrit-emacs-index-build-write out :lisp-dir (car dirs) :info-dirs (cdr dirs)
                                                :version emacs-version
                                                :revision (bound-and-true-p emacs-repository-version))))
    (efrit-emacs-index-load t)
    (message "efrit: indexed %d libraries and %d manuals of Emacs %s into %s"
             (car counts) (cdr counts) emacs-version (abbreviate-file-name out))
    counts))

;;;###autoload
(defun efrit-emacs-index-show (library)
  "Show what LIBRARY touches on its own."
  (interactive
   (list (progn (efrit-emacs-index-load)
                (completing-read "Library: " (and efrit-emacs-index--by-name (hash-table-keys efrit-emacs-index--by-name))
                                 nil t))))
  (require 'efrit-ui-helpers)
  (if-let* ((entry (efrit-emacs-index-entry library)))
      (efrit-show-popup "*efrit library*" (efrit-emacs-index-describe entry))
    (message "efrit: no index entry for %s" library)))

(declare-function efrit-show-popup "efrit-ui-helpers" (name text &optional mode))

(provide 'efrit-emacs-index)

;;; efrit-emacs-index.el ends here
