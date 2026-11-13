;;; test-emacs-index.el --- the Emacs library index -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-emacs-index)
(require 'efrit-emacs-index-build)
(require 'efrit-sandbox)
(require 'efrit-sandbox-eval)
(require 'efrit-tool-contract)
(require 'efrit-review-flags)

(defvar test-idx--real nil
  "An index of the running Emacs's own lisp directory, built once per test process.")

(defun test-idx--real-index ()
  "The file of `test-idx--real', building it on first use (libraries only, ~2 s)."
  (or (and test-idx--real (file-readable-p test-idx--real) test-idx--real)
      (let ((out (make-temp-file "efrit-idx-real-" nil ".eld")))
        (efrit-emacs-index-build-write out :lisp-dir (car (efrit-emacs-index-build-installed-dirs)))
        (setq test-idx--real out))))

(defmacro test-idx--with-real-index (&rest body)
  "BODY with the index of this Emacs's libraries loaded."
  (declare (indent 0))
  `(let ((efrit-emacs-index-file (test-idx--real-index)) (efrit-emacs-index--data nil))
     (should (efrit-emacs-index-load t))
     ,@body))

(ert-deftest test-emacs-index-builder-reads-a-library-without-loading-it ()
  "The builder sees a library's file and program options with their
rendered defaults, the programs it starts by name, the literal paths and
hosts, the primitive classes it uses, hooks and requires; the obsolete
marker; nothing is loaded or evaluated."
  (let* ((dir (file-name-as-directory (make-temp-file "efrit-idx-" t)))
         (file (expand-file-name "fixlib.el" dir)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; fixlib.el --- a fixture library  -*- lexical-binding: t; -*-\n"
                    ";; Keywords: tools, fixture\n"
                    "(require 'cl-lib)\n"
                    "(defcustom fixlib-save-file (locate-user-emacs-file \"fixlib-state.eld\" \".fixlib\")\n"
                    "  \"Where the state goes.\" :type 'file :group 'fixlib)\n"
                    "(defcustom fixlib-cache-directory (expand-file-name \"fixlib\" temporary-file-directory)\n"
                    "  \"Cache.\" :type 'directory :group 'fixlib)\n"
                    "(defcustom fixlib-program (if (executable-find \"frob2\") \"frob2\" \"frob\")\n"
                    "  \"Program.\" :type 'string :group 'fixlib)\n"
                    "(defcustom fixlib-mode-hook nil \"Hook.\" :type 'hook :group 'fixlib)\n"
                    "(defun fixlib-refresh ()\n"
                    "  \"Refresh.\"\n"
                    "  (interactive)\n"
                    "  (when (file-exists-p \"/etc/fixlib.conf\") (insert-file-contents \"/etc/fixlib.conf\"))\n"
                    "  (call-process \"frobnicate\" nil t nil \"--all\")\n"
                    "  (url-retrieve-synchronously \"https://fixtures.example.org/api\")\n"
                    "  (yes-or-no-p \"Really? \")\n"
                    "  (error \"never evaluated: %s\" (delete-directory \"/\")))\n"
                    "(provide 'fixlib)\n"))
          (let ((e (efrit-emacs-index-build-file file dir)))
            (should (equal "fixlib" (plist-get e :name)))
            (should (equal "a fixture library" (plist-get e :summary)))
            (should (equal '("tools" "fixture") (plist-get e :keywords)))
            (should (equal '(fixlib) (plist-get e :provides)))
            (should (equal '(cl-lib) (plist-get e :requires)))
            (should (equal '((fixlib-save-file "~/.emacs.d/fixlib-state.eld" file)
                             (fixlib-cache-directory "$TMPDIR/fixlib" file)
                             (fixlib-program "frob2" program))
                           (plist-get e :options)))
            (should (equal '("~/.emacs.d/fixlib-state.eld" "$TMPDIR/fixlib") (plist-get e :files)))
            (should (equal '("frob" "frob2" "frobnicate") (sort (copy-sequence (plist-get e :programs)) #'string<)))
            (should (equal '("/etc/fixlib.conf") (plist-get e :paths)))
            (should (equal '("fixtures.example.org") (plist-get e :hosts)))
            (should (equal '(fixlib-mode-hook) (plist-get e :hooks)))
            (should (memq 'insert-file-contents (plist-get e :reads)))
            (should (memq 'delete-directory (plist-get e :writes)))
            (should (memq 'call-process (plist-get e :processes)))
            (should (memq 'url-retrieve-synchronously (plist-get e :network)))
            (should (memq 'yes-or-no-p (plist-get e :prompts)))
            (should (= 1 (plist-get e :commands)))
            (should-not (featurep 'fixlib))
            (should-not (fboundp 'fixlib-refresh)))
          ;; the whole-tree writer and the reader agree
          (let ((out (expand-file-name "idx.eld" dir)))
            (should (equal '(1 . 0) (efrit-emacs-index-build-write out :lisp-dir dir)))
            (let ((efrit-emacs-index-file out) (efrit-emacs-index--data nil))
              (should (efrit-emacs-index-load t))
              (should (efrit-emacs-index-entry 'fixlib))
              (should (efrit-emacs-index-entry "fixlib"))
              (should (equal "fixlib" (plist-get (efrit-emacs-index-entry-for-function 'fixlib-refresh) :name)))
              (should (equal "fixlib" (plist-get (efrit-emacs-index-entry-for-option 'fixlib-save-file) :name)))
              (should (equal '("fixlib") (mapcar (lambda (e) (plist-get e :name)) (efrit-emacs-index-search "frob"))))
              (should (string-match-p "\\`reads/writes ~/.emacs.d/fixlib-state.eld, /etc/fixlib.conf; runs frob2?, frob2?, frobnicate; contacts fixtures.example.org; may prompt\\'"
                                      (efrit-emacs-index-describe (efrit-emacs-index-entry 'fixlib) t)))
              ;; a live option value wins over the rendered default
              (defvar fixlib-save-file)
              (let ((fixlib-save-file (expand-file-name "live-state.eld" dir)))
                (should (member (expand-file-name "live-state.eld" dir)
                                (efrit-emacs-index-expected-files (efrit-emacs-index-entry 'fixlib))))))))
      (delete-directory dir t))))

(ert-deftest test-emacs-index-knows-the-core-libraries ()
  "An index of this Emacs answers for the libraries that bit us: mailcap's
files, savehist's and tramp's state files, doc-view's programs,
package's network use; a function resolves to its library."
  (test-idx--with-real-index
    (should (> (length (efrit-emacs-index-load)) 1000))
    (let ((mailcap (efrit-emacs-index-entry 'mailcap)))
      (should (member "~/.mailcap" (plist-get mailcap :paths)))
      (should (member "/etc/mime.types" (plist-get mailcap :paths))))
    (should (member "mutool" (plist-get (efrit-emacs-index-entry 'doc-view) :programs)))
    (should (equal "savehist" (plist-get (car (efrit-emacs-index-libraries-touching
                                               (expand-file-name "history" user-emacs-directory)))
                                         :name)))
    (should (equal "mailcap" (plist-get (efrit-emacs-index-entry-for-function 'mailcap-parse-mailcaps) :name)))
    (should (equal "tramp-cache" (plist-get (efrit-emacs-index-entry-for-function 'tramp-cache-read-persistent-data) :name)))
    (should (memq 'url-retrieve (plist-get (efrit-emacs-index-entry 'package) :network)))))

(defvar test-idx--state-file nil)
(defun test-idx--library-save (text)
  "A stand-in for a core library saving its own state file."
  (with-temp-file test-idx--state-file (insert text)))
(defun test-idx--library-save-elsewhere (file text)
  "The same library writing somewhere it does not own."
  (with-temp-file file (insert text)))

(ert-deftest test-emacs-index-sandbox-waves-a-library-through-to-its-own-file ()
  "A write from a library to the file its own option names is expected
\(a note, a grant on that file); the same library writing another file
in the same directory asks; the prompt detail names the library and
what it touches."
  (let* ((dir (file-name-as-directory (make-temp-file "efrit-idx-sb-" t)))
         (lib-dir (file-name-as-directory (expand-file-name "lisp" dir)))
         (root (file-name-as-directory (expand-file-name "proj" dir)))
         (efrit-project-root root)
         (efrit-sandbox-enabled t)
         (efrit-sandbox-default-project-grants '(read elisp))
         (efrit-sandbox-request-function nil)
         (efrit-sandbox--session-grants (make-hash-table :test 'equal))
         (efrit-sandbox--project-grants (make-hash-table :test 'equal))
         (efrit-sandbox--turn-state (make-hash-table :test 'equal))
         (efrit-sandbox-store--loaded (make-hash-table :test 'equal))
         (efrit-sandbox-expected-write-roots nil)
         (efrit-sandbox-expected-read-roots nil)
         (efrit-sandbox-expected-library-files t)
         (asked nil) (notes nil)
         (listener (lambda (e) (when (string-match-p "allowed without asking" (alist-get :text e))
                                 (push (alist-get :text e) notes)))))
    (make-directory lib-dir t) (make-directory root t)
    (unwind-protect
        (progn
          ;; an index with one library whose option names the state file
          (with-temp-file (expand-file-name "test-idx.el" lib-dir)
            (insert ";;; test-idx.el --- idx fixture  -*- lexical-binding: t; -*-\n"
                    "(defcustom test-idx--state-file nil \"State.\" :type 'file :group 'x)\n"
                    "(provide 'test-idx)\n"))
          (let* ((out (expand-file-name "idx.eld" dir))
                 (efrit-emacs-index-file out) (efrit-emacs-index--data nil)
                 (test-idx--state-file (expand-file-name "state.eld" dir))
                 (other (expand-file-name "other.eld" dir)))
            (efrit-emacs-index-build-write out :lisp-dir lib-dir)
            (should (efrit-emacs-index-load t))
            (should (equal "test-idx" (plist-get (efrit-emacs-index-entry-for-function 'test-idx--library-save) :name)))
            (efrit-subscribe 'note listener)
            (unwind-protect
                (let ((efrit-sandbox-request-function
                       (lambda (req) (push (efrit-sandbox-request-detail req) asked) nil)))
                  ;; the library's own file: no prompt, a note naming it
                  (should (efrit-sandbox-eval-form '(progn (test-idx--library-save "x") t)))
                  (should (null asked))
                  (should (= 1 (length notes)))
                  (should (string-match-p "test-idx's own file" (car notes)))
                  (should (equal "x" (with-temp-buffer (insert-file-contents test-idx--state-file) (buffer-string))))
                  ;; another file beside it: asks, and the detail says who
                  (should-error (efrit-sandbox-eval-form `(test-idx--library-save-elsewhere ,other "y"))
                                :type 'efrit-sandbox-denied)
                  (should (= 1 (length asked)))
                  (should (string-match-p "\\`write-region, from test-idx--library-save-elsewhere (reads/writes .*state.eld)"
                                          (car asked)))
                  (should-not (file-exists-p other)))
              (efrit-unsubscribe 'note listener))))
      (delete-directory dir t))))

(ert-deftest test-emacs-index-library-profile-and-flags ()
  "library_profile answers by name and by query from the index; a
`require' of a library that starts programs or contacts hosts gets a
FLAG library line for the reviewer."
  (test-idx--with-real-index
    (let ((r (efrit-tool-library-profile '((library . "doc-view")))))
      (should (eq t (alist-get 'success r)))
      (should (member "mutool" (append (alist-get 'programs (alist-get 'result r)) nil)))
      (should (cl-some (lambda (o) (string-prefix-p "doc-view-pdfdraw-program=" o))
                       (append (alist-get 'program_options (alist-get 'result r)) nil))))
    (let ((r (efrit-tool-library-profile '((query . "mutool")))))
      (should (member "doc-view" (mapcar (lambda (l) (alist-get 'name l))
                                         (append (alist-get 'libraries (alist-get 'result r)) nil)))))
    (should (string-match-p "not in the index" (format "%s" (efrit-tool-library-profile '((library . "no-such-lib-xyz"))))))
    (should (string-match-p "library or query" (format "%s" (efrit-tool-library-profile nil))))
    ;; the reviewer's flag
    (let ((eff (efrit-review-flags--text-effects "(require 'doc-view)" nil)))
      (should (assq 'load eff))
      (should (string-match-p "doc-view runs gs" (cdr (assq 'library eff)))))
    (should-not (assq 'library (efrit-review-flags--text-effects "(require 'savehist)" nil)))))

(defun test-idx--write (file &rest lines)
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert (mapconcat #'identity lines "\n") "\n")))

(ert-deftest test-emacs-index-manuals-from-texinfo-and-info ()
  "The builder reads Texinfo manuals (explicit index entries, the implicit
ones of @defopt/@defun/@deffn, @include of chapters, @value in titles)
and installed .info index nodes; a manual found both ways is taken from
the Texinfo; manual entries link to the library defining the term;
info_lookup answers by term, kind and exactness, and reads a node's text
from the installed .info."
  (let* ((dir (file-name-as-directory (make-temp-file "efrit-idx-man-" t)))
         (lisp (expand-file-name "lisp/" dir))
         (doc (expand-file-name "doc/" dir))
         (info (expand-file-name "info/" dir))
         (out (expand-file-name "idx.eld" dir)))
    (unwind-protect
        (progn
          (test-idx--write (expand-file-name "fixview.el" lisp)
                           ";;; fixview.el --- fixture viewer  -*- lexical-binding: t; -*-"
                           "(defcustom fixview-resolution 100 \"Res.\" :type 'integer)"
                           "(defun fixview-open (file) \"Open.\" file)"
                           "(provide 'fixview)")
          (test-idx--write (expand-file-name "misc/fixman.texi" doc)
                           "@setfilename ../../info/fixman.info"
                           "@set FIXVER 2.1"
                           "@settitle Fixture Manual @value{FIXVER}"
                           "@node Top" "@top Fixture"
                           "@include fixchap.texi")
          (test-idx--write (expand-file-name "misc/fixchap.texi" doc)
                           "@node Viewing" "@chapter Viewing"
                           "@cindex PDF file"
                           "@vindex fixview-resolution"
                           "@defopt fixview-resolution"
                           "Dots per inch." "@end defopt"
                           "@node Opening" "@section Opening"
                           "@deffn Command fixview-open file"
                           "Open it." "@end deffn"
                           "@kindex C-c v")
          ;; an installed manual with an index node, and one that
          ;; duplicates the Texinfo manual (ignored)
          (test-idx--write (expand-file-name "sysman.info" info)
                           "This is sysman.info."
                           "INFO-DIR-SECTION Utilities" "START-INFO-DIR-ENTRY"
                           "* Sysman: (sysman).     A system manual."
                           "END-INFO-DIR-ENTRY"
                           "\x1f" "File: sysman.info,  Node: Top,  Next: Pipes,  Up: (dir)" "" "Top." ""
                           "\x1f" "File: sysman.info,  Node: Pipes,  Prev: Top,  Up: Top" "" "Pipes carry bytes from one program to the next." ""
                           "\x1f" "File: sysman.info,  Node: Index,  Prev: Pipes,  Up: Top" "" "Index" "*****" ""
                           "\0\b[index\0\b]" "* Menu:" ""
                           "* pipe:                                   Pipes.                (line 6)"
                           "* fixview-open:                           Pipes.                (line 7)"
                           "")
          (test-idx--write (expand-file-name "fixman.info" info)
                           "\x1f" "File: fixman.info,  Node: Index" "" "\0\b[index\0\b]" "* Menu:" ""
                           "* stale entry:                            Top.                  (line 1)" "")
          (should (equal '(1 . 2) (efrit-emacs-index-build-write out :lisp-dir lisp :doc-dir doc :info-dirs (list info)
                                                                 :version "30.1" :revision "abc1234")))
          (let ((efrit-emacs-index-file out) (efrit-emacs-index--data nil))
            (should (efrit-emacs-index-load t))
            ;; fixman: PDF file, fixview-resolution (vindex and defopt: one),
            ;; fixview-open, C-c v
            (should (equal '(("fixman" "Fixture Manual 2.1" 4) ("sysman" "A system manual." 2))
                           (efrit-emacs-index-manuals)))
            (should (equal '(("PDF file" c "fixman" "Viewing")) (efrit-emacs-index-manual-hits "pdf file" nil t)))
            ;; @vindex and @defopt name the same node once
            (should (equal '(("fixview-resolution" v "fixman" "Viewing"))
                           (efrit-emacs-index-manual-hits "fixview-resolution" 'v t)))
            (should (equal '(("fixview-open" f "fixman" "Opening"))
                           (efrit-emacs-index-manual-hits "fixview-open" 'f t)))
            (should (equal '(("C-c v" k "fixman" "Opening")) (efrit-emacs-index-manual-hits "C-c v")))
            (should-not (efrit-emacs-index-manual-hits "stale entry"))
            (should (equal "pipe" (car (car (efrit-emacs-index-manual-hits "pip")))))
            ;; the library names the nodes documenting what it defines,
            ;; from both manuals
            (should (equal '("(fixman)Viewing" "(fixman)Opening" "(sysman)Pipes")
                           (plist-get (efrit-emacs-index-entry 'fixview) :info)))
            (should (equal '("(fixman)Viewing" "(fixman)Opening" "(sysman)Pipes")
                           (append (alist-get 'documented_in (alist-get 'result (efrit-tool-library-profile '((library . "fixview")))))
                                   nil)))
            ;; the tool
            (let ((r (alist-get 'result (efrit-tool-info-lookup '((term . "fixview-resolution") (kind . "variable"))))))
              (should (equal "(fixman)Viewing" (alist-get 'node (aref (alist-get 'hits r) 0))))
              (should (equal "variable" (alist-get 'kind (aref (alist-get 'hits r) 0)))))
            (let ((Info-directory-list (list info)) (Info-additional-directory-list nil))
              (let ((r (alist-get 'result (efrit-tool-info-lookup '((manual . "sysman") (node . "Pipes"))))))
                (should (string-match-p "\\`Pipes carry bytes" (alist-get 'text r))))
              (should (string-match-p "not in the installed Info"
                                      (format "%s" (efrit-tool-info-lookup '((manual . "sysman") (node . "Nope")))))))
            (should (string-match-p "term, or manual and node" (format "%s" (efrit-tool-info-lookup nil))))
            ;; what the doctor reads
            (let ((st (efrit-emacs-index-status)))
              (should (equal "30.1" (plist-get st :emacs)))
              (should (equal "abc1234" (plist-get st :revision)))
              (should (eq (equal "30.1" emacs-version) (plist-get st :matches))))))
      (delete-directory dir t))))

(ert-deftest test-emacs-index-format-and-pinning ()
  "An index of another format is not read and the problem says how to
rebuild; the configure.ac version pins an index to its sources; batch
arguments parse; an installed .el.gz library is named without .gz."
  (let ((dir (file-name-as-directory (make-temp-file "efrit-idx-fmt-" t))))
    (unwind-protect
        (let ((old (expand-file-name "old.eld" dir)))
          (with-temp-file old (prin1 '(:version 1 :emacs "29.1" :entries ((:name "x"))) (current-buffer)))
          (let ((efrit-emacs-index-file old) (efrit-emacs-index--data nil))
            (should-not (efrit-emacs-index-load t))
            (should (string-match-p "format 1, this efrit reads 3: rebuild it" (plist-get (efrit-emacs-index-status) :problem))))
          (should (= efrit-emacs-index-format efrit-emacs-index-build-version))
          (test-idx--write (expand-file-name "configure.ac" dir) "dnl x" "AC_INIT([GNU Emacs], [30.2], [bug-gnu-emacs@gnu.org], [],")
          (should (equal "30.2" (efrit-emacs-index-build--configure-version (expand-file-name "configure.ac" dir))))
          (should (equal (list :lisp-dir "/l" :doc-dir "/d" :version "30.2" :revision "abc" :ref nil :out "/o" :info-dirs '("/i2" "/i1"))
                         (efrit-emacs-index-build--args (list "lisp=/l" "doc=/d" (concat "configure=" dir "configure.ac")
                                                              "revision=abc" "ref=" "out=/o" "info=/i1" "info=/i2"))))
          (should-error (efrit-emacs-index-build--args '("bogus")))
          (should-error (efrit-emacs-index-build--args '("color=red")))
          ;; .el.gz as an installed Emacs ships it
          (let ((gz (expand-file-name "gzlib.el" dir)))
            (test-idx--write gz ";;; gzlib.el --- compressed  -*- lexical-binding: t; -*-" "(provide 'gzlib)")
            (skip-unless (executable-find "gzip"))
            (call-process "gzip" nil nil nil gz)
            (let ((e (efrit-emacs-index-build-file (concat gz ".gz") dir)))
              (should (equal "gzlib" (plist-get e :name)))
              (should (equal "gzlib.el" (plist-get e :file)))
              (should (equal "compressed" (plist-get e :summary))))))
      (delete-directory dir t))))

(provide 'test-emacs-index)
;;; test-emacs-index.el ends here
