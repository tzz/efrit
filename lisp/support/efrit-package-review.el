;;; efrit-package-review.el --- Model-assisted review of incoming packages -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Emacs 31 can stop before installing a package and let you read the
;; source, diff it against the previous installation, and read the
;; change log (`package-review-policy', `package-review').  Reading a
;; whole package before every upgrade is more than most people do,
;; which is what makes the review a formality.  This module has a
;; model read it and report, so the human decision is made with the
;; risky forms already pointed at.
;;
;; `efrit-package-review-mode' wraps `package-review'.  Before
;; package.el asks "Install?", the reviewer sees the same material the
;; user could open (the sources, the diff when there was a previous
;; install, the changelog) and returns a verdict with findings: each
;; a severity, a file and line, and one sentence.  The report opens in
;; a popup beside the prompt (q closes it), and the prompt itself is
;; prefixed with the verdict.  What happens next is
;; `efrit-package-review-action':
;;
;;   annotate            show the report; you answer the prompt (default)
;;   auto-approve-clean  a clean verdict answers yes for you; anything
;;                       flagged, or a failed review, falls back to
;;                       the prompt with the report shown
;;
;; `M-x efrit-review-package' runs the same review on demand for an
;; installed or an archive package, on any Emacs version.
;;
;; The rubric asks for the things a reader looks for and a diff hides
;; well: network access, evaluation or loading of fetched or generated
;; code, writes outside the package's own directory or into init
;; files, advice on core primitives, processes spawned, obfuscated or
;; encoded forms, and a change of maintainer or archive.  The reviewer
;; is told what it did not see: files are cut at
;; `efrit-package-review-max-file-chars', and a verdict on a cut
;; package says so.
;;
;; The model is `efrit-package-review-model' when set, else the
;; per-turn reviewer's model, else the default: a rarer and higher
;; stakes review is where a stronger model earns its cost.
;;
;; Nothing here runs package code.  The reads are efrit's own, made
;; on the user's request, so they are not sandbox prompts (the sandbox
;; gates what the model does); the API call carries a purpose so a
;; failure names it.  None of this is reachable from eval_sexp: the
;; efrit-package-review prefix is on the refused list.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'package)
(require 'efrit-log)
(require 'efrit-api)
(require 'efrit-chat-response)
(require 'efrit-review)          ; efrit-review-model, efrit-review-parse-verdict
(require 'efrit-ui-helpers)

(defvar efrit-default-model)
(declare-function package-find-news-file "package")
(declare-function package-desc-dir "package")
(declare-function package--get-activatable-pkg "package")

(defgroup efrit-package-review nil
  "Model-assisted review of packages before they are installed."
  :group 'efrit-review
  :prefix "efrit-package-review-")

(defcustom efrit-package-review-model nil
  "Model that reviews packages, or nil for `efrit-review-model' / the default.
A package review is rare and the cost of a miss is high; name a
stronger model here than the one that reviews every turn."
  :type '(choice (const :tag "Same as the turn reviewer" nil) string)
  :group 'efrit-package-review)

(defcustom efrit-package-review-action 'annotate
  "What the review does with its verdict at install time.
`annotate' shows the report and leaves the answer to you.
`auto-approve-clean' answers yes when the verdict is clean; a
flagged package, or a review that failed, still asks you."
  :type '(choice (const annotate) (const auto-approve-clean))
  :group 'efrit-package-review)

(defcustom efrit-package-review-max-file-chars 60000
  "Most characters of one source file shown to the reviewer.
Longer files are cut and the cut is reported to the reviewer and in
the verdict."
  :type 'integer
  :group 'efrit-package-review)

(defcustom efrit-package-review-max-total-chars 300000
  "Most characters of source, diff and changelog sent in one review."
  :type 'integer
  :group 'efrit-package-review)

(defcustom efrit-package-review-timeout 120
  "Seconds to wait for the reviewer at install time.
`package-review' is synchronous, so the review must be too."
  :type 'integer
  :group 'efrit-package-review)

(defconst efrit-package-review-severities '(high medium low info)
  "Severities a finding may carry, most serious first.")

(defconst efrit-package-review--buffer "*efrit-package-review*")

;;; Gathering what the reviewer sees

(defun efrit-package-review--source-files (dir)
  "The reviewable files under DIR: Lisp, shell, Makefiles, docs; never .elc."
  (cl-remove-if
   (lambda (f)
     (or (string-match-p "\\.\\(elc\\|eln\\|png\\|jpg\\|gif\\|svg\\|info\\|gz\\)\\'" f)
         (string-match-p "/\\.git/" f)))
   (directory-files-recursively dir "" nil)))

(defun efrit-package-review--read-file (file limit)
  "FILE's text, cut at LIMIT characters with a note.
Not a sandbox-checked read: the model did not ask for this file, the
user did by installing the package, and package.el is about to show
the same files on request.  The sandbox gates the model's actions."
  (with-temp-buffer
    (insert-file-contents file nil 0 (* 2 limit))
    (let ((text (buffer-string)))
      (if (> (length text) limit)
          (concat (substring text 0 limit)
                  (format "\n[... cut: %d more characters not shown ...]\n"
                          (- (nth 7 (file-attributes file)) limit)))
        text))))

(defun efrit-package-review--news-file (pkg-desc pkg-dir)
  "The change log file for PKG-DESC unpacked in PKG-DIR, or nil.
package.el looks in the desc's own dir, which at review time may not
be PKG-DIR yet; look in PKG-DIR first, with the usual names."
  (or (cl-some (lambda (name)
                 (let ((f (expand-file-name name pkg-dir)))
                   (and (file-regular-p f) (file-readable-p f) f)))
               '("NEWS-elpa" "NEWS" "news" "CHANGELOG" "CHANGELOG.md" "ChangeLog" "NEWS.md" "NEWS.org"))
      (and (fboundp 'package-find-news-file)
           (ignore-errors (package-find-news-file pkg-desc)))))

(defun efrit-package-review--diff (old-dir new-dir)
  "Unified diff from OLD-DIR to NEW-DIR as a string, or nil when git is missing."
  (when (executable-find "git")
    (with-temp-buffer
      (call-process "git" nil t nil "diff" "--no-index" "--no-color"
                    "--diff-filter=d" "--minimal" old-dir new-dir)
      (buffer-string))))

(defun efrit-package-review-gather (pkg-desc pkg-dir old-desc)
  "Everything the reviewer sees for PKG-DESC unpacked in PKG-DIR.
OLD-DESC is the installed version or nil.  Returns a plist
\(:name :version :archive :maintainers :old-version :sources :diff
:news :cut), where :sources is an alist (RELATIVE-NAME . TEXT) and
:cut is non-nil when something was left out."
  (let* ((budget efrit-package-review-max-total-chars)
         (cut nil)
         (take (lambda (text)
                 (cond
                  ((null text) nil)
                  ((<= (length text) budget)
                   (cl-decf budget (length text)) text)
                  (t (setq cut t)
                     (prog1 (concat (substring text 0 (max 0 budget))
                                    "\n[... cut: total review budget reached ...]\n")
                       (setq budget 0))))))
         (news (efrit-package-review--news-file pkg-desc pkg-dir))
         (old-dir (and old-desc (package-desc-dir old-desc)))
         (diff (and old-dir (file-directory-p old-dir)
                    (funcall take (efrit-package-review--diff old-dir pkg-dir))))
         (sources
          (delq nil
                (mapcar (lambda (f)
                          (let ((text (funcall take (efrit-package-review--read-file
                                                     f efrit-package-review-max-file-chars))))
                            (when (string-match-p "\\[\\.\\.\\. cut:" (or text ""))
                              (setq cut t))
                            (and text (cons (file-relative-name f pkg-dir) text))))
                        (efrit-package-review--source-files pkg-dir)))))
    (list :name (symbol-name (package-desc-name pkg-desc))
          :version (package-version-join (package-desc-version pkg-desc))
          :archive (package-desc-archive pkg-desc)
          :maintainers (ignore-errors (package-maintainers pkg-desc))
          :old-version (and old-desc (package-version-join (package-desc-version old-desc)))
          :sources sources
          :diff diff
          :news (and news (file-readable-p news)
                     (funcall take (efrit-package-review--read-file
                                    news efrit-package-review-max-file-chars)))
          :cut cut)))

;;; The request

(defconst efrit-package-review--system-prompt
  "You review an Emacs Lisp package before it is installed.  You are given its source files, a unified diff from the previously installed version when there was one, and its change log.  Report what a careful maintainer would want pointed at before saying yes.

Look for, in this order of concern:
1. Code that fetches from the network and evaluates, loads, or writes what it fetched.
2. eval, load, load-file, require of computed names, or byte-code constants applied to data from outside the package.
3. Writes outside the package's own directory: init files, ~/.emacs.d, dotfiles, ssh/gpg/auth material, the shell profile, system paths.
4. Processes spawned (call-process, start-process, shell-command, make-process) and what they run.
5. Advice on, or redefinition of, core functions (defalias/fset of built-ins, advice on read/eval/load/process primitives, file-name handlers).
6. Obfuscation: base64 or hex blobs, string-built symbol names, unusual encodings, code hidden after ^L or in long lines.
7. Credentials read or sent: auth-source, environment variables with KEY/TOKEN/SECRET, hard-coded hosts.
8. In the diff: a change of maintainer, archive, or repository URL; new dependencies; new autoloads that run on load.

Ordinary package behaviour is not a finding: reading its own files, customizable options, buffers, timers, hooks, network access that is the package's declared purpose (say so, once, as info).

Answer with exactly one JSON object and nothing else:
{\"verdict\": \"approve\" | \"reject\",
 \"summary\": one or two sentences for the user,
 \"findings\": [{\"severity\": \"high\"|\"medium\"|\"low\"|\"info\", \"file\": \"relative/path.el\", \"line\": N or null, \"note\": one sentence}],
 \"saw_everything\": true | false}
Reject when any finding is high, or when a medium finding is not explained by the package's purpose.  If input was cut, set saw_everything false and say what you could not judge."
  "System prompt for the package reviewer.")

(defun efrit-package-review--user-message (info)
  "The user message for INFO (see `efrit-package-review-gather')."
  (concat
   (format "Package: %s %s%s\nArchive: %s\nMaintainers: %s\n%s\n"
           (plist-get info :name) (plist-get info :version)
           (if (plist-get info :old-version)
               (format " (upgrading from %s)" (plist-get info :old-version))
             " (new install)")
           (or (plist-get info :archive) "unknown")
           (or (mapconcat (lambda (m) (format "%s" (if (consp m) (car m) m)))
                          (plist-get info :maintainers) ", ")
               "unknown")
           (if (plist-get info :cut)
               "NOTE: some content was cut for size; say so in the verdict."
             ""))
   (when-let* ((news (plist-get info :news)))
     (concat "\n=== CHANGELOG ===\n" news "\n"))
   (when-let* ((diff (plist-get info :diff)))
     (concat "\n=== DIFF FROM INSTALLED VERSION ===\n" diff "\n"))
   "\n=== SOURCE FILES ===\n"
   (mapconcat (lambda (src) (format "\n--- %s ---\n%s" (car src) (cdr src)))
              (plist-get info :sources) "\n")))

(defun efrit-package-review-model ()
  "The model that reviews packages."
  (or efrit-package-review-model efrit-review-model efrit-default-model))

(defun efrit-package-review--request (info)
  `(("model" . ,(efrit-package-review-model))
    ("max_tokens" . 2000)
    ("system" . ,(efrit-api-cacheable-system efrit-package-review--system-prompt))
    ("messages" . [(("role" . "user")
                    ("content" . ,(efrit-package-review--user-message info)))])))

(defun efrit-package-review-parse (text)
  "Parse the reviewer's TEXT into a plist, or nil if malformed.
\(:verdict SYM :summary STR :findings ((:severity SYM :file STR :line N :note STR)...)
:saw-everything BOOL)."
  (when (and (stringp text) (string-match "{\\(?:.\\|\n\\)*}" text))
    (condition-case nil
        (let* ((obj (json-parse-string (match-string 0 text)
                                       :object-type 'alist :array-type 'list
                                       :null-object nil :false-object nil))
               (verdict (intern (downcase (format "%s" (alist-get 'verdict obj))))))
          (when (memq verdict efrit-review-verdicts)
            (list :verdict verdict
                  :summary (or (alist-get 'summary obj) "")
                  :saw-everything (eq (alist-get 'saw_everything obj) t)
                  :findings
                  (delq nil
                        (mapcar (lambda (f)
                                  (let ((sev (intern (downcase (format "%s" (alist-get 'severity f))))))
                                    (when (memq sev efrit-package-review-severities)
                                      (list :severity sev
                                            :file (alist-get 'file f)
                                            :line (alist-get 'line f)
                                            :note (or (alist-get 'note f) "")))))
                                (alist-get 'findings obj))))))
      (error nil))))

(defun efrit-package-review-run (info)
  "Review INFO synchronously.  Returns the parsed verdict plist, or
\(:verdict error :summary WHY) when the call or the parse failed."
  (condition-case err
      (let* ((efrit-api-request-purpose
              (format "reviewing package %s %s before install"
                      (plist-get info :name) (plist-get info :version)))
             (response (efrit-api-request-sync (efrit-package-review--request info)
                                               efrit-package-review-timeout)))
        (cond
         ((and response (efrit-response-error response))
          (list :verdict 'error :summary (efrit-error-message (efrit-response-error response))))
         (t (or (efrit-package-review-parse (efrit-package-review--response-text response))
                (list :verdict 'error :summary "the reviewer's answer was not a verdict")))))
    (error (list :verdict 'error :summary (error-message-string err)))))

(defun efrit-package-review--response-text (response)
  (let ((content (efrit-response-content response)) (texts nil))
    (when content
      (dotimes (i (length content))
        (let ((item (aref content i)))
          (when (and (hash-table-p item) (equal (gethash "type" item) "text"))
            (push (gethash "text" item) texts)))))
    (string-join (nreverse texts) "")))

;;; The report

(defun efrit-package-review-clean-p (verdict)
  "Non-nil if VERDICT approves with nothing above `info' and saw everything."
  (and (eq (plist-get verdict :verdict) 'approve)
       (plist-get verdict :saw-everything)
       (cl-every (lambda (f) (eq (plist-get f :severity) 'info))
                 (plist-get verdict :findings))))

(defun efrit-package-review-verdict-line (verdict)
  "One line: the verdict and the count of findings by severity."
  (let ((counts (mapcar (lambda (sev)
                          (cons sev (cl-count sev (plist-get verdict :findings)
                                              :key (lambda (f) (plist-get f :severity)))))
                        efrit-package-review-severities)))
    (pcase (plist-get verdict :verdict)
      ('error (format "efrit review failed: %s" (plist-get verdict :summary)))
      (v (format "efrit: %s%s%s"
                 (if (eq v 'approve) "approve" "REJECT")
                 (let ((parts (cl-remove-if (lambda (c) (zerop (cdr c))) counts)))
                   (if parts
                       (concat " · " (mapconcat (lambda (c) (format "%d %s" (cdr c) (car c))) parts ", "))
                     " · no findings"))
                 (if (plist-get verdict :saw-everything) "" " · did not see everything"))))))

(defun efrit-package-review-report (info verdict)
  "The full report text for INFO and VERDICT."
  (concat
   (format "%s %s%s\n%s\n\n"
           (plist-get info :name) (plist-get info :version)
           (if (plist-get info :old-version) (format "  (from %s)" (plist-get info :old-version)) "")
           (efrit-package-review-verdict-line verdict))
   (plist-get verdict :summary) "\n"
   (when-let* ((fs (plist-get verdict :findings)))
     (concat "\n"
             (mapconcat (lambda (f)
                          (format "  %-6s %s%s\n         %s"
                                  (upcase (symbol-name (plist-get f :severity)))
                                  (or (plist-get f :file) "")
                                  (if (plist-get f :line) (format ":%s" (plist-get f :line)) "")
                                  (plist-get f :note)))
                        (sort (copy-sequence fs)
                              (lambda (a b) (< (cl-position (plist-get a :severity) efrit-package-review-severities)
                                               (cl-position (plist-get b :severity) efrit-package-review-severities))))
                        "\n")
             "\n"))
   (format "\nReviewer: %s.  Sources: %d file(s)%s%s."
           (efrit-package-review-model)
           (length (plist-get info :sources))
           (if (plist-get info :diff) ", diff against installed" "")
           (if (plist-get info :news) ", changelog" ""))))

(defun efrit-package-review-show (info verdict)
  "Show the report in a popup and return it."
  (let ((text (efrit-package-review-report info verdict)))
    (efrit-show-preview efrit-package-review--buffer text)
    text))

;;; Install-time hook (Emacs 31's package-review)

(defun efrit-package-review--around (orig pkg-desc pkg-dir old-desc)
  "Review PKG-DESC in PKG-DIR against OLD-DESC before ORIG asks the user."
  (let* ((info (efrit-package-review-gather pkg-desc pkg-dir old-desc))
         (verdict (efrit-package-review-run info)))
    (efrit-log 'info "package review %s %s: %s" (plist-get info :name)
               (plist-get info :version) (efrit-package-review-verdict-line verdict))
    (if (and (eq efrit-package-review-action 'auto-approve-clean)
             (efrit-package-review-clean-p verdict))
        (progn
          (message "%s: %s (installed without asking; efrit-package-review-action)"
                   (plist-get info :name) (efrit-package-review-verdict-line verdict))
          nil)
      (efrit-package-review-show info verdict)
      ;; package.el's prompt is read-multiple-choice; prefix its question
      ;; with the verdict so the answer is made with it in view
      (cl-letf* ((rmc (symbol-function 'read-multiple-choice))
                 ((symbol-function 'read-multiple-choice)
                  (lambda (prompt choices &rest rest)
                    (apply rmc (concat (efrit-package-review-verdict-line verdict) "\n" prompt)
                           choices rest))))
        (funcall orig pkg-desc pkg-dir old-desc)))))

;;;###autoload
(define-minor-mode efrit-package-review-mode
  "Have efrit review packages before Emacs 31's `package-review' asks.
Needs `package-review-policy' set to something other than nil; the
mode turns it on (t) when it is nil and says so."
  :global t
  :group 'efrit-package-review
  (cond
   ((not (fboundp 'package-review))
    (setq efrit-package-review-mode nil)
    (user-error "package-review needs Emacs 31; use M-x efrit-review-package instead"))
   (efrit-package-review-mode
    (advice-add 'package-review :around #'efrit-package-review--around)
    (when (and (boundp 'package-review-policy) (null package-review-policy))
      (setq package-review-policy t)
      (message "efrit: package-review-policy set to t so reviews happen")))
   (t (advice-remove 'package-review #'efrit-package-review--around))))

;;; On demand

;;;###autoload
(defun efrit-review-package (name)
  "Review the installed package NAME with efrit and show the report.
With an archive copy available that is newer, the installed one is
still what is reviewed: this command judges what is on disk."
  (interactive
   (list (intern (completing-read "Review package: "
                                  (mapcar (lambda (p) (symbol-name (car p))) package-alist)
                                  nil t))))
  (let* ((desc (or (cadr (assq name package-alist))
                   (user-error "%s is not installed" name)))
         (dir (package-desc-dir desc))
         (info (efrit-package-review-gather desc dir nil))
         (verdict (efrit-package-review-run info)))
    (efrit-package-review-show info verdict)
    (message "%s" (efrit-package-review-verdict-line verdict))
    verdict))

(provide 'efrit-package-review)

;;; efrit-package-review.el ends here
