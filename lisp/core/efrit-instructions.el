;;; efrit-instructions.el --- Load AGENTS.md / CLAUDE.md the way Claude Code does -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.12.12
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Project instruction files for agents (AGENTS.md, CLAUDE.md) are
;; layered, not single: a user keeps standing rules in their home
;; directory, an organisation keeps rules in a parent directory above
;; several checkouts, each checkout has its own file, and a developer
;; may keep a private, unversioned local file.  Claude Code reads all
;; of them; an agent that reads only the nearest one silently ignores
;; the rules the user most relied on.
;;
;; This file collects, in order of increasing specificity:
;;
;;   1. user files       ~/.claude/CLAUDE.md, ~/.efrit/AGENTS.md ...
;;                       (`efrit-instructions-user-files'; always local)
;;   2. ancestor files   for each directory from the filesystem root
;;                       down to the parent of the project root, the
;;                       first match of `efrit-instructions-files'
;;   3. project files    the first match in the project root
;;   4. local overrides  CLAUDE.local.md and friends in the project
;;                       root (`efrit-instructions-local-files')
;;
;; Within a directory, only the first matching name is used, so a
;; project with both AGENTS.md and CLAUDE.md contributes one file (the
;; order of `efrit-instructions-files' decides which).  Across
;; directories, everything is included, each under a heading naming
;; its file, most specific last.  That is the order a reader resolves
;; conflicts in, and the model is told so.
;;
;; `@path' on a line of its own imports another file relative to the
;; importing file (Claude Code's import syntax).  Imports nest to
;; `efrit-instructions-max-import-depth'; cycles and missing files are
;; noted inline, not fatal.
;;
;; Each file is capped at `efrit-instructions-max-file-size' and the
;; whole block at `efrit-instructions-max-total-size'; the cap is
;; applied last-file-first so the most specific instructions survive.
;;
;; Remote projects: the ancestor walk and project files are read on
;; the project's host; user files are read on the local machine.
;; Each remote directory is listed once, and the located files and the
;; assembled text are kept for `efrit-instructions-remote-cache-seconds'.
;; The callers (system prompt, reviewer, sandbox) run inside process
;; sentinels, where a TRAMP round trip per ancestor per call froze
;; Emacs (2026-11-10).  `efrit-instructions-forget' drops the cache.
;;
;; These reads are efrit acting for the user, not a tool acting for
;; the model, so they do not pass through the sandbox -- the same
;; standing as reading .efrit/sandbox.json.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-log)
(require 'efrit-tool-utils)   ; efrit-tool--get-project-root
(declare-function efrit-show-popup "efrit-ui-helpers")

(defgroup efrit-instructions nil
  "Loading of project instruction files (AGENTS.md, CLAUDE.md)."
  :group 'efrit
  :prefix "efrit-instructions-")

(defcustom efrit-instructions-files
  '(".efrit/AGENTS.md" "AGENTS.md" "AGENT.md" "CLAUDE.md" ".claude/CLAUDE.md")
  "File names tried, in order, in the project root and each ancestor directory.
The first that exists in a directory is that directory's contribution."
  :type '(repeat string)
  :group 'efrit-instructions)

(defcustom efrit-instructions-local-files
  '("CLAUDE.local.md" "AGENTS.local.md")
  "Unversioned per-developer overrides read from the project root, after the project file.
Claude Code treats CLAUDE.local.md this way."
  :type '(repeat string)
  :group 'efrit-instructions)

(defcustom efrit-instructions-user-files
  '("~/.claude/CLAUDE.md" "~/.efrit/AGENTS.md" "~/.config/efrit/AGENTS.md")
  "User-level instruction files, read first, from the local machine.
All that exist are included."
  :type '(repeat string)
  :group 'efrit-instructions)

(defcustom efrit-instructions-ancestors t
  "When non-nil, also read instruction files from directories above the project root.
Claude Code does; this is how an organisation-wide file above several
checkouts reaches every one of them."
  :type 'boolean
  :group 'efrit-instructions)

(defcustom efrit-instructions-max-file-size 50000
  "Bytes read from any single instruction file; the rest is dropped with a note."
  :type 'integer
  :group 'efrit-instructions)

(defcustom efrit-instructions-max-total-size 120000
  "Cap on the assembled block.  Earlier (less specific) files are trimmed first."
  :type 'integer
  :group 'efrit-instructions)

(defcustom efrit-instructions-max-import-depth 3
  "How many levels of @path imports are followed."
  :type 'integer
  :group 'efrit-instructions)

(defcustom efrit-instructions-remote-cache-seconds 300
  "Seconds the located files and text of a remote project root are reused.
Local roots are not cached: a stat per candidate file is cheap and an
edit shows at once.  On a remote root every candidate is a TRAMP round
trip, and the callers run inside process sentinels."
  :type 'number
  :group 'efrit-instructions)

(defvar efrit-instructions--remote-cache nil
  "Alist ROOT -> (TIME LOCATED TEXT-CELL) for remote roots.
TEXT-CELL is nil until `efrit-instructions-text' ran, then (TEXT).")

(defun efrit-instructions-forget ()
  "Drop the remote cache: the next call reads the files again."
  (interactive)
  (setq efrit-instructions--remote-cache nil))

(defun efrit-instructions--remote-entry (root)
  "The live cache entry for remote ROOT, or nil when absent or stale."
  (let ((entry (assoc root efrit-instructions--remote-cache)))
    (and entry
         (< (float-time (time-since (nth 1 entry))) efrit-instructions-remote-cache-seconds)
         entry)))

(defun efrit-instructions--remote-remember (root located)
  "Record LOCATED for remote ROOT; returns the new entry."
  (setq efrit-instructions--remote-cache
        (cons (list root (current-time) located nil)
              (assoc-delete-all root efrit-instructions--remote-cache)))
  (car efrit-instructions--remote-cache))

(defconst efrit-instructions--import-regexp
  "^@\\([^[:space:]@][^[:space:]]*\\)[[:space:]]*$"
  "A line that is only @PATH imports PATH.  Email-like @word inside prose does not match.")

(defconst efrit-instructions-truncation-note "[truncated: file exceeds efrit-instructions-max-file-size]")

;;; Finding files

(defun efrit-instructions--listing (dir)
  "What one round trip says DIR holds: `local' for a local DIR, else its entry names.
A remote directory that cannot be listed yields nil: nothing under it."
  (if (file-remote-p dir)
      (ignore-errors (directory-files dir))
    'local))

(defun efrit-instructions--candidate-paths (dir names &optional listing)
  "Those of NAMES that can exist in DIR, as absolute paths, in order.
Local DIR: all of them; a stat each is cheap.  Remote DIR: only the
names whose first component (`CLAUDE.md', or `.claude' of
`.claude/CLAUDE.md') is in DIR's LISTING (fetched here when not given).
Most ancestors hold none, so a walk up a remote tree costs one round
trip per directory instead of one per candidate name: five names over
nine ancestors was 45 TRAMP round trips before the first review of a
remote-rooted session could start (2026-11-11)."
  (let ((listing (or listing (efrit-instructions--listing dir))))
    (mapcar (lambda (name) (expand-file-name name dir))
            (if (eq listing 'local)
                names
              (cl-remove-if-not
               (lambda (name) (member (car (split-string name "/" t)) listing))
               names)))))

(defun efrit-instructions--readable-file-p (path)
  "Non-nil when PATH is a readable file, not a directory."
  (and (file-readable-p path) (not (file-directory-p path))))

(defun efrit-instructions--first-in (dir names &optional listing)
  "The first of NAMES that is a readable file in DIR, as an absolute path, or nil.
LISTING is DIR's `efrit-instructions--listing' when the caller has it."
  (cl-some (lambda (path) (and (efrit-instructions--readable-file-p path) path))
           (efrit-instructions--candidate-paths dir names listing)))

(defun efrit-instructions--ancestors (root)
  "Directories strictly above ROOT, outermost first, on ROOT's host."
  (let ((dirs nil)
        (dir (file-name-directory (directory-file-name root))))
    (while (and dir (not (member dir dirs)))
      (push dir dirs)
      (let ((parent (file-name-directory (directory-file-name dir))))
        (setq dir (and parent (not (equal parent dir)) parent))))
    dirs))

(defun efrit-instructions-locate (&optional root)
  "The instruction files that apply to ROOT (default the project root), in reading order.
Each element is (PATH . LAYER), LAYER one of `user', `ancestor',
`project', `local'.  Nothing is read.  A remote ROOT's list is reused
for `efrit-instructions-remote-cache-seconds'."
  (let ((root (file-name-as-directory (or root (efrit-tool--get-project-root)))))
    (if (not (file-remote-p root))
        (efrit-instructions--locate-1 root)
      (nth 2 (or (efrit-instructions--remote-entry root)
                 (efrit-instructions--remote-remember
                  root (efrit-instructions--locate-1 root)))))))

(defun efrit-instructions--locate-1 (root)
  "`efrit-instructions-locate' for directory ROOT, uncached."
  (let ((found nil))
    (dolist (f efrit-instructions-user-files)
      (let ((path (expand-file-name f)))
        (when (efrit-instructions--readable-file-p path)
          (push (cons path 'user) found))))
    (when efrit-instructions-ancestors
      (dolist (dir (efrit-instructions--ancestors root))
        (when-let* ((path (efrit-instructions--first-in dir efrit-instructions-files)))
          (push (cons path 'ancestor) found))))
    ;; the project directory is listed once for both its layers
    (let ((listing (efrit-instructions--listing root)))
      (when-let* ((path (efrit-instructions--first-in root efrit-instructions-files listing)))
        (push (cons path 'project) found))
      (dolist (path (efrit-instructions--candidate-paths root efrit-instructions-local-files listing))
        (when (efrit-instructions--readable-file-p path)
          (push (cons path 'local) found))))
    ;; A user file that is also an ancestor (project under ~) would
    ;; appear twice; keep the first occurrence.
    (cl-remove-duplicates (nreverse found) :key #'car :test #'equal :from-end t)))

;;; Reading, with imports

(defun efrit-instructions--read-file (path)
  "PATH's text, cut at `efrit-instructions-max-file-size' with a note.  nil on error."
  (condition-case err
      (let* ((size (file-attribute-size (file-attributes path)))
             (cut (and size (> size efrit-instructions-max-file-size))))
        (with-temp-buffer
          (insert-file-contents path nil 0 (and cut efrit-instructions-max-file-size))
          (when cut
            (goto-char (point-max))
            (insert "\n" efrit-instructions-truncation-note "\n"))
          (buffer-string)))
    (error
     (efrit-log 'warn "instructions: cannot read %s: %s" path (error-message-string err))
     nil)))

(defun efrit-instructions--expand-imports (text base-dir depth seen)
  "Replace @PATH lines in TEXT with the named file's content.
Paths resolve against BASE-DIR.  DEPTH counts down; SEEN is the list of
files already on the import path, for cycle detection."
  (if (<= depth 0)
      text
    (replace-regexp-in-string
     efrit-instructions--import-regexp
     (lambda (line)
       (let* ((rel (match-string 1 line))
              (path (expand-file-name rel base-dir))
              (label (abbreviate-file-name path)))
         (cond
          ((member path seen)
           (format "[import skipped: %s is already being imported]" label))
          ((not (and (file-readable-p path) (not (file-directory-p path))))
           (format "[import not found: %s]" label))
          (t
           (let ((body (efrit-instructions--read-file path)))
             (if (null body)
                 (format "[import unreadable: %s]" label)
               (concat (format "<!-- imported from %s -->\n" label)
                       (efrit-instructions--expand-imports
                        body (file-name-directory path) (1- depth) (cons path seen))
                       (format "\n<!-- end of %s -->" label))))))))
     text t t)))

(defun efrit-instructions--section (path layer)
  "One labelled section for PATH at LAYER, or nil if unreadable."
  (when-let* ((body (efrit-instructions--read-file path)))
    (let ((expanded (efrit-instructions--expand-imports
                     body (file-name-directory path)
                     efrit-instructions-max-import-depth (list path))))
      (format "### %s (%s)\n\n%s\n"
              (abbreviate-file-name path)
              (pcase layer
                ('user "your user-level instructions")
                ('ancestor "from a directory above the project")
                ('project "project instructions")
                ('local "local, unversioned overrides")
                (_ (symbol-name layer)))
              (string-trim-right expanded)))))

;;; Assembly

(defun efrit-instructions--fit (sections)
  "Trim SECTIONS (strings, least specific first) to `efrit-instructions-max-total-size'.
Earlier sections are cut first, each to a stub that names what was dropped."
  (let ((total (apply #'+ (mapcar #'length sections)))
        (result (copy-sequence sections))
        (i 0))
    (while (and (> total efrit-instructions-max-total-size) (< i (length result)))
      (let* ((s (nth i result))
             (head (car (split-string s "\n")))
             (stub (format "%s\n\n[omitted: instructions block exceeds efrit-instructions-max-total-size]\n" head)))
        (setq total (+ (- total (length s)) (length stub)))
        (setf (nth i result) stub)
        (cl-incf i)))
    result))

(defun efrit-instructions-text (&optional root)
  "The assembled instructions block for ROOT, or nil when no file applies.
Sections are ordered least to most specific; the preamble tells the
model that later sections take precedence."
  (let* ((root (file-name-as-directory (or root (efrit-tool--get-project-root))))
         (entry (and (file-remote-p root)
                     (progn (efrit-instructions-locate root)
                            (efrit-instructions--remote-entry root)))))
    (if (and entry (nth 3 entry))
        (car (nth 3 entry))
      (let* ((located (efrit-instructions-locate root))
             (sections (delq nil (mapcar (lambda (e) (efrit-instructions--section (car e) (cdr e)))
                                         located)))
             (text (when sections
                     (concat
                      "PROJECT-SPECIFIC INSTRUCTIONS\n"
                      "These come from the user's instruction files, listed from most general to most\n"
                      "specific.  Where they conflict, the later (more specific) file wins.  Follow them.\n\n"
                      (string-join (efrit-instructions--fit sections) "\n")))))
        (when entry (setf (nth 3 entry) (list text)))
        text))))

(defun efrit-instructions-for-prompt (&optional root)
  "The instructions block wrapped for the system prompt, or an empty string."
  (if-let* ((text (efrit-instructions-text root)))
      (concat "\n\n" text "\n\n")
    ""))

;;; Inspection

;;;###autoload
(defun efrit-instructions-show (&optional root)
  "Show which instruction files apply to ROOT and the block the model receives."
  (interactive)
  (require 'efrit-ui-helpers)
  (let* ((root (or root (efrit-tool--get-project-root)))
         (located (efrit-instructions-locate root))
         (listing (if located
                      (mapconcat (lambda (e) (format "  %-9s %s" (cdr e) (abbreviate-file-name (car e))))
                                 located "\n")
                    "  (none)")))
    (efrit-show-popup
     "*efrit-instructions*"
     (concat (format "Instruction files for %s\n\n%s\n\n" (abbreviate-file-name root) listing)
             (or (efrit-instructions-text root) "(no instructions)"))
     'markdown-mode)))

(provide 'efrit-instructions)

;;; efrit-instructions.el ends here
