;;; efrit-tool-defs.el --- Tools the user and the model define together -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.20.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; efrit's tools are fixed at build time; every user has routines it
;; does not know.  This file reads tool definitions from JSON (global
;; `tools.json' under `efrit-data-directory', per project
;; `.efrit/tools.json'), registers each through `efrit-register-tool'
;; so the schema, dispatch and permission tables need nothing new,
;; and offers the model `define_tool' / `undefine_tool' / `list_tools'
;; to write them.  A watcher notices the model repeating the same
;; short sequence of calls in a session and says so once, so the
;; model proposes a definition and the user approves it at the
;; sandbox prompt.  Design: docs/designs/user-defined-tools.md.
;;
;; A definition's body is `elisp' (one form, evaluated with `input'
;; bound, exactly as eval_sexp evaluates: inspected, guarded, input
;; blocked) or `template' (a shell line with {{field}} placeholders,
;; run as shell_exec runs).  A definition can do nothing those two
;; tools cannot.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'efrit-log)
(require 'efrit-config)
(require 'efrit-tool-registry)
(require 'efrit-settings)
(require 'efrit-sandbox)

(defgroup efrit-tool-defs nil
  "Tools defined in JSON by the user and the model."
  :group 'efrit)

(defcustom efrit-tool-defs-enabled t
  "Non-nil loads the JSON tool definitions and offers define_tool."
  :type 'boolean
  :group 'efrit-tool-defs)

(defcustom efrit-tool-defs-repeat-threshold 3
  "How many times the same short call sequence repeats before a note is published."
  :type 'integer
  :group 'efrit-tool-defs)

(defconst efrit-tool-defs-file-name "tools.json")
(defconst efrit-tool-defs-classes '("read" "write" "exec" "net"))
(defconst efrit-tool-defs-kinds '("elisp" "template"))
(defconst efrit-tool-defs-package 'efrit-tool-defs
  "The `efrit-register-tool' package every definition is registered under.")

(declare-function efrit-tool--get-project-root "efrit-tool-utils")
(declare-function efrit-sandbox-eval-inspect "efrit-sandbox-eval" (form))
(declare-function efrit-sandbox-approve-once "efrit-sandbox" (cap target tool detail))
(declare-function efrit-tools--eval-with-context "efrit-tools" (sexp))
(declare-function efrit-do--handle-shell-exec "efrit-do-handlers" (input-str))
(declare-function efrit-publish "efrit-events" (type &optional data))
(defvar efrit-current-session-id)

;;;; Files

(defun efrit-tool-defs-global-file ()
  "The global definitions file."
  (efrit-config-data-file efrit-tool-defs-file-name))

(defun efrit-tool-defs-project-file (&optional root)
  "ROOT's definitions file (the current project's when nil)."
  (let ((root (or root (ignore-errors (efrit-tool--get-project-root)))))
    (when root
      (expand-file-name efrit-tool-defs-file-name
                        (expand-file-name efrit-settings-dir root)))))

(defun efrit-tool-defs--read-file (file)
  "The definitions in FILE as a list of hash tables; nil when absent or broken."
  (when (and file (file-readable-p file))
    (condition-case err
        (let ((obj (with-temp-buffer
                     (insert-file-contents file)
                     (json-parse-buffer :object-type 'hash-table :array-type 'list))))
          (cond
           ((and (hash-table-p obj) (listp (gethash "tools" obj))) (gethash "tools" obj))
           ((listp obj) obj)
           (t (efrit-log 'warn "tool definitions %s: not a list or {\"tools\": [...]}, ignoring" file)
              nil)))
      (error (efrit-log 'warn "tool definitions %s: unreadable (%s), ignoring" file (error-message-string err))
             nil))))

(defun efrit-tool-defs--write-file (file defs)
  "Write DEFS (hash tables) to FILE as {\"tools\": [...]}; delete FILE when DEFS is empty."
  (if (null defs)
      (when (file-exists-p file) (delete-file file))
    (let ((obj (make-hash-table :test 'equal)))
      (puthash "version" 1 obj)
      (puthash "tools" (vconcat (mapcar #'efrit-settings--serializable defs)) obj)
      (efrit-settings-write-json file obj)))
  file)

;;;; Validation

(defun efrit-tool-defs--get (def key)
  (and (hash-table-p def) (gethash key def)))

(defun efrit-tool-defs--template-fields (source)
  "The {{field}} names in SOURCE."
  (let ((pos 0) (fields nil))
    (while (string-match "{{\\([A-Za-z_][A-Za-z0-9_]*\\)}}" source pos)
      (push (match-string 1 source) fields)
      (setq pos (match-end 0)))
    (nreverse fields)))

(defun efrit-tool-defs--schema-fields (schema)
  "The property names of the JSON SCHEMA (a hash table or alist)."
  (let ((props (cond ((hash-table-p schema) (gethash "properties" schema))
                     ((listp schema) (cdr (assoc "properties" schema))))))
    (cond ((hash-table-p props) (hash-table-keys props))
          ((listp props) (mapcar (lambda (p) (format "%s" (car p))) props)))))

(defun efrit-tool-defs-validate (def)
  "Nil when DEF (a hash table) is a usable definition, else why not."
  (let* ((name (efrit-tool-defs--get def "name"))
         (class (efrit-tool-defs--get def "class"))
         (body (efrit-tool-defs--get def "body"))
         (kind (efrit-tool-defs--get body "kind"))
         (source (efrit-tool-defs--get body "source"))
         (schema (efrit-tool-defs--get def "input_schema")))
    (cond
     ((not (hash-table-p def)) "a definition is a JSON object")
     ((not (and (stringp name) (let ((case-fold-search nil)) (string-match-p "\\`[a-z][a-z0-9_]*\\'" name))))
      "name must be a lowercase identifier (letters, digits, _)")
     ((efrit-tool-registry--builtin-p name) (format "%s is one of efrit's own tools" name))
     ((not (stringp (efrit-tool-defs--get def "description"))) "description (a string) is required")
     ((not (member class efrit-tool-defs-classes))
      (format "class must be one of %s" (mapconcat #'identity efrit-tool-defs-classes ", ")))
     ((and schema (not (hash-table-p schema))) "input_schema must be a JSON object")
     ((not (member kind efrit-tool-defs-kinds))
      (format "body.kind must be one of %s" (mapconcat #'identity efrit-tool-defs-kinds ", ")))
     ((not (and (stringp source) (not (string-empty-p (string-trim source))))) "body.source is required")
     ((equal kind "elisp")
      (condition-case err
          (let ((form (efrit-tool-defs--read-form source)))
            (when (require 'efrit-sandbox-eval nil t)
              (when-let* ((why (efrit-sandbox-eval-inspect form)))
                (format "body.source: %s" why))))
        (error (format "body.source does not read as one Lisp form: %s" (error-message-string err)))))
     ((equal kind "template")
      (let ((unknown (cl-set-difference (efrit-tool-defs--template-fields source)
                                        (efrit-tool-defs--schema-fields schema) :test #'equal)))
        (when unknown
          (format "template names fields the schema does not declare: %s" (mapconcat #'identity unknown ", "))))))))

(defun efrit-tool-defs--read-form (source)
  "SOURCE as one Lisp form; signals when it holds none or more than one."
  (let* ((start 0)
         (first (read-from-string source start))
         (rest (string-trim (substring source (cdr first)))))
    (unless (string-empty-p rest)
      (error "more than one form (after the first: %s)" (truncate-string-to-width rest 40 nil nil "…")))
    (car first)))

;;;; Running

(defun efrit-tool-defs--fill-template (source input)
  "SOURCE with each {{field}} replaced by INPUT's value, shell-quoted."
  (replace-regexp-in-string
   "{{\\([A-Za-z_][A-Za-z0-9_]*\\)}}"
   (lambda (m)
     (let ((v (cdr (assoc (match-string 1 m) input))))
       (shell-quote-argument (if (null v) "" (format "%s" v)))))
   source t t))

(defun efrit-tool-defs--run (def input)
  "Run definition DEF on INPUT (an alist with string keys); return the result string."
  (let* ((body (efrit-tool-defs--get def "body"))
         (kind (efrit-tool-defs--get body "kind"))
         (source (efrit-tool-defs--get body "source"))
         (name (efrit-tool-defs--get def "name")))
    (pcase kind
      ("elisp"
       (require 'efrit-tools)
       ;; `input' is what the body reads; the whole form goes through
       ;; eval_sexp's path: inspection, guards, input block, observation
       (let* ((form `(let ((input ',input)) ,(efrit-tool-defs--read-form source)))
              (data (efrit-tools--eval-with-context form)))
         (if (plist-get data :success)
             (concat (format "%s" (plist-get data :result))
                     (when-let* ((ms (plist-get data :messages)))
                       (format "\n[messages: %s]" (mapconcat #'identity ms " | ")))
                     (when-let* ((bs (plist-get data :changed-buffers)))
                       (format "\n[buffers changed: %s]" (mapconcat #'identity bs ", "))))
           (format "Error in %s: %s" name (plist-get data :error)))))
      ("template"
       (require 'efrit-do-handlers)
       (efrit-do--handle-shell-exec (efrit-tool-defs--fill-template source input)))
      (_ (format "\n[Error: %s has an unknown body kind %s]" name kind)))))

;;;; Loading into the registry

(defvar efrit-tool-defs--loaded nil
  "((FILE . MTIME) ...) of the files last loaded, to reload only on change.")

(defvar efrit-tool-defs--active nil
  "The definitions in force: alist of (NAME . (DEF . SCOPE)), SCOPE `project' or `global'.")

(defun efrit-tool-defs--files (&optional root)
  "The files that apply to ROOT: global first, then the project's."
  (delq nil (list (efrit-tool-defs-global-file) (efrit-tool-defs-project-file root))))

(defun efrit-tool-defs--mtimes (files)
  (mapcar (lambda (f) (cons f (and (file-exists-p f) (file-attribute-modification-time (file-attributes f)))))
          files))

(defun efrit-tool-defs-load (&optional root force)
  "Register the definitions that apply to ROOT; a project one shadows a global one.
Reads the files only when their modification time changed, unless
FORCE.  Returns the names in force."
  (when efrit-tool-defs-enabled
    (let* ((files (efrit-tool-defs--files root))
           (mtimes (efrit-tool-defs--mtimes files)))
      (when (or force (not (equal mtimes efrit-tool-defs--loaded)))
        (efrit-unregister-package-tools efrit-tool-defs-package)
        (setq efrit-tool-defs--active nil)
        (cl-loop for file in files
                 for scope in '(global project)
                 do (dolist (def (efrit-tool-defs--read-file file))
                      (let ((why (efrit-tool-defs-validate def)))
                        (if why
                            (efrit-log 'warn "tool definition %s in %s skipped: %s"
                                       (or (efrit-tool-defs--get def "name") "?") (abbreviate-file-name file) why)
                          (efrit-tool-defs--register def scope)))))
        (setq efrit-tool-defs--loaded mtimes)
        (efrit-log 'debug "tool definitions: %d in force" (length efrit-tool-defs--active))))
    (mapcar #'car efrit-tool-defs--active)))

(defun efrit-tool-defs--register (def scope)
  "Put DEF (SCOPE `global' or `project') into the registry and the active list."
  (let ((name (efrit-tool-defs--get def "name")))
    (efrit-register-tool name
                         :description (efrit-tool-defs--get def "description")
                         :input-schema (efrit-tool-defs--schema->alist
                                        (or (efrit-tool-defs--get def "input_schema")
                                            (let ((h (make-hash-table :test 'equal)))
                                              (puthash "type" "object" h)
                                              (puthash "properties" (make-hash-table :test 'equal) h)
                                              h)))
                         :function (lambda (input) (efrit-tool-defs--run def input))
                         :class (intern (efrit-tool-defs--get def "class"))
                         :package efrit-tool-defs-package)
    (setf (alist-get name efrit-tool-defs--active nil nil #'equal) (cons def scope))))

(defun efrit-tool-defs--schema->alist (value)
  "VALUE (parsed JSON: hash tables and lists) in the alist/vector shape of the schema."
  (cond
   ((hash-table-p value)
    (let (out)
      (maphash (lambda (k v) (push (cons k (efrit-tool-defs--schema->alist v)) out)) value)
      (nreverse out)))
   ((and (listp value) value) (vconcat (mapcar #'efrit-tool-defs--schema->alist value)))
   ((null value) [])
   (t value)))

(defun efrit-tool-defs-active (&optional name)
  "The active definitions as (NAME DEF SCOPE) lists, or the one called NAME."
  (if name
      (when-let* ((e (assoc name efrit-tool-defs--active)))
        (list name (cadr e) (cddr e)))
    (mapcar (lambda (e) (list (car e) (cadr e) (cddr e))) (reverse efrit-tool-defs--active))))

;;;; Defining, undefining, listing (the model's tools)

(defun efrit-tool-defs--scope-file (scope)
  (pcase scope
    ("global" (efrit-tool-defs-global-file))
    ("project" (or (efrit-tool-defs-project-file) (error "no project root for a project-scoped tool")))
    (_ (error "scope must be \"project\" or \"global\""))))

(defun efrit-tool-defs--json (def)
  "DEF as readable JSON, the text the user reads and may edit at the prompt."
  (let ((json-encoding-pretty-print t) (json-encoding-default-indentation "  "))
    (json-encode (efrit-settings--serializable def))))

(defun efrit-tool-defs-define (def scope &optional origin)
  "Write DEF (a hash table) into SCOPE's file and register it.  Returns a result string.
The sandbox asks the user about the write with the definition's JSON
in view; `e' at the prompt edits that JSON and the edited text is
what gets defined (validated again).  A denial signals
`efrit-sandbox-denied' as any write does."
  (when-let* ((why (efrit-tool-defs-validate def)))
    (error "define_tool: %s" why))
  (let ((file (efrit-tool-defs--scope-file scope)))
    (when origin (puthash "origin" origin def))
    ;; one approval per definition, no grant: the file is under
    ;; .efrit/, which no grant may cover
    (let ((answer (efrit-sandbox-approve-once 'write file "define_tool" (efrit-tool-defs--json def))))
      (when (stringp answer)
        (setq def (condition-case err
                      (json-parse-string answer :object-type 'hash-table :array-type 'list)
                    (error (error "the edited definition is not valid JSON: %s" (error-message-string err)))))
        (when-let* ((why (efrit-tool-defs-validate def)))
          (error "the edited definition: %s" why))
        (efrit-log 'info "define_tool: defining the user's edited definition")))
    (let* ((name (efrit-tool-defs--get def "name"))
           (defs (cl-remove name (efrit-tool-defs--read-file file)
                            :key (lambda (d) (efrit-tool-defs--get d "name")) :test #'equal)))
      (efrit-tool-defs--write-file file (append defs (list def)))
      (efrit-tool-defs-load nil t)
      (format "Defined %s (%s, %s class) in %s. It is available from your next call."
              name scope (efrit-tool-defs--get def "class") (abbreviate-file-name file)))))

(defun efrit-tool-defs-undefine (name scope)
  "Remove the definition NAME from SCOPE's file.  Returns a result string."
  (let* ((file (efrit-tool-defs--scope-file scope))
         (defs (efrit-tool-defs--read-file file))
         (rest (cl-remove name defs :key (lambda (d) (efrit-tool-defs--get d "name")) :test #'equal)))
    (if (= (length rest) (length defs))
        (format "No tool %s is defined in the %s file." name scope)
      (efrit-sandbox-approve-once 'write file "undefine_tool" (format "remove the tool %s" name))
      (efrit-tool-defs--write-file file rest)
      (efrit-tool-defs-load nil t)
      (format "Removed %s from %s." name (abbreviate-file-name file)))))

(defun efrit-tool-defs-list-text ()
  "The active definitions as text for the model or the user."
  (efrit-tool-defs-load)
  (if (null efrit-tool-defs--active)
      "No user-defined tools. define_tool creates one; docs/designs/user-defined-tools.md describes the format."
    (mapconcat
     (lambda (entry)
       (pcase-let* ((`(,name ,def ,scope) entry)
                    (origin (efrit-tool-defs--get def "origin")))
         (format "- %s [%s, %s]: %s%s"
                 name scope (efrit-tool-defs--get def "class") (efrit-tool-defs--get def "description")
                 (if (hash-table-p origin)
                     (format " (defined %s%s)" (or (gethash "when" origin) "?")
                             (if (gethash "pattern" origin) (format " from %s" (gethash "pattern" origin)) ""))
                   ""))))
     (efrit-tool-defs-active) "\n")))

;;;; Tool handlers (dispatch table entries)

(defun efrit-tool-defs--input-def (input)
  "The definition object in INPUT (hash table) as a hash table, from `definition'
\(an object or a JSON string)."
  (let ((def (gethash "definition" input)))
    (cond
     ((hash-table-p def) def)
     ((stringp def) (condition-case err
                        (json-parse-string def :object-type 'hash-table :array-type 'list)
                      (error (error "definition is not valid JSON: %s" (error-message-string err)))))
     (t (error "definition (an object) is required")))))

(defun efrit-tool-defs-handle-define (input)
  "define_tool: INPUT has definition, scope, optional pattern."
  (condition-case err
      (let* ((def (efrit-tool-defs--input-def input))
             (scope (or (gethash "scope" input) "project"))
             (origin (let ((h (make-hash-table :test 'equal)))
                       (puthash "when" (format-time-string "%FT%TZ" nil t) h)
                       (when-let* ((id (bound-and-true-p efrit-current-session-id))) (puthash "session" id h))
                       (when-let* ((p (gethash "pattern" input))) (puthash "pattern" p h))
                       h)))
        (format "\n[%s]" (efrit-tool-defs-define def scope origin)))
    (efrit-sandbox-denied (signal (car err) (cdr err)))
    (error (format "\n[Error: %s]" (error-message-string err)))))

(defun efrit-tool-defs-handle-undefine (input)
  "undefine_tool: INPUT has name and scope."
  (condition-case err
      (format "\n[%s]" (efrit-tool-defs-undefine (or (gethash "name" input) (error "name is required"))
                                                 (or (gethash "scope" input) "project")))
    (efrit-sandbox-denied (signal (car err) (cdr err)))
    (error (format "\n[Error: %s]" (error-message-string err)))))

(defun efrit-tool-defs-handle-list (_input)
  "list_tools."
  (format "\n[User-defined tools]\n%s" (efrit-tool-defs-list-text)))

;;;; Noticing a repetition

(defvar efrit-tool-defs--recent (make-hash-table :test 'equal)
  "SESSION-ID -> list of call keys, most recent first (capped).")

(defvar efrit-tool-defs--noted (make-hash-table :test 'equal)
  "SESSION-ID -> list of sequences already noted this session.")

(defconst efrit-tool-defs--recent-max 40)

(defun efrit-tool-defs--call-key (tool input)
  "TOOL's call reduced to its shape: name plus the head of an eval form or the
first word of a shell line.  Two calls with the same key are \"the same\"."
  (let ((get (lambda (k) (and (hash-table-p input) (gethash k input)))))
    (pcase tool
      ("eval_sexp"
       (let* ((expr (or (funcall get "expr") (and (stringp input) input) ""))
              (head (and (stringp expr) (string-match "\\`\\s-*(\\s-*\\([^][[:space:]()\"]+\\)" expr)
                         (match-string 1 expr))))
         (format "eval_sexp(%s)" (or head "?"))))
      ("shell_exec"
       (let* ((line (or (funcall get "command") (and (stringp input) input) ""))
              (word (and (stringp line) (car (split-string (string-trim line) "[[:space:]]+" t)))))
         (format "shell_exec(%s)" (or word "?"))))
      ("run_program"
       (format "run_program(%s)" (or (funcall get "program") "?")))
      (_ tool))))

(defun efrit-tool-defs--rotations (seq)
  "Every rotation of SEQ: the same routine seen from each of its calls."
  (cl-loop for i below (length seq)
           collect (append (seq-drop seq i) (seq-take seq i))))

(defun efrit-tool-defs--sequence-noted-p (id seq)
  "Non-nil when SEQ, or a rotation of it, was noted for session ID.
After the note, the next call extends the run by one and the detector
sees the same routine starting one call later."
  (let ((noted (gethash id efrit-tool-defs--noted)))
    (cl-some (lambda (r) (member r noted)) (efrit-tool-defs--rotations seq))))

(defun efrit-tool-defs--find-repeat (keys)
  "A sequence of 2 to 4 keys at the head of KEYS (most recent first) that
appears `efrit-tool-defs-repeat-threshold' times in a row, as a list in
call order; or nil."
  (cl-loop for len from 4 downto 2
           for needed = (* len efrit-tool-defs-repeat-threshold)
           when (>= (length keys) needed)
           thereis (let ((head (seq-take keys len)))
                     ;; n copies of one call are a loop, not a routine
                     (and (> (length (delete-dups (copy-sequence head))) 1)
                          (cl-loop for i from 1 below efrit-tool-defs-repeat-threshold
                                   always (equal head (seq-subseq keys (* i len) (* (1+ i) len))))
                          (reverse head)))))

(defun efrit-tool-defs-watch (tool input &optional session-id)
  "Record TOOL's call; when a short sequence repeats, publish a note once.
Returns the note text when one was made (the dispatcher appends it to
the result), else nil.  Calls to define_tool and the definitions
themselves are not watched."
  (when (and efrit-tool-defs-enabled
             (not (member tool '("define_tool" "undefine_tool" "list_tools")))
             (not (assoc tool efrit-tool-defs--active)))
    (let* ((id (or session-id (bound-and-true-p efrit-current-session-id)))
           (keys (cons (efrit-tool-defs--call-key tool input) (gethash id efrit-tool-defs--recent))))
      (puthash id (seq-take keys efrit-tool-defs--recent-max) efrit-tool-defs--recent)
      (when-let* ((seq (efrit-tool-defs--find-repeat keys))
                  ((not (efrit-tool-defs--sequence-noted-p id seq))))
        (push seq (gethash id efrit-tool-defs--noted))
        (let* ((pattern (mapconcat #'identity seq " → "))
               (text (format "you ran %s %d times this session; define_tool can make that one call the user approves once (list_tools shows what exists)"
                             pattern efrit-tool-defs-repeat-threshold)))
          (when (fboundp 'efrit-publish)
            (efrit-publish 'note `((:session-id . ,id) (:text . ,(concat "⚙ " text)) (:kind . tool-defs))))
          (efrit-log 'info "tool definitions: repetition noted for %s: %s" id pattern)
          (format "\n[note: %s]" text))))))

(defun efrit-tool-defs-forget (&optional session-id)
  "Drop SESSION-ID's repetition memory."
  (let ((id (or session-id (bound-and-true-p efrit-current-session-id))))
    (remhash id efrit-tool-defs--recent)
    (remhash id efrit-tool-defs--noted)))

;;;; Showing the user

;;;###autoload
(defun efrit-tool-defs-show ()
  "Show the user-defined tools in force for the current project."
  (interactive)
  (require 'efrit-ui-helpers)
  (efrit-show-popup "*efrit user tools*"
                    (concat (efrit-tool-defs-list-text) "\n\nFiles: "
                            (mapconcat #'abbreviate-file-name (efrit-tool-defs--files) ", "))))

(declare-function efrit-show-popup "efrit-ui-helpers" (name text &optional mode))

(provide 'efrit-tool-defs)

;;; efrit-tool-defs.el ends here
