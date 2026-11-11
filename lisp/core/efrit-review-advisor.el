;;; efrit-review-advisor.el --- A decision model's second opinion for the reviewer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.18.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; The reviewer (efrit-review) is a general model that decides whether
;; a batch of proposed tool calls may run.  This file gives it an
;; advisor: a decision model (efrit-decisions) asked, per call, two
;; fixed questions -- does the user's request cover this action, and
;; how much harm could it do -- and answers in a few hundred
;; milliseconds with numbers.  The answers go under the call in the
;; reviewer's view as an [ADVISOR …] line, next to the FLAG lines, and
;; into the review's transcript details.
;;
;; Leader and advisor.  The reviewer decides; the advisor informs.
;; Nothing here grants, approves, raises a confidence, or changes a
;; verdict.  An advisor that is slow (past `efrit-review-advisor-timeout')
;; or down leaves one line saying so and the review goes on as before.
;; No decision backend configured: nothing happens at all.  This is
;; the user's standing instruction (tzz, 2026-11-10): the review stays
;; with an independent general model because the decisions API is new
;; and few models offer it; a second opinion is welcome on those terms.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-log)

(declare-function efrit-decisions-available-p "efrit-decisions")
(declare-function efrit-decide-many "efrit-decisions" (inputs questions callback &rest args))
(declare-function efrit-decisions-answer "efrit-decisions" (answers name))
(declare-function efrit-decisions-question "efrit-decisions" (type name instructions &rest args))

(defgroup efrit-review-advisor nil
  "A decision model's second opinion beside the reviewer."
  :group 'efrit-review
  :prefix "efrit-review-advisor-")

(defcustom efrit-review-advisor-enabled t
  "When non-nil and a decision backend exists, the reviewer sees ADVISOR lines."
  :type 'boolean)

(defcustom efrit-review-advisor-backend nil
  "The decision backend the advisor uses; nil means the default one."
  :type '(choice (const nil) string))

(defcustom efrit-review-advisor-timeout 3
  "Seconds the review waits for the advisor before going on without it."
  :type 'number)

(defcustom efrit-review-advisor-max-chars 1500
  "Longest text of one proposed call shown to the advisor."
  :type 'integer)

(defun efrit-review-advisor-questions ()
  "The two questions the advisor answers about one proposed call."
  (list
   (efrit-decisions-question
    'predicate "asked"
    "The USER REQUEST and the user's STANDING INSTRUCTIONS are given, then one PROPOSED ACTION an AI agent wants to perform in the user's editor. Does the request or the instructions cover this action, directly or as an evident step toward what was asked?")
   (efrit-decisions-question
    'score "risk"
    "How much harm could this PROPOSED ACTION do if it is not what the user wanted?"
    :levels '(("none" . "reads or searches; nothing changes")
              ("low" . "a change inside the project that is easy to undo")
              ("medium" . "a change outside the project, or hard to undo")
              ("high" . "deletes, sends data out, changes configuration or credentials, runs commands with wide effects")))))

(defun efrit-review-advisor--input (intent standing call-text)
  "The text the advisor reads for one call."
  (concat "USER REQUEST:\n" (or intent "(not available)")
          (when (and standing (not (string-empty-p standing)))
            (concat "\n\nSTANDING INSTRUCTIONS:\n" standing))
          "\n\nPROPOSED ACTION:\n"
          (truncate-string-to-width call-text efrit-review-advisor-max-chars nil nil "…")))

(defun efrit-review-advisor-available-p ()
  "Non-nil when the advisor can be asked: enabled and a decision backend exists."
  (and efrit-review-advisor-enabled
       (require 'efrit-decisions nil t)
       (efrit-decisions-available-p)))

(defun efrit-review-advisor-format (answers err)
  "The [ADVISOR …] line for one call from ANSWERS, or from ERR when it failed."
  (cond
   (err (format "[ADVISOR: unavailable (%s)]" err))
   (t (let* ((asked (efrit-decisions-answer answers "asked"))
             (risk (efrit-decisions-answer answers "risk"))
             (p (alist-get 'probability asked))
             (level (alist-get 'level risk))
             (score (alist-get 'score risk)))
        (format "[ADVISOR: asked %.2f, risk %s %.1f%s]"
                (or p 0) (or level "?") (or score 0)
                (cond ((and p (< p 0.3) level (member level '("medium" "high")))
                       " — the request does not seem to cover this and it could hurt; look closely")
                      ((and p (< p 0.3)) " — the request does not seem to cover this")
                      ((and level (equal level "high")) " — high potential harm")
                      (t "")))))))

(defun efrit-review-advisor-ask (intent standing call-texts callback)
  "Ask the advisor about each of CALL-TEXTS; CALLBACK gets one line per call.
INTENT is the user's request, STANDING their instructions.  CALLBACK is
(lambda (LINES)), LINES a list as long as CALL-TEXTS, each an
[ADVISOR …] string or nil.  It is called once, within
`efrit-review-advisor-timeout' seconds: with the answers, or with
\"unavailable\" lines when the advisor is slow or down.  Returns nil at
once, with CALLBACK called with nils, when the advisor is not
available."
  (if (not (and (efrit-review-advisor-available-p) call-texts))
      (progn (funcall callback (make-list (length call-texts) nil)) nil)
    (let* ((inputs (mapcar (lambda (c) (efrit-review-advisor--input intent standing c)) call-texts))
           (questions (efrit-review-advisor-questions))
           (done nil) (timer nil) (cancel nil)
           (finish (lambda (lines why)
                     (unless done
                       (setq done t)
                       (when timer (cancel-timer timer))
                       (when why
                         (when cancel (ignore-errors (funcall cancel)))
                         (efrit-log 'info "review advisor: %s" why))
                       (funcall callback lines)))))
      (condition-case err
          (progn
            (setq cancel
                  (efrit-decide-many
                   inputs questions
                   (lambda (results)
                     (funcall finish
                              (mapcar (lambda (r) (efrit-review-advisor-format (nth 1 r) (nth 2 r))) results)
                              nil))
                   :backend efrit-review-advisor-backend))
            (setq timer (run-at-time efrit-review-advisor-timeout nil
                                     (lambda ()
                                       (funcall finish
                                                (make-list (length call-texts)
                                                           (format "[ADVISOR: unavailable (no answer in %s s)]"
                                                                   efrit-review-advisor-timeout))
                                                "timed out; the review goes on without it")))))
        (error
         (funcall finish (make-list (length call-texts)
                                    (format "[ADVISOR: unavailable (%s)]" (error-message-string err)))
                  (error-message-string err))))
      t)))

(provide 'efrit-review-advisor)

;;; efrit-review-advisor.el ends here
