;;; efrit-documents-gcalendar.el --- Meeting attachments from Google Calendar -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; A related-documents provider for `efrit-documents': given an item
;; with a title and a date (a meeting recap's subject and Date), find
;; the calendar event it is about and return the event's attachments.
;; Calendar's "take meeting notes" Doc is such an attachment.  Title
;; search alone misses it when the meeting was renamed after the notes
;; were created, or the notes were named after an earlier title; the
;; event keeps the attachment whatever either is called now.
;;
;; How the event is found: `events.list' on each calendar in
;; `efrit-documents-gcalendar-calendars' for the window of
;; `efrit-documents-related-days' around the date (recurring events
;; expanded to instances), then the instances are scored against the
;; item: shared title words count, an organizer or attendee whose
;; address appears in the item's :from or :participants counts, and
;; closeness in time breaks ties.  Events with no attachments are
;; ignored.  The best few (`efrit-documents-gcalendar-max-events') give
;; their attachments as documents of the Drive source (by fileId), so
;; `efrit-documents-fetch' exports them like any other Doc.
;;
;; Credentials: an auth-source entry named
;; `efrit-documents-gcalendar-auth-host' (default "gcalendar"), else the
;; Drive/Gmail entry when its :scope carries a Calendar scope.  Add
;; https://www.googleapis.com/auth/calendar.events.readonly (or
;; calendar.readonly) to the scope and re-consent once
;; (`efrit-auth-reauthorize').  `M-x efrit-documents-gcalendar-check'
;; lists the calendars the token can read.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-documents)
(require 'efrit-documents-gdrive)
(require 'efrit-auth)

(defgroup efrit-documents-gcalendar nil
  "Google Calendar as a provider of documents related to a meeting."
  :group 'efrit-documents
  :prefix "efrit-documents-gcalendar-")

(defcustom efrit-documents-gcalendar-auth-host "gcalendar"
  "The auth-source host of the entry used for Calendar."
  :type 'string)

(defcustom efrit-documents-gcalendar-auth-hosts '("gdrive" "gmail" "gmail.com" "imap.gmail.com")
  "Hosts tried, in order, when `efrit-documents-gcalendar-auth-host' has no entry.
An entry counts only if its :scope names a Calendar scope."
  :type '(repeat string))

(defcustom efrit-documents-gcalendar-user nil
  "The auth-source user to look up, or nil for any."
  :type '(choice (const nil) string))

(defcustom efrit-documents-gcalendar-calendars '("primary")
  "Calendar ids searched: \"primary\", an address, or a shared calendar's id."
  :type '(repeat string))

(defcustom efrit-documents-gcalendar-max-events 2
  "Most events whose attachments are returned for one item."
  :type 'integer)

(defcustom efrit-documents-gcalendar-min-score 2
  "Least score an event needs to count as the item's meeting.
One shared title word is 1; an organizer or attendee named in the item
is 2; the same day is 1."
  :type 'integer)

(defconst efrit-documents-gcalendar--api "https://www.googleapis.com/calendar/v3"
  "Calendar v3 endpoint.")

(defconst efrit-documents-gcalendar--scope "https://www.googleapis.com/auth/calendar.events.readonly"
  "The scope the token needs (calendar.readonly also works).")

(defvar efrit-documents-gcalendar--host nil
  "The auth-source host found to work, once looked up.")

;;;; Credentials

(defun efrit-documents-gcalendar--scoped-p (entry)
  "Whether auth-source ENTRY's scope includes a Calendar scope."
  (string-match-p "googleapis\\.com/auth/calendar" (or (efrit-auth-get entry :scope) "")))

(defun efrit-documents-gcalendar--find-host ()
  "The auth-source host to use, finding and remembering it on first call."
  (or efrit-documents-gcalendar--host
      (let ((tried nil) (found nil))
        (dolist (host (cons efrit-documents-gcalendar-auth-host efrit-documents-gcalendar-auth-hosts))
          (unless found
            (push host tried)
            (condition-case nil
                (let ((entry (efrit-auth-credentials host efrit-documents-gcalendar-user)))
                  (when (or (equal host efrit-documents-gcalendar-auth-host)
                            (efrit-documents-gcalendar--scoped-p entry))
                    (setq found host)))
              (efrit-auth-no-credentials nil))))
        (unless found
          (signal 'efrit-auth-no-credentials
                  (list (format "no auth-source entry for Calendar: tried hosts %s; add %s to the scope of your Google entry and run M-x efrit-auth-reauthorize"
                                (string-join (nreverse tried) ", ") efrit-documents-gcalendar--scope))))
        (setq efrit-documents-gcalendar--host found))))

(defun efrit-documents-gcalendar--request (path &optional params)
  "GET PATH under the Calendar endpoint with PARAMS."
  (condition-case err
      (efrit-auth-request (efrit-documents-gcalendar--find-host) "GET"
                          (concat efrit-documents-gcalendar--api path)
                          :user efrit-documents-gcalendar-user :params params)
    (efrit-auth-http-error
     (signal 'efrit-documents-error (list (efrit-auth-explain err))))))

;;;; Events

(defun efrit-documents-gcalendar--events (since until)
  "Event instances with attachments on the configured calendars between SINCE and UNTIL."
  (let ((out nil))
    (dolist (calendar efrit-documents-gcalendar-calendars)
      (let ((result (efrit-documents-gcalendar--request
                     (format "/calendars/%s/events" (url-hexify-string calendar))
                     `(("timeMin" . ,(format-time-string "%FT%TZ" since t))
                       ("timeMax" . ,(format-time-string "%FT%TZ" until t))
                       ("singleEvents" . "true")
                       ("orderBy" . "startTime")
                       ("maxResults" . "250")
                       ("fields" . "items(id,summary,htmlLink,start,organizer,attendees,attachments)")))))
        (dolist (event (alist-get 'items result))
          (when (alist-get 'attachments event)
            (push event out)))))
    (nreverse out)))

(defun efrit-documents-gcalendar--event-time (event)
  "EVENT's start as an Emacs time, or nil."
  (let ((start (alist-get 'start event)))
    (efrit-documents--time (or (alist-get 'dateTime start) (alist-get 'date start)))))

(defun efrit-documents-gcalendar--addresses (item)
  "Lower-cased mail addresses named in ITEM's :from and :participants."
  (let ((text (string-join (delq nil (list (plist-get item :from)
                                            (and (listp (plist-get item :participants))
                                                 (string-join (plist-get item :participants) " "))
                                            (and (stringp (plist-get item :participants))
                                                 (plist-get item :participants))))
                           " "))
        (out nil) (start 0))
    (while (string-match "[[:alnum:]._%+-]+@[[:alnum:].-]+" text start)
      (push (downcase (match-string 0 text)) out)
      (setq start (match-end 0)))
    out))

(defun efrit-documents-gcalendar-score (event item date)
  "How well EVENT matches ITEM dated DATE: shared title words, people, same day."
  (let* ((words (efrit-documents-title-words (plist-get item :title)))
         (event-words (efrit-documents-title-words (alist-get 'summary event)))
         (shared (length (cl-intersection words event-words :test #'equal)))
         (addresses (efrit-documents-gcalendar--addresses item))
         (people (delq nil (cons (alist-get 'email (alist-get 'organizer event))
                                 (mapcar (lambda (a) (alist-get 'email a)) (alist-get 'attendees event)))))
         (people (mapcar #'downcase people))
         (named (if (cl-intersection addresses people :test #'equal) 2 0))
         (event-time (efrit-documents-gcalendar--event-time event))
         (same-day (if (and date event-time
                            (equal (format-time-string "%F" date t) (format-time-string "%F" event-time t)))
                       1 0)))
    (+ shared named same-day)))

(defun efrit-documents-gcalendar--attachment-doc (attachment event)
  "A document plist for ATTACHMENT of EVENT: a Drive file by id when it is one."
  (let* ((file-id (alist-get 'fileId attachment))
         (url (alist-get 'fileUrl attachment))
         (drive-id (or file-id (and url (efrit-documents-source-match
                                          (or (efrit-documents-source "gdrive")
                                              (efrit-documents-gdrive-register))
                                          url)))))
    (when drive-id
      (list :source "gdrive"
            :id drive-id
            :title (or (alist-get 'title attachment) drive-id)
            :url (or url (format "https://docs.google.com/document/d/%s" drive-id))
            :modified (or (alist-get 'dateTime (alist-get 'start event)) (alist-get 'date (alist-get 'start event)))
            :kind (if (equal (alist-get 'mimeType attachment) "application/vnd.google-apps.document") "doc" "file")
            :event (alist-get 'summary event)
            :event-url (alist-get 'htmlLink event)))))

(defun efrit-documents-gcalendar-related (item)
  "For `efrit-documents-related-functions': attachments of ITEM's meeting."
  (let ((date (efrit-documents--time (plist-get item :date))))
    (when date
      (let* ((window (* efrit-documents-related-days 24 3600))
             (events (efrit-documents-gcalendar--events (time-subtract date window) (time-add date window)))
             (scored (delq nil (mapcar (lambda (e)
                                         (let ((score (efrit-documents-gcalendar-score e item date)))
                                           (and (>= score efrit-documents-gcalendar-min-score) (cons score e))))
                                       events)))
             (best (seq-take (sort scored (lambda (a b) (> (car a) (car b))))
                             efrit-documents-gcalendar-max-events))
             (out nil))
        (dolist (entry best)
          (dolist (attachment (alist-get 'attachments (cdr entry)))
            (when-let* ((doc (efrit-documents-gcalendar--attachment-doc attachment (cdr entry))))
              (push doc out))))
        (nreverse out)))))

(add-hook 'efrit-documents-related-functions #'efrit-documents-gcalendar-related)

;;;###autoload
(defun efrit-documents-gcalendar-check ()
  "Say which auth-source entry Calendar uses and list the calendars it can read."
  (interactive)
  (setq efrit-documents-gcalendar--host nil)
  (condition-case err
      (let* ((host (efrit-documents-gcalendar--find-host))
             (list (efrit-documents-gcalendar--request "/users/me/calendarList"
                                                       '(("fields" . "items(id,summary,primary)")))))
        (message "efrit-documents: Calendar works through auth-source host %S; calendars: %s"
                 host (mapconcat (lambda (c) (format "%s%s" (alist-get 'summary c)
                                                     (if (eq (alist-get 'primary c) t) " (primary)" "")))
                                 (alist-get 'items list) ", ")))
    (error (message "efrit-documents: Calendar is not usable: %s" (efrit-documents-explain err)))))

(provide 'efrit-documents-gcalendar)

;;; efrit-documents-gcalendar.el ends here
