;;; efrit-pill.el --- Small SVG pills and badges for labels and states -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.12.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai, faces

;;; Commentary:

;; A PILL is a short text drawn on a rounded rectangle: the tab line's
;; agent tabs, the dashboard's states and folders, the diagnosis
;; severities, the review badges.  One primitive so they all match.
;;
;; `efrit-pill' returns TEXT with a `display' image.  The colour comes
;; from a KEY (the same key always gives the same hue, so a project or
;; an agent keeps its colour everywhere) or from an explicit STATE
;; (`efrit-pill-state-colors': working, waiting, unread, idle, failed,
;; ok, info, warning, blocked), or from BG/FG given outright.  Without
;; SVG (a terminal, batch) the same text comes back with a face, so
;; every caller can insert the result as is.
;;
;; `efrit-pill-function' lets a user with a pill drawer of their own
;; (tzz, 2026-10-14: "Use the same nice graphics I already have")
;; plug it in: it is called with the same keywords and its result is
;; used when non-nil.  Images are cached by their inputs and the cache
;; is dropped when the font metrics change.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'svg)

(defgroup efrit-pill nil
  "Pills and badges."
  :group 'efrit)

(defcustom efrit-pill-function nil
  "A function that draws pills instead of `efrit-pill', or nil.
Called with (TEXT &key key bg fg scale help-echo); its non-nil result
is used as is.  For users who already have a pill drawer."
  :type '(choice (const nil) function)
  :group 'efrit-pill)

(defcustom efrit-pill-scale 0.9
  "Pill height as a fraction of the line height."
  :type 'number
  :group 'efrit-pill)

(defcustom efrit-pill-saturation 0.55
  "Saturation of the hue derived from a key."
  :type 'number
  :group 'efrit-pill)

(defcustom efrit-pill-background-lightness 0.86
  "Lightness of a pale (background) variant."
  :type 'number
  :group 'efrit-pill)

(defcustom efrit-pill-foreground-lightness 0.32
  "Lightness of a saturated (foreground) variant."
  :type 'number
  :group 'efrit-pill)

(defcustom efrit-pill-state-colors
  '((working . "#d98e04") (waiting . "#c0392b") (unread . "#6c3fc5") (idle . "#2e8b57")
    (failed . "#8b0000") (stopped . "#6e6e6e")
    (ok . "#2e8b57") (info . "#3b6ea5") (warning . "#d98e04") (blocked . "#b22222")
    (suggestion . "#3b6ea5") (comment . "#b7791f") (lgtm . "#2e8b57"))
  "The colour of each state a pill may show, as #rrggbb.
The pill takes a pale variant as background and the colour itself as text,
or the reverse when `:strong' is asked for."
  :type '(alist :key-type symbol :value-type string)
  :group 'efrit-pill)

(defface efrit-pill-fallback '((t :inherit bold :box (:line-width -1)))
  "A pill where no image can be drawn." :group 'efrit-pill)

(defvar efrit-pill--cache (make-hash-table :test 'equal))
(defvar efrit-pill--cache-key nil)

(defun efrit-pill-available-p ()
  "Non-nil when pills can be drawn as images here."
  (and (display-graphic-p) (image-type-available-p 'svg)))

(defun efrit-pill--hex (rgb)
  (apply #'color-rgb-to-hex (append rgb '(2))))

(defun efrit-pill-hue-color (key &optional light)
  "A stable colour for KEY as #rrggbb; LIGHT gives the pale variant."
  (let ((h (/ (mod (sxhash-equal key) 360) 360.0)))
    (efrit-pill--hex (color-hsl-to-rgb h efrit-pill-saturation
                                       (if light efrit-pill-background-lightness
                                         efrit-pill-foreground-lightness)))))

(defun efrit-pill-state-color (state &optional light)
  "The colour of STATE; LIGHT gives a pale variant of it.  Nil for an unknown state."
  (when-let* ((hex (alist-get state efrit-pill-state-colors)))
    (if (not light) hex
      (pcase-let* ((`(,r ,g ,b) (color-name-to-rgb hex))
                   (`(,h ,s ,_l) (color-rgb-to-hsl r g b)))
        (efrit-pill--hex (color-hsl-to-rgb h (* 0.9 s) efrit-pill-background-lightness))))))

(defun efrit-pill--reset-cache-if-needed ()
  (let ((key (cons (default-font-height) (default-font-width))))
    (unless (equal key efrit-pill--cache-key)
      (clrhash efrit-pill--cache)
      (setq efrit-pill--cache-key key))))

(defun efrit-pill--draw (text bg fg scale)
  "TEXT as a pill image string with BG and FG colours at SCALE."
  (let* ((height (max 12 (round (* scale (default-font-height)))))
         (font-size (round (* 0.62 height)))
         (family (face-attribute 'default :family))
         (width (+ (* (string-width text) (round (* 0.66 font-size))) (default-font-width)))
         (svg (svg-create width height)))
    (svg-rectangle svg 0 1 width (- height 2) :rx (/ height 2.0) :fill bg)
    (svg-text svg text :x (/ width 2.0) :y (/ height 2.0)
              :text-anchor "middle" :dominant-baseline "central"
              :font-family family :font-weight "bold" :font-size font-size :fill fg)
    (propertize text 'display (svg-image svg :ascent 'center :scale 1))))

(cl-defun efrit-pill (text &key key state strong bg fg (scale efrit-pill-scale) help-echo)
  "TEXT drawn as a pill.
KEY picks a stable hue (default TEXT); STATE picks a colour from
`efrit-pill-state-colors' instead; STRONG swaps to a saturated
background with pale text (the selected tab, an alarm); BG and FG
override everything.  SCALE is the height relative to the line.
HELP-ECHO becomes the tooltip.  Without SVG, TEXT with a face."
  (or (and efrit-pill-function
           (ignore-errors (funcall efrit-pill-function text :key key :bg bg :fg fg :scale scale :help-echo help-echo)))
      (progn
        (efrit-pill--reset-cache-if-needed)
        (let* ((key (or key text))
               (pale (or (and state (efrit-pill-state-color state t)) (efrit-pill-hue-color key t)))
               (deep (or (and state (efrit-pill-state-color state)) (efrit-pill-hue-color key)))
               (bg (or bg (if strong deep pale)))
               (fg (or fg (if strong pale deep)))
               (cache-key (list text bg fg scale))
               (drawn (or (gethash cache-key efrit-pill--cache)
                          (puthash cache-key
                                   (if (efrit-pill-available-p)
                                       (efrit-pill--draw text bg fg scale)
                                     (propertize text 'face (list :inherit 'efrit-pill-fallback :foreground deep)))
                                   efrit-pill--cache))))
          (if help-echo (propertize drawn 'help-echo help-echo) (copy-sequence drawn))))))

(defun efrit-pill-state (state &optional text)
  "A pill naming STATE (or TEXT) in STATE's colour."
  (efrit-pill (or text (symbol-name state)) :state state))

(provide 'efrit-pill)

;;; efrit-pill.el ends here
