(defpackage #:lem-config/commands
  (:use #:cl #:lem)
  (:export #:open-init-file
           #:*time-stamp-format*
           #:time-stamp)
  (:documentation "Custom commands."))

(in-package #:lem-config/commands)


;;; =============================================================================
;;; Window Layouts
;;; =============================================================================
;; src/commands/window.lisp
(define-command stack-window-layout () ()
  (lem-core/commands/window:split-active-window-horizontally)
  (lem-core/commands/window:next-window)
  (lem-core/commands/window:split-active-window-vertically))

(define-key *global-keymap* "C-c s" 'stack-window-layout)

#+(or)
(define-command project-window-layout () ()
  (lem/filer::filer))
#+(or)
(define-key *global-keymap* "C-c s" 'stack-window-layout)


;;; =============================================================================
;;; Basics Commands
;;; =============================================================================

(define-command open-init-file () ()
  (lem:find-file
   (merge-pathnames "init.lisp" (lem-home))))


;;; =============================================================================
;;; Time Stamps
;;; =============================================================================

(defvar *time-stamp-format*
  ;; Equals Emacs org-mode's default format.
  '("<" :year "-" (:month 2) "-" (:day 2) " " :short-weekday ">")
  "Time-stamp format.
  By default, prints year, month, day, and short english day: \"<2023-07-05 Wed>\"")

(defun format-time-stamp (&key (day (local-time:now)) (stream nil))
  (local-time:format-timestring stream day :format *time-stamp-format*))

(define-command time-stamp () ()
  "Print a timestamp of today, in the form <2042-12-01 Mon>."
  (insert-string (current-point) (format-time-stamp :stream t)))

