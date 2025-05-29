(defpackage :lem-config/commands
  (:use :cl :lem)
  (:export #:open-init-file
           #:*time-stamp-format*
           #:time-stamp))
(in-package :lem-config/commands)


(define-command open-init-file () ()
  (lem:find-file
   (merge-pathnames "init.lisp" (lem-home))))

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

