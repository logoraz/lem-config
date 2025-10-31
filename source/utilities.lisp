(defpackage :lem-config/utilities
  (:use :cl :lem)
  (:export #:*time-stamp-format*
           #:time-stamp
           #:executable-find)
  (:documentation "Basic utilities for lem-config"))
(in-package :lem-config/utilities)


(defun executable-find (program)
  "Simple function to return path to PROGRAM"
  (remove #\newline 
          (uiop:run-program (list "which" program)
                            :output :string)))

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
