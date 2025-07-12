(defpackage :lem-config/utilities
  (:use :cl :lem)
  (:export #:executable-find)
  (:documentation "Basic utilities for lem-config"))
(in-package :lem-config/utilities)


(defun executable-find (program)
  "Simple function to return path to PROGRAM"
  (remove #\newline 
          (uiop:run-program (list "which" program)
                            :output :string)))
