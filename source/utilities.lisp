(defpackage :lem-config/source/utilities
  (:use :cl :lem)
  (:export #:executable-find)
  (:documentation "Basic utilities for lem-config"))
(in-package :lem-config/source/utilities)


(defun executable-find (program)
  "Simple function to return path to PROGRAM"
  (remove #\newline 
          (uiop:run-program (list "which" program)
                            :output :string)))
