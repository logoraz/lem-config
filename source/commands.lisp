(defpackage :lem-config/source/commands
  (:use :cl :lem))
(in-package :lem-config/source/commands)


(define-command open-init-file () ()
  (lem:find-file
   (merge-pathnames "init.lisp" (lem-home))))

