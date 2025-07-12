(defpackage :lem-config/commands
  (:use :cl :lem))
(in-package :lem-config/commands)


(define-command open-init-file () ()
  (lem:find-file
   (merge-pathnames "init.lisp" (lem-home))))

