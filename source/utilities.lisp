(defpackage #:lem-config/source/utilities
  (:use #:cl
        #:lem
        #+(or)
        #:lem-lisp-mode/implementation)
  (:export #:open-init-file))
(in-package #:lem-config/source/utilities)


(defvar *lisp-implementations* (list  "sbcl" "ecl" "ccl"))

;; TODO
;; Need to find a way to diversify 'slime' command so the user can provide
;; other Common Lisp implementations to start a REPL with...

#+(or)
(progn
  ;; lem/extensions/lisp-mode/implementation.lisp
  (lem-lisp-mode/implementation::list-installed-implementations)

  (defun list-installed-implementations ()
    (when (exist-program-p "sbcl")
      (list "sbcl" "ecl" "ccl")))
  
  ) ;; progn

;;; Commands
(define-command open-init-file () ()
  ;; @sasanidas
  (lem:find-file
   (merge-pathnames "init.lisp" (lem-home))))

