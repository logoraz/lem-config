(defpackage :lem-config/lisp-ide
  (:use :cl :lem
        :lem-lisp-mode
        :lem-config/utilities))
(in-package :lem-config/lisp-ide)


(define-command slime-select () ()
  (slime t))

(define-key *lisp-mode-keymap* "C-c m s" 'slime-select)
(define-key *lisp-mode-keymap* "C-c l" 'lem-lisp-mode/eval::lisp-eval-clear)

(defvar *lisp-implementations* (list "sbcl" "ccl" "ecl" "clisp" "clasp")
  "List of currently installed implementations.")

(defun lem-lisp-mode/implementation::list-installed-implementations ()
  "Override internal function to specify my installed CL implementations"
  (loop :for val :in *lisp-implementations*
        :collect (if (exist-program-p val) val)))

#+or
(set-default-command "ccl")
  

#+nil
(defun run-lisp (&key command port directory)
  (labels ((output-callback (string)
             (let* ((buffer (make-lisp-process-buffer port))
                    (point (buffer-point buffer)))
               (buffer-end point)
               (insert-escape-sequence-string point string))))
    (let ((process
            (lem-process:run-process (uiop:split-string command)
                                     :directory directory
                                     :output-callback #'output-callback)))
      (lem-process:process-send-input process (format nil "(require :asdf)"))
      process)))

#+nil
(defun send-micros-create-server (process port)
  (let ((file (asdf:system-source-file (asdf:find-system :micros))))
    (lem-process:process-send-input
     process
     (format nil "(asdf:load-asd ~S)" file)))
  ;; Try to quickload micros, but fallback to asdf:load-system if ql not installed
  (lem-process:process-send-input
   process
   "(handler-case (eval (read-from-string \"(ql:quickload :micros)\"))
      (error (c) (asdf:load-system :micros)))")
  (lem-process:process-send-input
   process
   (format nil "(micros:create-server :port ~D :dont-close t)~%" port)))
