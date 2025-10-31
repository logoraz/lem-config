(defpackage :lem-config/lisp-ide
  (:use :cl :lem
        :lem-lisp-mode
        :lem-config/utilities)
  (:export ))
(in-package :lem-config/lisp-ide)


;; Enable paredit-mode in lisp-mode
(add-hook *find-file-hook*
          (lambda (buffer)
            (when (eq (buffer-major-mode buffer) 'lem-lisp-mode:lisp-mode)
              (change-buffer-mode buffer 'lem-paredit-mode:paredit-mode t))))

;; Enable paredit-mode in scheme-mode
(add-hook *find-file-hook*
          (lambda (buffer)
            (when (eq (buffer-major-mode buffer) 'lem-scheme-mode:scheme-mode)
              (change-buffer-mode buffer 'lem-paredit-mode:paredit-mode t))))


;; Paredit Mappings
(define-key lem-paredit-mode:*paredit-mode-keymap* "Shift-Right"
  'lem-paredit-mode:paredit-slurp)
(define-key lem-paredit-mode:*paredit-mode-keymap* "Shift-Left"
  'lem-paredit-mode:paredit-barf)

;; FIXME - Seems to be causing an error
(define-command paredit-quote-wrap () ()
  (progn
    (lem-paredit-mode:paredit-insert-doublequote)
    (lem-paredit-mode:paredit-slurp)
    (lem:delete-next-char)))

(define-key lem-paredit-mode:*paredit-mode-keymap* "M-\"" 'paredit-quote-wrap)


;; WIP See if I can get slime to work with other CL implementations?
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
