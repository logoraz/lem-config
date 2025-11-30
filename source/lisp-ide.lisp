(defpackage #:lem-config/lisp-ide
  (:use #:cl #:lem)
  (:import-from #:lem-lisp-mode
                #:lisp-mode
                #:*lisp-mode-keymap*)
  (:import-from #:lem-scheme-mode
                #:scheme-mode)
  (:import-from #:lem-paredit-mode
                #:paredit-mode
                #:paredit-slurp
                #:paredit-barf
                #:paredit-insert-doublequote
                #:*paredit-mode-keymap*)
  (:import-from #:lem-config/utilities
                #:executable-find)
  (:local-nicknames (#:ppcre #:cl-ppcre))
  (:export #:line-numbers-attribute
           #:active-line-number-attribute
           #:restore-save-buffer)
  (:documentation "Lisp IDE"))

(in-package #:lem-config/lisp-ide)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Editing 
;; Globally Enable Line Numbers:
(lem/line-numbers::line-numbers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Paredit
;; Enable paredit-mode in lisp-mode
(add-hook *find-file-hook*
          (lambda (buffer)
            (when (eq (buffer-major-mode buffer) 'lisp-mode)
              (change-buffer-mode buffer 'paredit-mode t))))

;; Enable paredit-mode in scheme-mode
(add-hook *find-file-hook*
          (lambda (buffer)
            (when (eq (buffer-major-mode buffer) 'scheme-mode)
              (change-buffer-mode buffer 'paredit-mode t))))


;; Paredit Mappings
(define-key *paredit-mode-keymap* "Shift-Right"
  'paredit-slurp)
(define-key *paredit-mode-keymap* "Shift-Left"
  'paredit-barf)

(define-command paredit-quote-wrap () ()
  (progn
    (paredit-insert-doublequote)
    (paredit-slurp)
    (delete-next-char)))

(define-key *paredit-mode-keymap* "M-\"" 'paredit-quote-wrap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lisp Interaction (aka SLIME)
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
        :collect (if (executable-find val) val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lisp Interaction (aka SLIME)
