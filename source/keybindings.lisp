(defpackage :lem-config/keybindings
  (:use :cl :lem)
  (:documentation "General place for altered default keybindings."))
(in-package :lem-config/keybindings)


;; Make undo & redo what I am used to
(defun custom-keybindings ()
  "Defining in a function to re-deploy after starting lem/legit after init."
  (define-key *global-keymap* "C-/" 'undo)
  (define-key *global-keymap* "C-_" 'redo)

  ;; Hack Alt "M-" key doesn't seem to work for lem on Fedora 42...
  ;; see https://github.com/lem-project/lem/pull/1811
  ;; Added fix to lem/frontends/sdl2/keyboard.lisp
  (define-key *global-keymap* "C-;" 'execute-command) ;; Alternative keybinding for `M-x'
  
  (define-key *global-keymap* "C-h B" 'describe-bindings)
  (define-key *global-keymap* "C-h k" 'describe-key)
  (define-key *global-keymap* "C-h a" 'apropos-command)
  (define-key *global-keymap* "C-h p" 'lem-lisp-mode:lisp-apropos-package)
  (define-key *global-keymap* "C-x F" 'lem-core/commands/file:find-file-recursively))

(custom-keybindings) ; Enable custom keybindings on initialization.
