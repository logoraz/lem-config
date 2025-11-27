(defsystem "lem-config"
  :description "Modular Lem Configuration."
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "MIT"
  :version (:read-file-form "version.sexp" :at (0 1))
  :depends-on ("lem")
  :components
  ((:module "src"
    :serial nil ; Allow for parallel compilation where possible
    :components
    (;; Base utilities - no dependencies
     (:file "utilities")
     (:file "appearance")
     
     ;; Core Functionality - minimal dependencies
     (:file "completions")
     (:file "commands" :depends-on ("utilities"))

     ;; Dependent modules
     (:file "keybindings" :depends-on ("commands"))
     (:file "lisp-ide"  :depends-on ("commands"))

     ;; Optional/Experimental
     (:file "playground"))))
  
  :long-description "
Modular Lem configuration scaffolded as its own system.

Components:
  - appearance: Theme, colors, UI customization
  - utilities: Helper functions and common utilities  
  - commands: Custom Lem commands
  - keybindings: Key binding configuration
  - completions: Completion system configuration
  - lisp-ide: Common Lisp IDE enhancements
  - playground: Experimental features

This system can be loaded independently or as part of Lem's initialization.
")

