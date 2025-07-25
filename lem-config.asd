(defsystem "lem-config"
  :description "Lem Configuration."
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "MIT"
  :version (:read-file-form "version.sexp" :at (0 1))
  :depends-on ("lem")
  :components
  ((:module "source"
    :serial t
    :components ((:file "appearance")
                 (:file "utilities")
                 (:file "keybindings")
                 (:file "commands")
                 #+(or)
                 (:file "completions")
                 (:file "file-prompt")
                 (:file "paredit")
                 (:file "lisp-ide"     :depends-on ("utilities"))
                 (:file "time-stamp")
                 (:file "playground")
                 (:file "issues"))))
  :long-description "
Personal Lem configuration, scaffolded as it's own system.
Includes configurations, 'fixes' as well as code 
base for my own purposes - perhaps to push upstream if and when I can.
")

