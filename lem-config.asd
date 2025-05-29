(defsystem "lem-config"
  :description "Modular Lem Configuration."
  :author "Erik P Almaraz <erikalmaraz@fastmail.com>"
  :license "MIT"
  :version (:read-file-form "version.sexp" :at (0 1))
  :depends-on ("lem")
  :components
  ((:module "source"
    :serial t
    :components ((:file "appearance")
                 (:file "utilities")
                 (:file "commands")
                 (:file "keybindings")
                 (:file "completions")
                 (:file "lisp-ide"     :depends-on ("utilities"))
                 (:file "playground"))))
  :long-description "
Modular Lem configuration scaffolded as its own system.
")

