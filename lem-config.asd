(defsystem #:lem-config
  :author "Erik P Almaraz"
  :license "MIT"
  :version "0.0.1"
  :description "Lem Configuration."
  ;; :defsystem-depends-on (:asdf-package-system)
  :class :package-inferred-system
  ;; :depends-on (":lem-config/extensions/tbd/all")
  :depends-on ()
  :serial t
  :pathname "source"
  :components ((:file "appearance")
               (:file "paredit")
               (:file "completions")
               (:file "keybindings")
               (:file "file-prompt")
               (:file "time-stamp")
               (:file "utilities")))

#+nil
(register-system-packages "lem-config/extensions/tbd/all" '(#:extension/tbd))
