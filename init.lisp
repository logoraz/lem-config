;;; Ref: https://github.com/garlic0x1/.lem/
;;; Ref: https://github.com/fukamachi/.lem
(defpackage :lem-config-init
  (:use :cl :lem))
(in-package :lem-config-init)

;; Add lem-config to ASDF registery and load lem-config system
(let ((asdf:*central-registry*
        (append (list (asdf:system-source-directory :lem)
                      #P"~/.config/lem/")
                asdf:*central-registry*)))
  (asdf:load-system :lem-config))
