;;; Ref: https://github.com/garlic0x1/.lem/
;;; Ref: https://github.com/fukamachi/.lem
(defpackage :lem-config-init
  (:use :cl :lem))
(in-package :lem-config-init)

;; Add lem-config to ASDF registery and load lem-config system
(progn
  (asdf:initialize-source-registry
   (list :source-registry
         (list :tree (uiop:xdg-config-home "lem/"))
         (list :tree (asdf:system-source-directory :lem))
         :inherit-configuration))
  (sb-ext:without-package-locks
    (asdf:load-system :lem-config)))
