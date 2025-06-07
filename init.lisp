;;; Ref: https://github.com/garlic0x1/.lem/
;;; Ref: https://github.com/fukamachi/.lem
(defpackage #:lem-config-init
  (:use #:cl 
        #:lem))
(in-package #:lem-config-init)

(defmacro without-package-locks (&body body)
  "Ignore package locks for the duration of the BODY.
Same as `progn' on implementations that don't have package locks.

Macro borrowed from Nyxt."
  #+sb-package-locks
  `(sb-ext:without-package-locks
     ,@body)
  #+(and ecl package-locks)
  `(ext:without-package-locks
     ,@body)
  #-(or sb-package-locks package-locks)
  `(progn ,@body))

;; Load init source files.
(let ((asdf:*central-registry*
        (append (list (asdf:system-source-directory 'lem)
                      #P"~/.config/lem/")
                asdf:*central-registry*)))
  (without-package-locks
    (asdf:load-system 'lem-config)))

