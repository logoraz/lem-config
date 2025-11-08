(require :asdf)
(require :uiop)
(require :sb-posix)

(eval-when (:load-toplevel :execute))

(defun setup ()
  "Thunk to setup Lem config."

  (uiop::ensure-directories-exist (uiop:xdg-config-home #P"lem/"))
  
  (sb-posix:symlink #P"~/Work/lem-config/init.lisp"
                    (uiop:xdg-config-home #P"lem/init.lisp"))

  (sb-posix:symlink #P"~/Work/lem-config/lem-config.asd"
                    (uiop:xdg-config-home #P"lem/lem-config.asd"))

  (sb-posix:symlink #P"~/Work/lem-config/version.sexp"
                    (uiop:xdg-config-home #P"lem/version.sexp"))
  
  (sb-posix:symlink #P"~/Work/lem-config/source"
                    (uiop:xdg-config-home #P"lem/source"))

  (sb-posix:symlink #P"~/Work/lem-config/extensions"
                    (uiop:xdg-config-home #P"lem/extensions"))

  #+(or)
  (sb-posix:symlink (uiop:xdg-config-home #P"lem/lem.desktop")
                    (uiop:xdg-data-home #P"applications/lem.desktop"))
  
  t)

(setup)
(sb-ext:quit)
