(require 'asdf)
(require 'uiop)
(require 'sb-posix)

(eval-when (:load-toplevel :execute))

(defun setup ()
  "Thunk to setup Lem config."
  ;; Symlink lem.desktop to ~/.local/share/applications/lem.desktop
  (sb-posix:symlink (uiop:xdg-config-home #P"lem/lem.desktop")
                    (uiop:xdg-data-home #P"applications/lem.desktop"))
  t)

(setup)
(sb-ext:quit)
