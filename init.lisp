(defpackage :lem-config-init
  (:use :cl :lem))
(in-package :lem-config-init)


(asdf:initialize-source-registry
 (list :source-registry
       (list :tree (uiop:xdg-config-home "lem/"))
       :inherit-configuration))

(defun save-log-file (pathspec output)
  "Save log files for initializing lem-config"
  (with-open-file (strm (uiop:xdg-config-home pathspec)
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create)
    (format strm "Load lem-config output: ~A~%" output)))

(multiple-value-bind (result error-condition)
    (ignore-errors
      (sb-ext:without-package-locks
        (asdf:load-system :lem-config)))
  (if error-condition
      (save-log-file "lem/error.log" error-condition)
    (save-log-file "lem/startup.log" result)))

