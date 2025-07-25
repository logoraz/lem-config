(defpackage :lem-config-init
  (:use :cl :lem)
  (:local-nicknames (:lt :local-time))
  (:documentation "lem-config System Initialization."))
(in-package :lem-config-init)


(asdf:initialize-source-registry
 (list :source-registry
       (list :tree (uiop:xdg-config-home "lem/"))
       :inherit-configuration))

(defun current-time ()
  "Emits formated time using local-time"
  (lt:format-timestring nil (lt:now)
                        :format '(:year "-" :month "-" :day "-T"
                                  :hour ":" :min ":" :sec)))

(defun save-log-file (pathspec output)
  "Save log files for initializing lem-config"
  (with-open-file (strm (uiop:xdg-config-home pathspec)
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create)
    (format strm "~A - Load lem-config output: ~A~%" (current-time) output)))

(multiple-value-bind (result error-condition)
    (ignore-errors
      (sb-ext:without-package-locks
        (asdf:load-system :lem-config)))
  (if error-condition
      (save-log-file "lem/logs/config-error.log" error-condition)
    (save-log-file "lem/logs/config-startup.log" result)))

