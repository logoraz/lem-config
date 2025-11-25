(defpackage #:lem-config-init
  (:use #:cl #:lem)
  (:import-from #:uiop
                #:xdg-config-home)
  (:import-from #:uiop/filesystem
                #:ensure-directories-exist)
  (:local-nicknames (#:lt #:local-time))
  (:documentation "lem-config System Initialization."))

(in-package #:lem-config-init)


;; ==============================================================================
;; ASDF Registry
;; ==============================================================================
(asdf:initialize-source-registry
 (list :source-registry
       (list :tree (xdg-config-home "lem/"))
       :inherit-configuration))

;; ==============================================================================
;; Loging Facilities
;; ==============================================================================
(defun current-time ()
  "Emits formatted time using local-time, with error handling."
  (handler-case
      (lt:format-timestring nil (lt:now)
                            :format '(:year "-" :month "-" :day "-T"
                                      :hour ":" :min ":" :sec))
    (error (condition)
      (format nil "Error getting current time: ~A" condition))))

(defun save-log-file (pathspec output)
  "Save log files for initializing lem-config, with improved error handling."
  (handler-case
      (let ((path (xdg-config-home pathspec)))
        (ensure-directories-exist path)  ; Ensures/creates parents, follows symlinks safely
        (with-open-file (strm path
                              :direction :output
                              :if-exists :append
                              :if-does-not-exist :create
                              :external-format :utf-8)  ; Explicit encoding for portability
          (format strm "~A - Load lem-config output: ~A~%" (current-time) output))
        t)  ; Return success
    (file-error (condition)
      (format t "File error while saving log ~A: ~A~%" pathspec condition)
      nil)
    (error (condition)
      (format t "Unexpected error while saving log ~A: ~A~%" pathspec condition)
      nil)))

;; ==============================================================================
;; Load Lem Configuration System :lem-config
;; ==============================================================================
(handler-case
    (progn
      (sb-ext:without-package-locks
        (asdf:load-system :lem-config))
      (save-log-file "lem/logs/config-startup.log" "Success"))
  (error (condition)
    (save-log-file "lem/logs/config-error.log" condition)))

