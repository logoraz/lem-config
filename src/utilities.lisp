(defpackage #:lem-config/utilities
  (:use #:cl #:lem)
  (:import-from #:lem-core
                #:lem-home)
  (:local-nicknames (#:u #:uiop))
  (:export #:executable-find
           #:cleanup-debug-logs)
  (:documentation "Basic utilities for lem-config"))

(in-package #:lem-config/utilities)

;;; =============================================================================
;;; General Utilities
;;; =============================================================================
(defun executable-find (program)
  "Find executable PROGRAM in PATH, return full path or NIL if not found (SBCL only)"
  (let* ((path-env (u:getenv "PATH"))
         (separator #+windows ";" #-windows ":")
         (paths (when path-env
                  (u:split-string path-env :separator separator))))
    (dolist (dir paths)
      (let* ((dir-path (u:ensure-directory-pathname dir))
             (candidate (merge-pathnames program dir-path)))
        (when (and (probe-file candidate)
                   (handler-case
                       (let ((stat (sb-posix:stat (namestring candidate))))
                         (plusp (logand (sb-posix:stat-mode stat) #o111)))
                     (error () nil)))
          (return-from executable-find (namestring candidate)))))))

;;; =============================================================================
;;; Other
;;; =============================================================================
(defun cleanup-debug-logs ()
  "Remove any existing debug.log files"
  (handler-case
      (dolist (file (u:directory-files (lem-home) "debug.log*"))
        (u:delete-file-if-exists file))
    (error () nil)))