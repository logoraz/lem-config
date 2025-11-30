(require :sb-posix)

(defpackage #:lem-setup
  (:use #:cl)
  (:local-nicknames (#:u #:uiop)
                    (#:posix #:sb-posix))
  (:export #:setup)
  (:documentation "Script to setup Lem locally."))

(in-package #:lem-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Create Symlinks (SBCL only)

(defun create-symlink (source target &key force)
  "Create a symlink from SOURCE to TARGET.
If FORCE is true, remove existing file/symlink at TARGET first.
Returns T if symlink was created, NIL if it already existed and FORCE was nil."
  (let ((source-path (u:native-namestring (u:ensure-pathname source)))
        (target-path (u:native-namestring (u:ensure-pathname target))))
    (when (probe-file target-path)
      (if force
          (progn
            (format t "Removing existing file at: ~A~%" target-path)
            (delete-file target-path))
          (progn
            (format t "Skipping (already exists): ~A~%" target-path)
            (return-from create-symlink nil))))
    (format t "Creating symlink: ~A -> ~A~%" target-path source-path)
    (posix:symlink source-path target-path)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lem Config Scaffolding

(defun setup (&key (force nil) (lem-binary-path #P"~/Work/builds/lem/lem"))
  "Setup Lem config by creating symlinks from the current directory.
If FORCE is true, replace existing symlinks/files.
LEM-BINARY-PATH specifies where the lem executable is located."
  
  (format t "~%Setting up Lem configuration...~%~%")
  
  ;; Get the directory where this script is located (current working directory)
  (let ((config-dir (u:getcwd)))
    (format t "Using config directory: ~A~%~%" config-dir)
    
    ;; Ensure target directories exist
    (ensure-directories-exist (u:xdg-config-home #P"lem/"))
    (ensure-directories-exist #P"~/.local/bin/")
    
    ;; Create symlinks for config files
    (create-symlink (merge-pathnames "init.lisp" config-dir)
                    (u:xdg-config-home #P"lem/init.lisp")
                    :force force)

    (create-symlink (merge-pathnames "lem-config.asd" config-dir)
                    (u:xdg-config-home #P"lem/lem-config.asd")
                    :force force)

    (create-symlink (merge-pathnames "version.sexp" config-dir)
                    (u:xdg-config-home #P"lem/version.sexp")
                    :force force)
    
    (create-symlink (merge-pathnames "src/" config-dir)
                    (u:xdg-config-home #P"lem/src")
                    :force force)

    (create-symlink (merge-pathnames "lib/" config-dir)
                    (u:xdg-config-home #P"lem/lib")
                    :force force)

    (create-symlink (merge-pathnames "assets/lem.desktop" config-dir)
                    (u:xdg-data-home #P"applications/lem.desktop")
                    :force force)

    ;; Create symlink for lem binary
    (create-symlink lem-binary-path
                    #P"~/.local/bin/lem"
                    :force force)
    
    (format t "~%Setup complete!~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Run Setup

;; To force overwrite existing symlinks, use: (setup :force t)
;; To specify a different lem binary location: 
;; (setup :lem-binary-path #P"/path/to/lem")
(setup)
(sb-ext:quit)
