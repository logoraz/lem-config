(defpackage :lem-config/appearance
  (:use :cl :lem)
  (:export #+lem-sdl2 :set-opacity
           #+lem-sdl2 :toggle-opacity))
(in-package :lem-config/appearance)


;; Look into adjust frame to a custom size
#+nil ;;Not currently working... with webview
(lem-core/commands/frame::maximize-frame)

#+nil
(ignore-errors
  "Enable Transparency."
  (defvar *opaquep* nil
    "Hold boolean state of opacity.")

  (defun set-opacity (&optional (opacity 0.9))
    "Set frame OPACITY (transparency)."
    (sdl2-ffi.functions:sdl-set-window-opacity (lem-sdl2/display:display-window
                                                (lem-sdl2/display:current-display))
                                               (coerce opacity 'single-float))
    #+(or)
    (uiop:run-program (format nil "transset-df -a ~A" opacity)))
  
  (define-command toggle-opacity () ()
    (set-opacity (if *opaquep* 1 0.9))
    (setf *opaquep* (not *opaquep*)))

  ;; Always start out with a transparent frame
  (set-opacity)
  (setf *opaquep* (not *opaquep*)))

;; Load Theme
;; (load-theme "decaf") ; default

;; Logs on the terminal output:
(log:config :info)

(ignore-errors
  "Configure Dashboard"
  ;; (setf lem-dashboard:*dashboard-enable* nil)
  (define-command lisp-scratch-2 () ()
    "Define lisp-scratch buffer that enables paredit mode straight away!"
    (let ((buffer (primordial-buffer)))
      (change-buffer-mode buffer 'lem-lisp-mode:lisp-mode)
      (change-buffer-mode buffer 'lem-paredit-mode:paredit-mode t)
      (switch-to-buffer buffer)))

  (lem-dashboard:set-default-dashboard :project-count 3
                                       :file-count 7
                                       :hide-links t)

  (define-key lem-dashboard:*dashboard-mode-keymap* "l" 'lisp-scratch-2))

;; Configure Fonts
(lem-core/commands/font::font-size-set 17)
