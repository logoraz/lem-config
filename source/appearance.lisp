(defpackage #:lem-config/source/appearance
  (:use #:cl 
        :lem)
  (:export #:set-opacity
           #:toggle-opacity))
(in-package #:lem-config/source/appearance)


;; Look into adjust frame to a custom size
;; lem-core/commands/frame::maximize-frame

(defparameter *regular-font*
  #P"/home/logoraz/.local/share/fonts/FiraCodeNerdFontMono-Regular.ttf")

(defparameter *bold-font*
  #P"/home/logoraz/.local/share/fonts/FiraCodeNerdFontMono-Bold.ttf")

(defvar *opaquep* nil
  "Hold boolean state of opacity.")

(defun set-opacity (&optional (opacity 0.9))
  "Set frame OPACITY (transparency)."
  (sdl2-ffi.functions:sdl-set-window-opacity (lem-sdl2/display:display-window 
                                              (lem-sdl2/display:current-display))
                                             (coerce opacity 'single-float)))

(define-command toggle-opacity () ()
  (set-opacity (if *opaquep* 1 0.9))
  (setf *opaquep* (not *opaquep*)))

;; Always start out with a transparent frame
(set-opacity)
(setf *opaquep* (not *opaquep*))

;; Load Theme
;; (load-theme "decaf") ; default

;; Logs on the terminal output:
(log:config :info)

;; Dashboard
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

(define-key lem-dashboard:*dashboard-mode-keymap* "l" 'lisp-scratch-2)

;; Use FiraCode Nerd fonts
;; FIX: figure out how to get icon sets display in C-x d
;;#+lem-sdl2
;; (ignore-errors
;;   (let ((font-regular *regular-font*)
;;         (font-bold *bold-font*))
;;     (if (and (uiop:file-exists-p font-regular)
;;              (uiop:file-exists-p font-bold))
;;         (lem-sdl2/display:change-font (lem-sdl2/display:current-display)
;;                                       (lem-sdl2/font:make-font-config
;;                                        :latin-normal-file font-regular
;;                                        :latin-bold-file font-bold
;;                                        :cjk-normal-file font-regular
;;                                        :cjk-bold-file font-bold))
;;         (message "Fonts not found."))))

