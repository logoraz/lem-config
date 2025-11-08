(defpackage :lem-config/appearance
  (:use :cl :lem))
(in-package :lem-config/appearance)


;;; =============================================================================
;;; Frame Parameters/Transparency
;;; =============================================================================
#+nil
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
                                               (coerce opacity 'single-float)))
  
  (define-command toggle-opacity () ()
    (set-opacity (if *opaquep* 1 0.9))
    (setf *opaquep* (not *opaquep*)))

  ;; Always start out with a transparent frame
  (set-opacity)
  (setf *opaquep* (not *opaquep*)))

;;; =============================================================================
;;; Fonts
;;; =============================================================================
;; See lem/src/interface.lisp (or lem/src/commands/font.lisp)
(lem-core::set-font :name "Fira Code" :size 13)

;;; =============================================================================
;;; Theme
;;; =============================================================================
;; See lem/src/ext/themes.lisp
#+(or)
(load-theme "lem-default") ; "decaf"

;; Set custom Cursor color (variant base0d)
;; https://iamroot.tech/color-picker/default.aspx?color=88a2b7
;; See lem/src/cursors.lisp, lem/src/attribute.lisp
;; See lem/src/line-numbers.lisp, lem/src/ext/themes.lisp, 
;; lem/src/highlight-line.lisp

(defvar *lc/default-cursor-color* "#88a2b7")

(define-attribute lem-core::cursor
  (:light :background "black") ;; TODO |--> Set color for light cursor (default for now)
  (:dark :background *lc/default-cursor-color*))

(define-attribute lem/line-numbers:line-numbers-attribute
  (t :foreground :base02 :background :base00))

(define-attribute lem/line-numbers:active-line-number-attribute
  (t :foreground :base0d :background (lem-core::highlight-line-color)))

;;; =============================================================================
;;; Dashboard
;;; =============================================================================

(ignore-errors
  #+(or)
  (setf lem-dashboard:*dashboard-enable* nil)
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

;;; =============================================================================
;;; Tabs
;;; =============================================================================
;;; Fix for tab-bar:
;; Tab bar does not update after killing buffers
;; See lem/frontends/server/tabbar.lisp (:lem/tabbar)
;; See lem/src/commands/file.lisp (:lem-core/commands/file)
;; See lem/src/buffer/buffer-ext.lisp (:lem-core)
(add-hook *post-command-hook* 'lem/tabbar::update)

