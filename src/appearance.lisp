(defpackage #:lem-config/appearance
  (:use #:cl #:lem)
  (:documentation "Appearance Configuration"))

(in-package #:lem-config/appearance)


;;; =============================================================================
;;; Frame Parameters/Transparency
;;; =============================================================================
;; TBD
;; Can't enable transparency or frame/window modifications as webview runs
;; as a separate process and communicates via json-rpc...

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
(define-command lisp-scratch-2 () ()
  "Define lisp-scratch buffer that enables paredit mode straight away!"
  (let ((buffer (primordial-buffer)))
    (change-buffer-mode buffer 'lem-lisp-mode:lisp-mode)
    (change-buffer-mode buffer 'lem-paredit-mode:paredit-mode t)
    (switch-to-buffer buffer)))

(ignore-errors
  #+(or)
  (setf lem-dashboard:*dashboard-enable* nil)
  (lem-dashboard:set-default-dashboard :project-count 3
                                       :file-count 7
                                       :hide-links t)

  (define-key lem-dashboard:*dashboard-mode-keymap* "l" 'lisp-scratch-2))

;;; =============================================================================
;;; Tabs
;;; =============================================================================
;; Tab bar does not update after killing buffers
;; see See https://github.com/lem-project/lem/issues/1993
;; |--> lem/frontends/server/tabbar.lisp (:lem/tabbar)
;; |--> lem/src/commands/file.lisp (:lem-core/commands/file)
;; |--> lem/src/buffer/buffer-ext.lisp (:lem-core)
;; Hack fix
(add-hook *post-command-hook* 'lem/tabbar::update)

