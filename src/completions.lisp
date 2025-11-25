(defpackage #:lem-config/completions
  (:use #:cl #:lem)
  (:local-nicknames (#:alx #:alexandria))
  (:export #:fp-up-directory
           #:fp-find-file)
  (:documentation "Completions framework."))

(in-package #:lem-config/completions)

;;; =============================================================================
;;; Completions
;;; =============================================================================
;;; Choose the position of the completion prompt (new in May, 2024)
(setf lem-core::*default-prompt-gravity* :bottom-display)
(setf lem/prompt-window::*prompt-completion-window-gravity* :horizontally-above-window)
(setf lem/prompt-window::*fill-width* t)

;; Show the completion list directly, without a first press on TAB:
(add-hook *prompt-after-activate-hook*
          (lambda ()
            (call-command 'lem/prompt-window::prompt-completion nil)))

(add-hook *prompt-deactivate-hook*
          (lambda ()
            (lem/completion-mode:completion-end)))
