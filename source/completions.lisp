(defpackage :lem-config/completions
  (:use :cl :lem)
  (:export #:fp-up-directory
           #:fp-find-file))
(in-package :lem-config/completions)


;;; =============================================================================
;;; File Prompt Customization
;;; =============================================================================

#+(or)
(progn
  (define-command fp-up-directory () ()
    "Delete the last path segment in file prompt."
    (alexandria:when-let*
        ((pwindow (lem/prompt-window::current-prompt-window))
         (wstring (and pwindow (lem/prompt-window::get-input-string))))
      (lem/prompt-window::replace-prompt-input
       (ignore-errors
         (let* ((trimmed (str:trim-right wstring :char-bag '(#\/ )))
                (endp (1+ (position #\/ trimmed :from-end t :test #'char-equal))))
           (subseq trimmed 0 endp))))
      (lem/completion-mode::completion-end)
      (ignore-errors (lem/prompt-window::prompt-completion))))

  (define-command fp-find-file () ()
    "find-file with backspace bound to up-directory."
    (let ((keys (make-keymap)))
      (define-key keys "Backspace" 'fp-up-directory)
      (with-special-keymap (keys)
        (call-command 'find-file (universal-argument-of-this-command)))))

  (define-key *global-keymap* "C-x C-f" 'fp-find-file))

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

