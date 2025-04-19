(defpackage #:lem-config/source/file-prompt
  (:use #:cl
        #:lem))
(in-package #:lem-config/source/file-prompt)


(progn
  (define-key *global-keymap* "C-x C-f" 'fp-find-file)

  (define-command fp-find-file () ()
    "find-file with backspace bound to up-directory."
    (let ((keys (make-keymap)))
      (define-key keys "Backspace" 'fp-up-directory)
      (with-special-keymap (keys)
        (call-command 'find-file (universal-argument-of-this-command)))))

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
      (ignore-errors (lem/prompt-window::prompt-completion)))))

#+(or) ;; lem/extensions/lisp-mode/ext/package-inferred-system.lisp
(defun replace-buffer-text-to-defpackage (buffer package-name)
  (let ((package-name (ensure-keyword package-name)))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (with-open-stream (stream (make-buffer-output-stream (buffer-point buffer) nil))
        (let ((*print-case* :downcase))
          (dolist (form (list `(defpackage #,package-name
                                 (:use #:cl))
                              `(in-package #,package-name)))
            (prin1 form stream)
            (terpri stream)))))))
