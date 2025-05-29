(defpackage #:lem-config/source/playground
  (:use #:cl
        #:lem)
  (:export ))
(in-package #:lem-config/source/playground)


;;; TODO: Determine how to implement these changes lem-config as opposed
;;;       to altering the source directly...

;;;lem/frontends/sdl2/keyboard.lisp
#+nil
(defun modifier-is-accept-text-input-p (modifier)
  (not (modifier-ctrl modifier)))

#+(or)
(defun modifier-is-accept-text-input-p (modifier)
  (and (not (modifier-ctrl modifier))
       (not (modifier-meta modifier))))


;;; lem/extensions/lisp-mode/ext/package-inferred-system.lisp
#+nil
(defun ensure-keyword (package-name)
  (etypecase package-name
    (string (make-keyword (string-upcase package-name)))
    (symbol (if (keywordp package-name)
                package-name
                (ensure-keyword (string package-name))))))

;; RAZ
#+(or)
(defun ensure-uninterned-symbol (package-name)
  (etypecase package-name
    ;; see alexandria --> symbols.lisp
    (string (format-symbol nil (string-upcase package-name)))
    (symbol (if (keywordp package-name)
                (format-symbol nil package-name)
                (ensure-uninterned-symbol (string package-name))))))

#+nil
(defun replace-buffer-text-to-defpackage (buffer package-name)
  (let ((package-name (ensure-keyword package-name)))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (with-open-stream (stream (make-buffer-output-stream (buffer-point buffer) nil))
        (let ((*print-case* :downcase))
          (dolist (form (list `(defpackage ,package-name
                                 (:use :cl))
                              `(in-package ,package-name)))
            (prin1 form stream)
            (terpri stream)))))))

#+(or)
(defun replace-buffer-text-to-defpackage (buffer package-name)
  (let ((package-name (ensure-uninterned-symbol package-name)))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (with-open-stream (stream (make-buffer-output-stream (buffer-point buffer) nil))
        (let ((*print-case* :downcase))
          (dolist (form (list `(defpackage ,package-name
                                 (:use #:cl))
                              `(in-package ,package-name)))
            (prin1 form stream)
            (terpri stream)))))))

#+(or)
(defmethod execute-find-file (executor (mode (eql 'lisp-mode)) pathname)
  (let ((buffer (call-next-method)))
    (when (empty-buffer-p buffer)
      (when-let (package-name (infer-package-name (buffer-filename buffer)))
        (replace-buffer-text-to-defpackage buffer package-name)))
    buffer))
