(defpackage :lem-config/issues
  (:use :cl))
(in-package :lem-config/issues)

;;; Fixes directly applied to Lem's source for now (sdl2-compat) issues!
;;; TODO#1: `M-x' key not working
;;  Soln -> See below, from Lem discord server...
;;
;;  src  -> lem/frontends/sdl2/keyboard.lisp

#+nil
(defun modifier-is-accept-text-input-p (modifier)
  (not (modifier-ctrl modifier)))

#+(or)
(defun modifier-is-accept-text-input-p (modifier)
  (and (not (modifier-ctrl modifier))
       (not (modifier-meta modifier))))

;;; TODO#2: Package Inferred System New File Creation assumes
;;          Keyword `defpackage` style configuration
;;  Soln -> Solved - needs review or optimization...
;;          see lem-config/source/ext/package-inferred-system.lisp
;;  src  -> lem/extensions/lisp-mode/ext/package-inferred-system.lisp

;; (setf lem-lisp-mode/package-inferred-system:*uninterned-defpackage-p* t)

;;; TODO#3: Issue Report
;;-> window defaults to initial sizing when leaving desktop or when Lem
;;   is no longer active. Figure out how to keep window size parameters
;;   persisten, so as to not have to resize every time leaving Lem.
