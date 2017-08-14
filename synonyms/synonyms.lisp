(defpackage :synonyms
  (:use :cl)
  (:export
   #:set-synonyms))
(in-package :synonyms)

(defmacro set-synonyms (fn-or-m ns &optional (export? nil))
  "Sets each symbol of ns as a synonym for fn-or-m."
  (let ((safe-ns (gensym)))
    `(let ((mode (macro-function ,fn-or-m))
	   (,safe-ns (if (listp ,ns)
			 ,ns
			 (list ,ns))))
       (if mode
	   (set-macro-synonyms    ,fn-or-m ,safe-ns)
	   (set-function-synonyms ,fn-or-m ,safe-ns)))))

(defmacro set-function-synonyms (fn ns &optional (export? nil))
  `(loop for n in ,ns
      do (setf (symbol-function n)
	       (symbol-function ,fn))
	(when ,export?
	  (export n))
      finally (return t)))

(defmacro set-macro-synonyms (m ns &optional (export? nil))
  `(loop for n in ,ns
      do (setf (macro-function n)
	       (macro-function ,m))
        (when ,export?
	  (export n))
        finally (return t)))

;; Presets.
(progn
  (set-synonyms 'set-synonyms 'ss                                    t)
  (ss 'car                    'i-am-become-death-destroyer-of-worlds t)
  (ss 'multiple-value-bind    '(mvb m-v-b mb m-b mbind m-bind)       t)
  (ss 'destructuring-bind     '(db d-b dbind d-bind)                 t)
(ss 'with-open-file '(wof w-o-f with-file wf w-f) t))
