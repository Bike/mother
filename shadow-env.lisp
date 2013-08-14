;;;; shadow-env.lisp
;;;; child environment that shadows stuff

(in-package #:mother)

(defclass shadow-env (environment)
  ((shadowing :accessor shadow-shadowing :initarg :shadow
	      :type list #|of symbols|#)
   (parent :accessor shadow-parent :initarg :parents
	   :type environment)))

(defun shadowing->shadow-env (shadow parent)
  (make-instance 'shadow-env
		 :shadow shadow
		 :parents parent))

(defmethod flat-lookup ((env shadow-env) key)
  (declare (ignore key))
  (values nil nil))

(defmethod lookup ((env shadow-env) key)
  (if (member key (shadow-shadowing env))
      (values nil nil)
      (lookup (shadow-parent env) key)))
