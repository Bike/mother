;;;; generic-env.lisp
;;;; full generality of kernel environments

(in-package #:mother)

(defclass generic-env (environment)
  ((flat :accessor generic-flat :initarg :flat
	 :type flat-env)
   (parents :accessor generic-parents :initarg :parents
	    :type list)))

(defun bindings->generic-env (bindings parents)
  (make-instance 'generic-env
		 :flat (bindings->flat-env bindings)
		 :parents parents))

(defmethod flat-lookup ((env generic-env) key)
  (flat-lookup (generic-flat env) key))
(defmethod (setf flat-lookup) (val (env generic-env) key)
  (setf (flat-lookup (generic-flat env) key) val))

(defmethod lookup ((env generic-env) key)
  (flet ((maybe (env)
	   (multiple-value-bind (val bound?) (lookup env key)
	     (when bound?
	       (return-from lookup (values val t))))))
    (maybe (generic-flat env))
    ;; depth-first (why did i think it was breadth-first?)
    (mapc #'maybe (generic-parents env))
    (values nil nil)))
