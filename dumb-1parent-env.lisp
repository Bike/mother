;;;; dumb-1parent-env.lisp
;;;; req. flat-env

(in-package #:mother)

(defclass dumb-1parent-env (environment)
  ((assoc :accessor dumb-assoc :initarg :assoc)
   (parent :accessor dumb-parent :initarg :parent)))

(defun bindings->dumb-1parent-env (bindings parent)
  (make-instance 'dumb-1parent-env
		 :assoc bindings
		 :parent parent))

(defmethod lookup ((env dumb-1parent-env) key)
  (multiple-value-bind (val bound?)
      (assoc-value (dumb-assoc env) key :test #'eq)
    (if bound?
	(values val bound?)
	(lookup (dumb-parent env) key))))

(defmethod flat-lookup ((env dumb-1parent-env) key)
  (assoc-value (dumb-assoc env) key :test #'eq))

(defmethod (setf flat-lookup) (val (env dumb-1parent-env) key)
  (setf (assoc-value (dumb-assoc env) key :test #'eq) val))

#||
(defclass dumb-1parent-env (environment)
  ((flat :accessor dumb-flat :initarg :flat
	 :type flat-env)
   (parent :accessor dumb-parent :initarg :parent)))

(defun bindings->dumb-1parent-env (bindings parent)
  (make-instance 'dumb-1parent-env
		 :flat (bindings->flat-env bindings)
		 :parent parent))

(defmethod lookup ((env dumb-1parent-env) key)
  (multiple-value-bind (val bound?)
      (flat-lookup (dumb-flat env) key)
    (if bound?
	(values val bound?)
	(lookup (dumb-parent env) key))))
(defmethod flat-lookup ((env dumb-1parent-env) key)
  (flat-lookup (dumb-flat env) key))
(defmethod (setf flat-lookup) (val (env dumb-1parent-env) key)
  (setf (flat-lookup (dumb-flat env) key) val))
||#
