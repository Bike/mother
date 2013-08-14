;;;; flat-env.lisp
;;;; "flat" environments, about at simple as possible
;;;; (except hash tables aren't actually that simple *shrug*)

(in-package #:mother)

(defclass flat-env (environment)
  ((hash :reader flat-env-hash :initarg :hash
	 :type hash-table)))

(defmethod print-object ((obj flat-env) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (write (hash-table-keys (flat-env-hash obj)) :stream stream :length 5)))

(defun bindings->flat-env (bindings)
  (make-instance 'flat-env :hash (alist-hash-table bindings :test #'eq)))

(defmethod lookup ((env flat-env) key)
  (gethash key (flat-env-hash env)))
(defmethod flat-lookup ((env flat-env) key)
  (gethash key (flat-env-hash env)))

(defmethod (setf lookup) (value (env flat-env) key)
  (setf (gethash key (flat-env-hash env)) value))
(defmethod (setf flat-lookup) (value (env flat-env) key)
  (setf (gethash key (flat-env-hash env)) value))
