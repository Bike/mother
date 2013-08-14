;;;; env.lisp
;;;; interface to environments

(in-package #:mother)

(defclass environment () ()) ; abstract, for subtyping

;;; LOOKUP and FLAT-LOOKUP return two values, like GETHASH.
;;; FLAT- just means that any parents of the environments aren't relevant.
;;; (SETF LOOKUP) is presently mostly unimplemented because Kernel doesn't need it.

(defgeneric lookup (environment key))
(defgeneric (setf lookup) (value environment key)
  (:argument-precedence-order environment key value))
(defgeneric flat-lookup (environment key))
(defgeneric (setf flat-lookup) (value environment key)
  (:argument-precedence-order environment key value))
