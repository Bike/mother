;;;; applicative.lisp
;;;; req. COMBINE in eval.lisp

(in-package #:mother)

(defclass applicative (combiner)
  ((underlying :accessor applicative-underlying :initarg :op)))

;; could make generic? eh
(defun wrap (op) (make-instance 'applicative :op op))
(defun unwrap (app) (declare (type applicative app)) (applicative-underlying app))

(defmethod combine ((operator applicative) operands env)
  (combine-tail (applicative-underlying operator)
		(mapcycle1 (rcurry #'eval env) operands)
		env)
  #+(or)
  (combine (applicative-underlying operator)
	   #+(or)
	   (mapcar (rcurry #'eval env) operands)
	   (mapcycle1 (rcurry #'eval env) operands)
	   env))
