;;;; macro.lisp
;;;; req. COMBINE, EVAL

(in-package #:mother)

(defclass macro (operative)
  ((underlying :accessor macro-underlying :initarg :op)))

(defun macrotize (op) (make-instance 'macro :op op)) ; generic?

(defun expand-macro-combination (macro operands env) ; generic?
  (combine (macro-underlying macro) operands env))

(defmethod combine ((operator macro) operands env)
  (eval (expand-macro-combination operator operands env) env))

(defprim macrotize (wrap (primitive %macrotize (#(op combiner)) nil (macrotize op))))

#+(and (or) kernel)
($define! $let-redirect
  ($vau (exp bindings . body) env
    (eval (list* (eval (list* $lambda (map car bindings) body)
		       (eval exp
			     env))
		 (map cadr bindings))
	  env)))

#+(and (or) kernel)
($define! $let-redirect
  ($macro (exp bindings . body) env
    (list* (list eval (list* $lambda (map car bindings) body) exp) (map cadr bindings))))
