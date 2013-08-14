;;;; eval.lisp
;;;; interface to, and definition of (since it's so damn simple) the evaluator.
;;;; also includes the interfaces to COMBINE and RUN because why not.
;;;; req. LOOKUP in env.lisp

(in-package #:mother)

;; FIXME move?
(defclass combiner () ()) ; abstract
(defclass operative (combiner) ()) ; abstract

;;; these docstrings are really unhelpful, but these things are pretty abstract...

(defgeneric combine (operator operands environment)
  (:documentation "Combine OPERATOR with OPERANDS in ENVIRONMENT."))

(defun eval (expr env)
  "Main entry to the evaluator, handling tail contexts"
  ;; list of tail contexts specified:
  ;; (<applicative> ...) tail calls ((unwrap <applicative>) ...); applicative.lisp
  ;; $if tail calls its decision (but needs the condition not-tail-called, obviously); primdef.lisp
  ;; invocations of compound operatives tail call the stored body; dumb-vau.lisp
  ;; eval; primdef.lisp
  ;; rest are in (standard-impl-y anyway) derived operatives
  ;; $sequence tail calls the last; primdef.lisp and eval-seq below
  ;; apply, $let et al., $and?, $or?, $remote-eval
  ;; call/cc, $let/cc
  (loop (setf (values expr env)
	      (catch 'eval-tail (return (%eval expr env))))))

(declaim (inline eval-tail))
(defun eval-tail (expr env)
  "eval EXPR in ENV as a tail context."
  (throw 'eval-tail (values expr env)))

(declaim (inline combine-tail))
(defun combine-tail (combiner combinand env)
  ;; conses D:
  (eval-tail (cons combiner combinand) env))

(defun eval-seq (exprs env)
  "Evaluate a sequence of expressions, tail-calling the last.
If EXPRS is empty, returns #inert"
  (if (null exprs)
      *inert*
      (do ((list exprs (cdr list)))
	  ((endp (cdr list))
	   (eval-tail (car list) env))
	(eval (car list) env))))

(defgeneric %eval (expression environment)
  (:documentation "Evaluate EXPRESSION in ENVIRONMENT.
EVAL is the external interface; %EVAL doesn't deal with tail contexts"))

(defmethod %eval ((expr symbol) env)
  (multiple-value-bind (val b?) (lookup env expr) (if b? val (error "unbound: ~s" expr))))
(defmethod %eval ((expr null) env) ;fsgsd
  (declare (ignore env))
  expr)
(defmethod %eval ((expr cons) env)
  (combine (eval (first expr) env) (rest expr) env))
(defmethod %eval (expr env)
  (declare (ignore env))
  expr)
