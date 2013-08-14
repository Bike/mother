;;;; core.lisp
;;;; miscellaneous core combiners

(in-package #:mother)

(defun $set! (in-env ptree val)
  ;; ($define! ptree form) is ($set! (get-current-environment) ptree form)
  ;; this primitive however does not take a form, it takes an already-evaluated thing!
  (labels ((assign (ptree val)
	     (etypecase ptree
	       (null (unless (null val) (error "could not match ~:a with ~a" nil args)))
	       (symbol (setf (flat-lookup in-env ptree) val))
	       (%ignore)
	       (cons
		(unless (consp val) (error "could not match ~a with ~a" ptree val))
		(assign (car ptree) (car val))
		(assign (cdr ptree) (cdr val))))))
    (assign ptree val)))
