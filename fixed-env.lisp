;;;; fixed-env.lisp

(in-package #:mother)

(defclass fixed-env (environment)
  ((symbols :reader fixed-env-symbols :initarg :symbols
	    :type (array symbol))
   (values :reader fixed-env-values :initarg :values
	   :type (array t))))

(defun bindings->fixed-env (bindings)
  (make-instance 'fixed-env
		 :symbols (make-array (length bindings) :element-type 'symbol
				      :initial-contents (mapcar #'car bindings))
		 :values (make-array (length bindings) :element-type 't
				     :initial-contents (mapcar #'cdr bindings))))

(defmethod lookup ((env fixed-env) key)
  (let ((pos (position key (fixed-env-keys env))))
    (if pos
	(values (aref (fixed-env-values env) pos) t)
	(values nil nil))))
