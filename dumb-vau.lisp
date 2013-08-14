;;;; dumb-vau.lisp
;;;; simplistic implementation of interpreted operatives
;;;; req. dumb-1parent-env stuff in dumb-1parent-env.lisp

(in-package #:mother)

(defclass dumb-vau (operative)
  ((static-env :accessor vau-env :initarg :env) ; i.e. the parent of the constructed env
   (args-augmenter :accessor vau-augmenter :initarg :aug
		   :type ptree-matcher)
   (code :accessor vau-code :initarg :code)
   (envparam :accessor vau-envparam :initarg :envparam
	     :type (or symbol %ignore))))

(defun make-dumb-vau (env ptree envparam source)
  ;; typecheck for ptree is implicit in make-matcher
  (check-type envparam (or symbol %ignore) "a valid environment parameter")
  (make-instance 'dumb-vau
		 :env env
		 :aug (make-matcher ptree (list envparam))
		 ;; CIRCULAR LISTS: replace copy-tree
		 :code (copy-tree source)
		 :envparam envparam))

(defmethod combine ((operator dumb-vau) operands env)
  (let ((runenv (bindings->dumb-1parent-env
		 (let ((flat (funcall (vau-augmenter operator) operands))
		       (param (vau-envparam operator)))
		   (if (%ignore-p param)
		       flat
		       (acons param env flat)))
		 (vau-env operator))))
    (eval-seq (vau-code operator) runenv)))

(defun make-matcher (ptree seen)
  (declare (optimize (speed 3) (safety 0) (space 0))
	   (type list seen))
  (etypecase ptree
    (null (values (lambda (args) (if (null args) nil (error "could not match ~:a with ~a" nil args))) seen))
    (symbol
     (if (find ptree seen :test #'eq)
	 (error "duplicate parameter ~a in ptree" ptree)
	 (values (lambda (args) (list (cons ptree args))) (cons ptree seen))))
    (%ignore (values (lambda (args) (declare (ignore args)) nil) seen))
    (cons
     (multiple-value-bind (car seen) (make-matcher (car ptree) seen)
       (multiple-value-bind (cdr seen) (make-matcher (cdr ptree) seen)
	 (values (lambda (args)
		   (if (consp args)
		       (nconc (funcall car (car args)) (funcall cdr (cdr args)))
		       ;; FIXME: dumb error, but i'd like to avoid closing over ptree
		       ;; (both on principle and because it could be later modified by the user)
		       (error "could not match ~a" args)))
		 seen))))))
