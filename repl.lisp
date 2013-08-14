;;;; repl.lisp

(in-package #:mother)

(defvar *repl-environment* (bindings->dumb-1parent-env nil *ground*))

(defun clear-repl-environment ()
  (setf *repl-environment* (bindings->dumb-1parent-env nil *ground*))
  (values))

(defun repl ()
  (with-simple-restart (quit "Return to CL.")
    (loop
       (with-simple-restart (abort "Return to abortion")
	 (fresh-line)
	 (princ "> ")
	 (let ((form (kernel-read)))
	   (fresh-line)
	   (write (eval form *repl-environment*)))))))
