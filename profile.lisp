;;;; profile.lisp
;;;; terrible profiling

(in-package #:mother)

(defvar *profiling*)

(defun start-profiling ()
  (setf *profiling* (cons (make-hash-table :test 'eq) (make-hash-table :test 'eq)))
  (values))

(defun stop-profiling ()
  (makunbound '*profiling*)
  (values))

(defmethod combine :around ((operator operative) operands environment)
  (if (boundp '*profiling*)
      (let ((time (get-internal-real-time)))
	(prog1 (call-next-method)
	  (let ((diff (- (get-internal-real-time) time)))
	    (if (gethash operator (car *profiling*))
		(incf (gethash operator (car *profiling*)))
		(setf (gethash operator (car *profiling*)) 1))
	    (if (gethash operator (cdr *profiling*))
		(incf (gethash operator (cdr *profiling*)) diff)
		(setf (gethash operator (cdr *profiling*)) diff)))))
      (call-next-method)))
