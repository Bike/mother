;;;; read.lisp

(in-package #:mother)

;;;; i warn you, this is ugly (and nonconforming)

(defun illegal-char-reader (stream char)
  (error "Error while reading from ~s: ~c is not a valid character" stream char))

(defun illegal-dispatch-reader (stream subchar arg)
  (declare (ignore arg))
  (error "Error while reading from ~s: ~c is not a valid dispatch character" stream subchar))

(defun make-kernel-readtable ()
  (macrolet ((defdispatch (char &body body)
	       `(set-dispatch-macro-character
		 #\# ,char
		 (lambda (s c n)
		   (declare (ignore c))
		   (when n (error "Error while reading from ~s: Illegal use of CL dispatching arg ~d" s n))
		   ,@body)
		 res)))
    ;; symbols are pretty much the same.  numbers are less the same but close.
    ;; standard macro characters are ()';"`, and #
    ;; () can just use CL's (so long as kernel conses are CL conses, anyway)
    ;; ' is explicitly illegal, as are ` and ,
    ;; " is probably the same but kernel doesn't say much about strings
    ;; ; is the same
    ;; so... use CL:READ, warped a bit (a lot)
    (let ((res (copy-readtable nil))) ; standard readtable
      ;; define illegal chars
      (map nil
	   (rcurry #'set-macro-character #'illegal-char-reader nil res)
	   "'`,")
      ;; undefine all dispatch functions including ones we'll be using, except #\ since it's mostly like CL's
      (map nil
	   (lambda (char) (set-dispatch-macro-character #\# char #'illegal-dispatch-reader res))
	   "#'(*:=|+-.abcoprsx")
      ;; truth
      (defdispatch #\t (kbool t))
      ;; falsity
      (defdispatch #\f (kbool nil))
      ;; oh my god no
      (defdispatch #\e (read s t nil t) nil) ; nooooo
      ;; ignore and inert - even messier!
      (defdispatch #\i
	  (labels ((die ()
		     (error "Error while reading from ~s: bad #i" s))
		   (try (string)
		     "Tries to read the rest of the given string exactly; if it's not there, unread as possible and flame."
		     (map nil (lambda (c)
				(let ((char (read-char s nil nil t)))
				  (unless (and char (char= char c))
				    (when char (unread-char char s))
				    (die))))
			  string)))
	    (let ((char (read-char s nil nil t)))
	      (case char
		((#\g) (try "nore") *ignore*)
		((#\n) (try "ert") *inert*)
		(otherwise (when char (unread-char char s)) (die))))))
      ;; TODO: numbers not implemented yet.
      res)))

(defun kernel-read (&optional (stream *standard-input*) errorp eof recursivep)
  (let ((*readtable* (make-kernel-readtable)))
    (read stream errorp eof recursivep)))

(defun kernel-read-from-string (string)
  (with-input-from-string (*standard-input* string) (kernel-read)))
