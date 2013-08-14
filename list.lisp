;;;; list.lisp
;;;; list utilities, which should be beefed up to deal with cycles (ugh UGH)

(in-package #:mother)

(defun maplast (function list zero)
  ;; doesn't have to worry about cycles, because it's used for $sequence which doesn't care about them
  (if (null list)
      zero
      (do ((list list (cdr list)))
	  ((endp (cdr list)) (funcall function (car list)))
	(funcall function (car list)))))

(declaim (ftype (function (t) (values non-negative-fixnum (member 0 1) non-negative-fixnum non-negative-fixnum))
		get-list-metrics))
;; only works on lists with a fixnum number of pairs
;; or well, it'll fail (or do something weird) if fastcount overflows most-positive-fixnum
;; just take out the type assertions if that becomes a problem, i guess!
(defun get-list-metrics (list)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cond ((atom list) (values 0 (if (null list) 1 0) 0 0))
	((atom (cdr list)) (values 1 (if (null (cdr list)) 1 0) 1 0))
	(t
	 (do ((slow (cdr list))
	      (slowlog 1)
	      (fast (cddr list) (cdr fast))
	      (fastcount 2 (1+ fastcount)))
	     ((atom fast) (values fastcount (if (null fast) 1 0) fastcount 0))
	   (declare (type positive-fixnum slowlog fastcount))
	   (let ((newslow (1- (integer-length fastcount)))) ; ilog2
	     (declare (type non-negative-fixnum newslow))
	     (when (> newslow slowlog)
	       (setf slow (nthcdr (the positive-fixnum (ash 1 slowlog)) slow))
	       (incf slowlog)))
	   (when (eq slow fast)
	     (let ((cycle-length (1+ (the non-negative-fixnum (- fastcount (the positive-fixnum (ash 1 slowlog)))))))
	       (declare (type positive-fixnum cycle-length))
	       (do ((cons list (cdr cons))
		    (acycle-length 0 (1+ acycle-length))
		    (comp (nthcdr cycle-length list) (cdr comp)))
		   ((eq cons comp)
		    (return-from get-list-metrics
		      (values (the positive-fixnum (+ acycle-length cycle-length)) 0 acycle-length cycle-length)))
		 (declare (type non-negative-fixnum acycle-length)))))))))

(defun decycle (list)
  (multiple-value-bind (total null acyclic cyclic) (get-list-metrics list)
    (declare (ignore acyclic))
    (when (and (zerop cyclic) (zerop null)) (error "improper list"))
    ;; pointlessly checks for list ending
    (loop for n downfrom total above 0 for x in list collecting x)))

(defun mapcycle1 (function list)
  (multiple-value-bind (total null acyclic cyclic) (get-list-metrics list)
    (declare (ignore total))
    (when (and (zerop cyclic) (zerop null)) (error "improper list"))
    (let ((res (list nil)))
      (do ((cons list (cdr cons))
	   (ptr res (setf (cdr ptr) (list (funcall function (car cons)))))
	   (count 0 (1+ count)))
	  ((= count acyclic)
	   (when (plusp cyclic)
	     (setf (cdr ptr)
		   (do ((cons cons (cdr cons))
			(ptr2 ptr (setf (cdr ptr2) (list (funcall function (car cons)))))
			(count 0 (1+ count)))
		       ((= count cyclic) (setf (cdr ptr2) (cdr ptr))))))
	   (cdr res))))))
