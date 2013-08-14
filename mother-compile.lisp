(in-package #:mother)

(defclass compiled-proc ()
  ())

(defvar *free*)

(defun linearize-form (form expecting)
  (typecase form
    (symbol (if expecting (list (gen-lookup form)) nil))
    ((cons symbol)
     (case (first form)
       ((lambda) (if expecting (linearize-lambda (second form) (cddr form)) nil))
       ((quote) (if expecting (list (gen-const (second form))) nil))
       ((begin) (linearize-begin (rest form) expecting))
       ((if) (linearize-if (second form) (third form) (fourth form) expecting))
       (otherwise (linearize-app (first form) (rest form) expecting))))
    (cons (linearize-app (first form) (rest form) expecting))
    (t (if expecting (list (gen-const (second form)))))))

(defun linearize-begin (forms expecting)
  ;; lol inefficient
  ;; (begin) doesn't work atm (or well, it does, it just returns an ugly value)
  (nconc (mapcan (rcurry #'linearize-form expecting) (butlast forms))
	 (linearize-form (first (last forms)) expecting)))

(defun of-list (list) (lambda (item) (find item list)))

(defun linearize-lambda (formals body)
  (let* ((thunk (make-thunk body))
	 ;; could be set-difference, but there should be other reasons to not close over variables
	 ;; e.g. if the function doesn't actually escape
	 (close (remove-if (of-list formals) (thunk-free thunk)))
	 (lambda (make-instance 'compiled-lambda :thunk thunk :formals formals)))
    (if close
	(linearize-closure lambda close)
	(list (lift lambda)))))

(defun linearize-closure (lambda closing)
  ;; generate code to allocate a closure
  (nconc (mapcar #'gen-lookup closing)
	 (list `(alloc-frame ,(length closing)))
	 (list (lift lambda))
	 (list '(alloc-closure))))

(defun make-thunk (body)
  (let ((*free* nil))
    ;; Uses left-to-right arg evaluation, since the linearization is accumulating the *free*.
    (make-instance 'compiled-thunk
		   :body (linearize-begin body t)
		   :free *free*)))

(defun linearize-app (fun args expecting)

(defun linearize-if (cond then alt expecting)
  (let ((else (gensym "ELSE"))
	(done (gensym "DONE")))
    (nconc (linearize-form cond t)
	   (list `(jump-if-false ,else))
	   (linearize-form then expecting)
	   (list `(jump ,done))
	   (list `(label ,else))
	   (linearize-form alt expecting)
	   (list `(label ,done)))))
