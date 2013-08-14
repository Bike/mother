;;;; primdef.lisp
;;;; the ground

(in-package #:mother)

(defclass primitive-operator (operative)
  ((name :accessor primitive-operative-name :initarg :name)
   (typed-ptree :accessor primitive-operative-typed-ptree :initarg :ptree)
   (uses-env-p :accessor primitive-operative-uses-env-p :initarg :envp)
   (fun :accessor primitive-operative-fun :initarg :fun)))

(defmethod print-object ((object primitive-operator) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write (primitive-operative-name object) :stream stream)))

(defmethod combine ((operator primitive-operator) operands env)
  (funcall (primitive-operative-fun operator) operands env))

(defun every? (fun list)
  (mapcycle1 (lambda (elt) (unless (funcall fun elt) (return-from every? (kbool nil)))) list)
  (kbool t))

(defmacro destructure-typed-ptree (ptree operands &body body)
  (let ((type-alist nil))
    (labels ((aux (ptree path)
	       (etypecase ptree
		 (null)
		 ((eql _)) ; %ignore
		 (symbol (list `(,ptree ,path)))
		 (cons (nconc (aux (car ptree) `(car ,path)) (aux (cdr ptree) `(cdr ,path))))
		 (vector (push (elt ptree 0) (assoc-value type-alist (elt ptree 1))) (list `(,(elt ptree 0) ,path))))))
      (let ((bindings (aux ptree operands)))
	`(let (,@bindings)
	   (declare ,@(mapcar (lambda (type) `(type ,(car type) ,@(cdr type))) type-alist))
	   ,@body)))))

(defmacro primitive (name typed-ptree env &body body)
  (with-gensyms (operands envparam)
    `(make-instance 'primitive-operator
		    :name ',name
		    :envp ',env
		    :ptree ',typed-ptree
		    :fun (named-lambda ,name (,operands ,(or env envparam))
			   (declare (ignorable ,operands)) ; for nil ptree... not sure about it
			   ,@(unless env `((declare (ignore ,envparam))))
			   (destructure-typed-ptree ,typed-ptree ,operands
			     ,@body)))))

(defmacro primitive-typer (name cltype)
  `(wrap (primitive ,name #(x list) nil
	   (every? (of-type ',cltype) x))))

(defparameter *ground* (bindings->flat-env nil))

(defmacro defprim (name val)
  `($set! *ground* ',name ,val))

(defprim $cl (primitive $cl (form) nil (cl:eval form)))
(defprim quit (primitive exit () nil (invoke-restart 'quit))) ; not EXIT because that's kernel's oops

(defprim $if (primitive $if (condition then else) env
	       (typecase (eval condition env)
		 (truth (eval-tail then env))
		 (falsity (eval-tail else env))
		 (t (error "bad $if condition")))))

(defprim cons (wrap (primitive kons (o1 o2) nil (cons o1 o2)))) ; naming

(defprim lookup (wrap (primitive %lookup (#(name symbol) #(environment environment)) nil
			(lookup environment name))))
(defprim combine (wrap (primitive %combine (#(combiner combiner) combinand #(env environment)) nil
			 (combine-tail combiner combinand env))))
(defprim eval (wrap (primitive %eval (expression #(environment environment)) nil
		      (eval-tail expression environment))))

;; should actually be (list environment) but... recursive types... ;_;
(defprim make-environment (wrap (primitive make-environment #(parents list) nil
				  (bindings->generic-env nil (decycle parents)))))
;; what are you doing
(defprim shadowed-env (wrap (primitive shadowed-env (#(shadow list) #(parent environment)) nil
			      (shadowing->shadow-env shadow parent))))

(defprim $define! (primitive $define! (#(definiend ptree) expr) env
		    ($set! env definiend (eval expr env))
		    *inert*))

(defprim $vau (primitive $vau (#(ptree ptree) #(eformal (or symbol %ignore)) . exprs) env
		(make-dumb-vau env ptree eformal exprs)))

(defprim wrap (wrap (primitive rap (#(combiner combiner)) nil (wrap combiner)))) ; names
(defprim unwrap (wrap (primitive unrap (#(combiner applicative)) nil (unwrap combiner)))) ; NAMES

(defprim boolean? (primitive-typer boolean? motherly-boolean))
(defprim symbol? (primitive-typer symbol? (and symbol (not boolean)))) ; herp
(defprim inert? (primitive-typer inert? %inert))
(defprim pair? (primitive-typer pair? cons))
(defprim null? (primitive-typer null? null)) ; oh no, not partitioned!
(defprim environment? (primitive-typer environment? environment))
(defprim ignore? (primitive-typer ignore? %ignore))
(defprim operative? (primitive-typer operative? operative))
(defprim applicative? (primitive-typer applicative? applicative))

(defprim eq? (wrap (primitive eq? (a b) nil (kbool (eq a b)))))
(defprim equal? (wrap (primitive equal? (a b) nil (kbool (equal a b))))) ; FIXME wrong

;; not spec'd but i think quite reasonable, and could be used to define $binds?

;; FIXME: ksym type
(defprim bound? (wrap (primitive bound? (#(env environment) #(sym (and symbol (not boolean)))) nil
			(kbool (nth-value 1 (lookup env sym))))))

;; Core (I)

(defprim $sequence (primitive $sequence forms env (eval-seq forms env)))

;; Core (II)
#+(or)
(defprim $letrec (primitive $letrec (#(bindings list) . forms) env
		   (let* ((newenv (bindings->dumb-1parent-env nil env))
			  (ptrees (mapcar #'first bindings))
			  (values (mapcar (compose (rcurry #'eval newenv) #'second)
					  bindings)))
		     (mapc (curry #'$set! newenv) ptrees values)
		     (eval-seq forms newenv))))

;; math (shitty)

(defprim <=? (wrap (primitive <=? (#(a number) #(b number)) nil (kbool (<= a b)))))
(defprim >=? (wrap (primitive >=? (#(a number) #(b number)) nil (kbool (>= a b)))))
(defprim <? (wrap (primitive <? (#(a number) #(b number)) nil (kbool (< a b)))))
(defprim >? (wrap (primitive >? (#(a number) #(b number)) nil (kbool (> a b)))))

(defprim =? (wrap (primitive =? (#(a number) #(b number)) nil (kbool (= a b)))))

(defprim + (wrap (primitive k+ (#(a number) #(b number)) nil (+ a b))))
(defprim - (wrap (primitive k- (#(a number) #(b number)) nil (- a b))))
(defprim * (wrap (primitive k* (#(a number) #(b number)) nil (* a b))))

(defprim inexact? (primitive-typer inexact? nil)) ; >_>

;; characters (unspec'd)

(defprim char=? (wrap (primitive char=? (#(a character) #(b character)) nil (kbool (char= a b)))))

;; vectors (not even mentioned)

(defprim vector-length (wrap (primitive vector-length (#(vec vector)) nil (length vec))))
(defprim vector-ref (wrap (primitive vector-length (#(vec vector) #(index (integer 0))) nil (aref vec index))))
; set, alloc

;; encapsulations (bad placement)

(defstruct encapsulation type object)

(defprim make-encapsulation-type
    (wrap (primitive make-encapsulation-type () nil
	    (let ((newtype (gensym "ENCAPSULATION-TYPE")))
	      (list (wrap (primitive encapsulate (object) nil (make-encapsulation :type newtype :object object)))
		    ;; TODO: n-ary
		    (wrap (primitive cap-type (object) nil (kbool (and (encapsulation-p object)
								       (eq (encapsulation-type object) newtype)))))
		    ;; TODO: error checking
		    (wrap (primitive decapsulate (object) nil (encapsulation-object object))))))))

;; shit (shitty)

;; like, wow.
(defprim load! (wrap (primitive load! (filename) env 
		       (with-open-file (s filename)
			 (handler-case (loop (eval (kernel-read s t) env))
			   (end-of-file () *inert*))))))
