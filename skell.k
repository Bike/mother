;;; FIXME (just, in general)

;;; combiners; core

($define! combiner ($class () ()))

($define! vau ($class (combiner) (params eparam body static)))

($define! make-vau (constructor vau))

($define! $vau
  ;; straight metacircle
  ($vau (params eparam body) static (vau params eparam body static)))

($define! applicative ($class (combiner) (underlying)))

($define! wrap (constructor applicative))
($define! unwrap ($generic (((applicative underlying)) underlying)))

;($define! macro ($class (combiner) (underlying)))

;($define! $macro
;  ($macro (params eparam body) #ignore
;    (list macro (list $vau params eparam body))))

($define! make-lambda
  ($lambda (params body static)
    (wrap (make-vau params #ignore body static))))

($define! $lambda
  ($vau (params body) static (make-lambda params body static)))

($define! eval
  ($generic (form env)
    (((symbol form) env)
     ($case (lookup form env)
       ((just value) value)
       ((nothing) (error "~a unbound" form))))
    (((cons combiner combinand) env) (combine combiner combinand env))))

($define! curry
  ($lambda (fn . left-args)
    ($lambda (right-args)
      (apply fn (append left-args right-args)))))

($define! rcurry
  ($lambda (fn . right-args)
    ($lambda left-args
      (apply fn (append left-args right-args)))))

($define! combine
  ($generic
    (((applicative combiner) combinand env)
     (combine combiner (map (rcurry eval env) combinand) env))
;    (((macro macro-combiner) combinand env)
;     (eval (combine macro-combiner combinand env) env))
    (((vau params eparam body static) combinand env)
     (eval body (vau-augment (augment-1 static eparam env) params combinand)))))

($define! augment-1
  ($lambda (env param arg)
    (bindings->environment (list (cons param arg)) env)))

($define! vau-augment
  ($lambda (env ptree arg)
    ($letrec ((flatten
	       ($generic
		 (((symbol name) arg) (list (cons name arg)))
		 (((cons left right) (cons argl argr)) (append (flatten left argl) (flatten right argr)))
		 (((ignore) #ignore) ())
		 (((null) (null)) ()))))
      (bindings->environment (flatten ptree arg) env))))

;($define! param->bindings
;  ;; ideally this would basically eval in a special sort of way
;  ;; but fuck
;  ($generic (param arg dyn)
;    (((symbol param) arg #ignore) (list param arg))
;    (((cons (symbol classname) rest) arg dyn)
;     ($case (lookup classname dyn)
;       ((just class)
;	(apply (destructurer class) (list rest arg dyn)))
;       ((nothing) (error "unknown class ~a" classname))))))

;($define! augment
;  ($lambda (env params args dyn)
;    ;; dyn is needed to get classes... kinda gross?
;    (bindings->environment (mappend (rcurry1 param->bindings dyn) params args) env)))

;;; classes

($define! top ($class () ()))

($define! standard-object ($class () (class values)))

($define! slots ($generic (((standard-object #ignore values)) values)))

($define! type ($class () ()))

($define! class ($class (type) (supers fields)))

($define! $class
  ($vau (supers fields) env
    (make-instance class ($if (null? supers) (list standard-object) (map (rcurry eval env) supers)) fields)))

($define! make-instance
  ;; durf straight metacircular
  ($lambda (class . values) (make-instance standard-object class values))))

;; in a real implementation these would probably be defined the other way
;; easier to specialize constructor than make-instance, and stuff
($define! constructor
  ($lambda (class) (curry make-instance class)))

($define! of-class
  ($lambda (class)
    ($generic
      (((standard-object objclass #ignore)) (eq? objclass class))
      ((#ignore) ; not a standard-object
       #f))))

($define! some1
  ($generic 
    (((pred (cons this more))) (or? (call pred this) (some1 pred more)))
    ((pred (null)) #f)))

($define! subclass?
  ;; bla bla genericism
  ($lambda (c1 c2)
    ;; cut a few metacircles
    ($cond ((eq? top c2) #t)
	   ((eq? top c1) #f)
	   ((eq? c1 c2) #t)
	   (#t ($case c1
		 ((standard-object supers #ignore)
		  (or? (find c2 supers)
		       (some1 (rcurry subclass? c2) supers))))))))

;;; case

($define! match?
  ($generic
    (((symbol var) #ignore #ignore) #t)
    (((ignore) #ignore #ignore) #t)
    (((cons class rest) object env)
     ($if ((of-class (eval class env)) object)
	  (every? (rcurry match? env) (slots object) rest)
	  #f))))

($define! pattern->bindings
  ($generic
    (((symbol var) object) (cons var object))
    (((ignore) #ignore) ())
    (((cons class rest) object)
     (mappend pattern->bindings rest (slots object)))))
    
($define! $case
  ($vau (form . clauses) env
    ($let ((object (eval form env)))
      ($letrec ((rec
		 ($lambda (clauses)
		   ($if (null? clauses)
			#inert
			($let ((((pattern body) clauses) clauses))
			  ($if (match? pattern object env)
			       (eval body (bindings->environment (pattern->bindings pattern object) env))
			       (rec clauses)))))))
	(rec clauses)))))

;;; methods

($define! method ($class () ()))

($define! standard-method ($class (method) (op specializers)))

($define! make-method (constructor standard-method))

($define! $method
  ($vau (sp-args body) env
    ($let ((split (map ($generic
			 (((symbol param)) (cons param top))
			 (((cons name (cons type (null)))) (cons name type)))
		       sp-args)))
      (make-method (map car args) (map cdr args)))))

($define! method-specializers ($generic (((method #ignore specializers)) specializers)))

;; imagine specialized-using-classes or w/e

($define! specialized?
  ($lambda (method . args)
    (every? of-type/standard-class args (method-specializers method))))

;;; environments

($define! environment ($class () ()))

($define! standard-environment ($class (environment) (alist parents)))

($define! parents ($generic (((environment #ignore parents)) parents)))

($define! bindings->environment
  ($lambda (alist . parents)
    (make-instance standard-environment alist parents)))

($define! pick-just
  ($lambda (fn list)
    ($letrec ((recur
	       ($generic
		 (((cons next more))
		  ($case (fn next)
		    ((just val) (just val))
		    ((nothing) (recur more))))
		 (((null)) (nothing)))))
      (recur list))))

($define! lookup
  ($generic
    ((name (standard-environment alist parents))
     ($case (assoc name alist)
       ((cons #ignore val) (just val))
       ((null) (pick-just (curry lookup name) parents))))))

;; skipping letrec because it'd either be a bunch of combinator bullshit

