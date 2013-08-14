($define! eval ($generic (form env)))
(add-method! eval
  ($method ((form symbol_t) env) (lookup form env)))
(add-method! eval
  ($method ((form cons_t) env) (combine (car form) (cdr form) env)))
(add-method! eval
  ($method (form env) form))

;; constructor w/ two fields; this is shit
($define! ((cons car cdr) (cons_t car_t cdr_t)) (constructor 2))

;; yeah fuck it
($define! list_t
  ($lambda (type)
    (disjoin_t nil_t
	       (cons_t type (list_t type)))))

($define! alist_t (compose list_t cons_t))

($define! env_t (singleton)) ; ?????
($define! simple-env_t (alist_t symbol_t top_t)) ; type syn.

($define! lookup ($generic (symbol env)))
(add-method! lookup
  ;; could use some pointlessness
  ($method (symbol (env simple-env_t)) (assoc symbol env)))

($define! ((%make-array-env array-env-symbols array-env values)
	   (array-env_t #ignore #ignore))
  ;; fuck, i don't know
  (constructor 2))

(add-method! lookup
  ($method (symbol (env array-env_t))
    (aref (array-env-values env)
	  (position symbol (array-env-symbols env))))) ; i hate myself

($define! combine ($generic (combiner combinand env)))
(add-method! combine 

;; pwhatever (usually) indicates how a call to whatever can be compiled
;; old peval is sort of like (peval (make-const form) (make-const env))

($define! peval
  ($lambda (form env)
    ($let ((t (form-type form)))
      ($cond ((subtype? t symbol_t) (plookup form env))
	     ((subtype? form cons_t) (pcombine (pcar form env) (pcdr form env) env))
	     ((subtype? form (not_t (or_t symbol_t cons_t))) (pconst form env))
	     (#t 
	       (pcombine (make-const $typecase)
			 (make-const
			  (list (list symbol_t (list lookup form env))
				(list cons_t (list operate (pcar form) (pcdr form) env))
				(list (list top_t form))))
			 env))))))

($define! pconst
  ($lambda (form env)
    (make-const form)))

($define! plookup
  ($lambda (form env)
    ($let ((envtype (form-type env)))
      ($tri/if (bound_t? form env)
	       ($if (subtype? envtype fixed_env_t)
		    (make-fixed-lookup form env)
		    ;; static typecheck garbage here
		    (make-generic-lookup form env))
	       (make-death unbound-variable form env)
	       (make-generic-lookup form env (lookup-type form env))))))

($define! pcombine
  ($lambda (combiner combinand env)
    ($let ((ctype (form-type combiner)))
      ($cond ((subtype? ctype applicative_t)
	      (pcombine (punwrap combiner env) (pevlis combinand env) env))
	     ((subtype? ctype 

($define! pcar
  ($lambda (form env)
    (pcombine (make-const (unwrap car)) (list form) env)))

($define! pcdr
  ($lambda (form env)
    (pcombine (make-const (unwrap cdr)) (list form) env)))

; evlis list env = map (rcurry eval env) list
($define! evlis
  ($lambda (forms env)
    (map (rcurry eval env) forms)))

; ---

($define! macro? ($lambda (x) #f))

($define! rcurry/2
  ($lambda (fn second)
    ($lambda (first) (fn first second))))

($define! every?
  ($lambda (pred . lists)
    (apply and? (apply map (cons pred lists)))))

($define! any?
  ($lambda (pred . lists)
    (apply or? (apply map (cons pred lists)))))

($define! $quote ($vau (obj) #ignore obj))

($define! fully-evaluated?
  ($lambda (form)
    ($cond ((symbol? form) #f)
	   ((pair? form) (eq? (car form) $quote))
	   (#t #t))))

($define! evaluation-object
  ;; assuming it's already been vetted by fully-evaluated? this is pretty trivial
  ($lambda (form)
    ($if (pair? form)
	 (cadr form) ; ($quote foo) => foo
	 form)))

($define! maybe-quote
  ($lambda (obj)
    ($if ($or? (symbol? obj) (pair? obj))
	 (list $quote obj)
	 obj)))

($define! peval
  ($lambda (form env)
    ($cond ((symbol? form)
	    ($if (bound? env form)
		 (maybe-quote (lookup form env)) ; lookup
		 form))
	   ((pair? form) (pcombine (peval (car form) env) (cdr form) env))
	   (#t form))))

($define! pcombine
  ($lambda (combiner combinand env)
    ($cond ((applicative? combiner)
	    ($let ((evaled (map (rcurry/2 peval env) combinand)))
	      ($if (every? fully-evaluated? evaled)
		   (pcombine (unwrap combiner) (map evaluation-object evaled) env)
		   (cons combiner evaled))))
	   ((macro? combiner)
	    ;; by definition we can expand ahead of time
	    (peval (macroexpand combiner combinand env) env))
	   ((constant-operative? combiner)
	    (constant-operate combiner combinand env))
	   ;; thiiiiis really shouldn't even be here
	   ((eq? combiner $define!)
	    (list $define! (first combinand) (peval (second combinand) env)))
	   ((eq? combiner $if)
	    ($let ((condition (peval (car combinand) env))
		   (consequent (peval (cadr combinand) env))
		   (alternate (peval (caddr combinand) env)))
	      ($if (fully-evaluated? condition)
		   ($if (evaluation-object condition)
			consequent
			alternate)
		   (list $if condition consequent alternate))))
	   ((eq? combiner $vau)
	    ($let* (((ptree eparam body) combinand)
		    (syms (ptree-symbols ptree))
		    (all-params (cons eparam syms))
		    (shadow (shadowed-env all-params env))
		    (pevaled (peval body shadow)))
	      ($if (env-droppable? body all-params)
		   ;; if the only free symbols in the body are parameters,
		   ;; (after peval wrt what we know of the surroundings)
		   ;; we can construct the operative ahead of time! yay
		   (combine $vau (list ptree eparam pevaled) (make-environment)) ; make-op would be nicer, oh well
		   ;; no dice; at least we peval'd what body we could
		   (list $vau ptree eparam pevaled))))
	   (#t (cons combiner combinand)))))

($define! constant-operative?
  ($lambda (combiner)
    ($cond ((not? (operative? combiner)) #f)
	   ((memq? combiner
		   (map unwrap (list boolean? eq? equal? symbol? inert? pair? null?
				     environment? ignore? operative? applicative?
				     wrap unwrap)))
	    #t)
	   (#t #f))))

($define! constant-operate
  ($lambda (combiner combinand env)
    ;; shouldn't need the environment but... i dunno
    (combine combiner combinand env)))

($define! ptree-symbols
  ($lambda (ptree)
    ($cond ((null? ptree) ())
	   ((ignore? ptree) ())
	   ((symbol? ptree) (list ptree))
	   ((pair? ptree) (append (ptree-symbols (car ptree)) (ptree-symbols (cdr ptree))))
	   (#t (error "bad ptree")))))

($define! set-<?
  ($lambda (set1 set2)
    (every? (rcurry/2 memq? set2) set1)))

($define! env-droppable?
  ;; #f if there are free variables in form.
  ($lambda (form bound) #f))

;;;;

($define! merge
  ($lambda lss
    ($if (any? boolean? lss)
	 #f
	  (apply append lss))))

($define! peval/free
  ($lambda (form env)
    ($cond ((symbol? form)
	    ($if (bound? env form)
		 (cons (maybe-quote (lookup form env)) ())
		 (cons form (list form))))
	   ((pair? form)
	    ($let* (((combiner . free) (peval/free (car form) env))
		    ((result . free2) (pcombine/free combiner (cdr form) env)))
	      (cons result (merge free free2))))
	   (#t (cons form ())))))

($define! pcombine/free
  ($lambda (combiner combinand env)
    ($cond ((applicative? combiner)
	    ($let* ((args (map (rcurry/2 peval/free env) combinand))
		    (evaled (map car args))
		    (free (apply merge (map cdr args))))
	      ($if (every? fully-evaluated? evaled)
		   ($let (((result . free2) (pcombine/free (unwrap combiner) (map evaluation-object evaled) env)))
		     (cons result (merge free free2)))
		   (cons (cons combiner evaled) free))))
	   ((macro? combiner)
	    ;; by definition we can expand ahead of time
	    (peval (macroexpand combiner combinand env) env))
	   ((constant-operative? combiner)
	    (cons (constant-operate combiner combinand env) ()))
	   ;; thiiiiis really shouldn't even be here
	   ((eq? combiner $define!)
	    ($let (((result . free) (peval/free (second combinand) env)))
	      (cons (list $define! (first combinand) (peval/free (second combinand) env)) free)))
	   ((eq? combiner $if)
	    ($let (((condition . free0) (peval/free (car combinand) env))
		   ((consequent . free1) (peval/free (cadr combinand) env))
		   ((alternate . free2) (peval/free (caddr combinand) env)))
	      ($if (fully-evaluated? condition)
		   ($if (evaluation-object condition)
			(cons consequent free1)
			(cons alternate free2))
		   (cons (list $if condition consequent alternate)
			 (merge free0 free1 free2)))))
	   ((eq? combiner $vau)
	    ($let* (((ptree eparam body) combinand)
		    (syms (ptree-symbols ptree))
		    (all-params (cons eparam syms))
		    (shadow (shadowed-env all-params env))
		    ((pevaled . free) (peval/free body shadow)))
	      ($if (set-<? free all-params)
		   ;; if the only free symbols in the body are parameters,
		   ;; (after peval wrt what we know of the surroundings)
		   ;; we can construct the operative ahead of time! yay
		   (cons (combine $vau (list ptree eparam pevaled) (make-environment)) ())
		   ;; no dice; at least we peval'd what body we could
		   (cons (list $vau ptree eparam pevaled) free))))
	   ;; given the anti-standard predicate to determine if an operative uses its calling environment,
	   ;; ((env-ignoring? combiner) ())
	   (#t (cons (cons combiner combinand) #f)))))
