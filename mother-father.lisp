(defvar *closure*
  (let ((x 55))
    (lambda (y) (+ y x))))

(defvar *proc* (closure-procedure *test*))

(typep *closure* 'function) => T
(typep *proc* 'function) => NIL

(procedure-free *proc*) => (Y) or (Y X) [or (X Y) but w/e] or (Y +) or in fact any set containing at least Y

(closure-environment *closure*) => an environment, but otherwise unspecified

[so for example it might be compiled as (lambda (y) (+ y 55)), or it might not.
  Hopefully this is enough information-hiding.]

One problem here is arguent information erasure. E.g. in SBCL on high opt setting, what arguments a function takes are unknown. Or more precisely, ((lambda (a b c) ...) 4 5) is undefined rather than a definite error, on low safety. Or something like that.

This amounts to allowing procedure-free to return an empty set or some undefinedness marker or whatever. But it would be nice for

(run-procedure *proc* (augment-environment (closure-environment *closure*) 'y 3))

to be defined as returning 58.

Hm no this is all wrong. Basically:
A procedure is a thunk, saved text (but not necessarily the source).
A value can be obtained from a procedure by running it in an environment where all its free variables are bound.
A closure is a procedure partially evaluated with respect to some environment.
Maybe.

Say we have a function (make-closure procedure environment) that just makes a closure by taking the unaltered procedure and packaging it up with the environment. Then (apply result args) just augments the packaged environment with the args bound to the frees and that's that, i.e. the standard naïve Lisp implementation. But a slower more involved function, call it (papply procedure environment), might do some more involved analysis to remove parts of the packaged environment.

E.g. (papply (closure-procedure (lambda (a b c) (funcall c (+ a b)))) (make-environment 'a 4 'b 6)) might result in a closure of a new procedure that just tail-calls c with 10 and an empty closure environment (and c still free, of course). Probably you'd want to do both at different kinds, or with configurable levels between them, since papply amounts to a compiler.

I think I'm overthinking a lot of things.
