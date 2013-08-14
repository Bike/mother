(asdf:defsystem #:mother
  :author "Bike <aeshtaer@gmail.com>"
  :license "WTFPL"
  :version "0.0.1"
  :depends-on (#:alexandria)
  :components ((:file "mother")
	       (:file "repl" :depends-on ("mother" "primdef" "read" "eval" "dumb-1parent-env"))
	       (:file "read" :depends-on ("mother"))
	       (:file "applicative" :depends-on ("eval" "mother"))
	       (:file "eval" :depends-on ("env" "mother"))
	       (:file "env" :depends-on ("mother"))
	       (:file "primdef" :depends-on ("eval"
					     "list" ; used in typers
					     "applicative" ; wrap, unwrap
					     "dumb-vau" ; make-vau
					     "generic-env" ; bindings->generic-env
					     "flat-env" ; definition of ground
					     "core" ; $set!
					     "mother"))
	       (:file "core" :depends-on ("list" "flat-env" "mother"))
	       (:file "dumb-vau" :depends-on ("dumb-1parent-env" "mother"))
	       (:file "list" :depends-on ("mother"))
	       (:file "shadow-env" :depends-on ("env" "mother"))
	       (:file "flat-env" :depends-on ("env" "mother"))
	       (:file "dumb-1parent-env" :depends-on ("flat-env" "mother"))
	       (:file "generic-env" :depends-on ("flat-env" "mother"))))
