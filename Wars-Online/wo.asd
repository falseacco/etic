(defsystem #:wo
  :description "Advance Wars and -like PVP Client"
  :depends-on ("hunchentoot"
	       "cl-who"
	       "parenscript")
  :components ((:file "package")
(:file "wo" :depends-on ("package"))))
