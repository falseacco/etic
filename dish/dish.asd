(defsystem #:dish
  :description "fillthisinlater"
  :depends-on ("cl-who" "parenscript" "hunchentoot")
  :components ((:file "package")
(:file "index" :depends-on ("package"))))
