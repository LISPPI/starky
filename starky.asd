;;
;;
(asdf:defsystem #:starky
  :description "set me!"
  :author "StackSmith <fpgasm@apple2.x10.mx>"
  :license "BSD 3-clause license"
  :serial t
  :depends-on (#:cl-binary #:trivial-with #:lisppi-openvg)
  :components ((:file "package")
	       (:file "util")
	       (:file "vec")

	       (:file "cold-font")
	       (:file "font")
	       (:file "dejavu-sans-mono")
	       
	       (:file "starky")))

