;;
;;
(asdf:defsystem #:starky
  :description "set me!"
  :author "StackSmith <fpgasm@apple2.x10.mx>"
  :license "BSD 3-clause license"
  :serial t
  :depends-on (#:cl-freetype2 #:trivial-with #:lisppi-openvg)
  :components ((:file "package")
	       (:file "util")
	       (:file "vec")

;;	       (:file "font")

	       
	       (:file "starky")
	       ;;	       (:file "fake-font")
	       ;;	       (:file "dejavu-sans-mono")

	       (:file "ft")

;;	       (:file "test")
	       ))

