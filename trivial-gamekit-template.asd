;;;; trivial-gamekit-template.asd

(asdf:defsystem #:trivial-gamekit-template
  :description "trivial-gamekit-template"
  :author "Author Name Here"
  :license  "License Here"
  :version "0.0.1"
  :serial t
  :depends-on (trivial-gamekit)
  :components ((:file "package")
               (:file "trivial-gamekit-template")
	       (:file "misc")))
