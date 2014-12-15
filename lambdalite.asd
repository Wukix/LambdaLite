(in-package :cl-user)

(defpackage :lambdalite-asd
  (:use :asdf :cl))

(in-package :lambdalite-asd)

(defsystem :lambdalite
  :depends-on (:wu-sugar :bordeaux-threads)
  :name "lambdalite"
  :description "A functional, relational Lisp database"
  :long-description "A functional, relational database in about 250 lines of Common Lisp"
  :version "1.0.1"
  :author "Wukix Inc <engineering@wukix.com>"
  :license "MIT"
  :components ((:file "lambdalite")))
