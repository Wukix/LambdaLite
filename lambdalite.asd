(in-package :cl-user)

(defpackage :lambdalite-asd
  (:use :asdf :cl))

(in-package :lambdalite-asd)

(defsystem :lambdalite
  :depends-on (:wu-sugar :bordeaux-threads)
  :name "lambdalite"
  :author "Wukix Inc <engineering@wukix.com>"
  :components ((:file "lambdalite")))
