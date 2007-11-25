(defpackage :rope-system (:use :cl :asdf))

(in-package :rope-system)

(defsystem :rope
  :components ((:file "package")
	       (:file "rope" :depends-on ("package")))
  :depends-on (:arnesi))