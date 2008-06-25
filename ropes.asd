(defpackage :ropes-system (:use :cl :asdf))

(in-package :ropes-system)

(defsystem :ropes
  :components ((:file "package")
	       (:file "rope" :depends-on ("package")))
  :depends-on (:iterate :alexandria))