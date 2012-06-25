(defpackage :ropes-system (:use :cl :asdf))

(in-package :ropes-system)

(defsystem :ropes
  :description "An implementation of ropes datastructure"
  :licence "BSD-style"
  :components ((:file "package")
	       (:file "rope" :depends-on ("package")))
  :depends-on (:iterate :alexandria))
