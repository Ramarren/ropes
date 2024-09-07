(defpackage :ropes-system (:use :cl :asdf :uiop))

(in-package :ropes-system)

(defsystem :ropes
  :description "An implementation of ropes datastructure"
  :licence "BSD-style"
  :components ((:file "package")
	       (:file "rope" :depends-on ("package")))
  :depends-on (:iterate :alexandria)
  :in-order-to ((test-op (test-op "ropes/tests"))))

(defsystem :ropes/tests
  :depends-on ("ropes" "fiveam" "fiveam-matchers" "uiop")
  :components ((:file "tests/rope"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run-all-tests )))
