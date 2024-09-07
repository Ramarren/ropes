(defpackage :ropes
  (:use :cl :iterate :alexandria)
  (:export #:make-rope
           #:rope-walk
           #:rope-concat
           #:rope-to-string
           #:rope-elt
           #:make-rope-iterator
	   #:do-rope
           #:rope-remove
           #:substring))
