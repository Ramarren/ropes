(defpackage :ropes
  (:use :cl :iterate)
  (:import-from #:arnesi #:aif #:it #:rcurry #:curry)
  (:export #:make-rope #:rope-walk #:rope-concat #:rope-to-string #:rope-elt #:make-rope-iterator
	   #:do-rope #:rope-substring))