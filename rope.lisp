(in-package :ropes)

(let ((memo-table (make-hash-table)))
  (defun memo-fibbon (k)
    (assert (>= k 0))
    (aif (gethash k memo-table)
	 it
	 (case k
	   (0 0)
	   (1 1)
	   (t (setf (gethash k memo-table)
		    (+ (memo-fibbon (1- k))
		       (memo-fibbon (- k 2)))))))))

(defclass rope-node ()
  ((node-len :accessor node-len :initform 0 :initarg :node-len)
   (depth :accessor depth :initform 0 :initarg :depth)))

(defclass rope-leaf (rope-node)
  ((str :accessor str :initform "" :initarg :str)))

(defclass rope-concat (rope-node)
  ((left :accessor left :initform nil :initarg :left)
   (right :accessor right :initform nil :initarg :right)))

(defgeneric make-rope (source))

(defmethod make-rope ((source string))
  (make-instance 'rope-leaf :str source))

(defmethod make-rope ((source rope-leaf))
  (make-instance 'rope-leaf :str (str source)))

(defmethod make-rope ((source rope-concat))
  (make-instance 'rope-concat
		 :left (make-rope (left source))
		 :right (make-rope (right source))
		 :depth (depth source)))

;;remember - functional data structure, no mutation in place when rebalancing
(defgeneric rope-rebalance (rope))

(defmethod rope-rebalance ((rope rope-leaf))
  rope)

(defmethod rope-rebalance ((rope rope-concat))
  )

(defgeneric rope-maybe-rebalance (rope))

(defmethod rope-maybe-rebalance ((rope rope-leaf))
  rope)

(defmethod rope-maybe-rebalance ((rope rope-concat))
  (cond
    ((> (node-len rope) (memo-fibbon (+ (depth rope) 2)))
     (rope-rebalance rope))
    (t (rope-maybe-rebalance (left rope));;add some conditionals here as above
       (rope-maybe-rebalance (right rope)))))

(defgeneric rope-concat (rope1 rope2))