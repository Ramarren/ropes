(in-package :ropes)

(defparameter *short-leaf-length* 20)
(defparameter *long-leaf-length* 100)

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

(defmethod print-object ((leaf rope-leaf) stream)
  (print-unreadable-object (leaf stream :type t :identity t)
    (format stream "\"~a\"" (str leaf))))

(defclass rope-concat (rope-node)
  ((left :accessor left :initform nil :initarg :left)
   (right :accessor right :initform nil :initarg :right)))

(defgeneric make-rope (source))

(defmethod make-rope ((source null))
  nil)

(defmethod make-rope ((source string))
  (if (< (length source) *long-leaf-length*)
      (make-instance 'rope-leaf :str source :node-len (length source))
      (rope-concat (make-rope (subseq source 0 (round (length source) 2)))
		   (make-rope (subseq source (round (length source) 2))))))

(defmethod make-rope ((source rope-leaf))
  (make-instance 'rope-leaf :str (str source)))

(defmethod make-rope ((source rope-concat))
  (make-instance 'rope-concat
		 :left (make-rope (left source))
		 :right (make-rope (right source))
		 :node-len (node-len source)
		 :depth (depth source)))

;;remember - functional data structure, no mutation in place when rebalancing
(defgeneric rope-rebalance (rope))

(defmethod rope-rebalance ((rope rope-leaf))
  rope)

(defgeneric rope-walk (rope fun))

(defmethod rope-walk ((rope rope-leaf) fun)
  (funcall fun rope))

(defmethod rope-walk ((rope rope-concat) fun)
  (rope-walk (left rope) fun)
  (rope-walk (right rope) fun)
  nil)

(defgeneric rope-insert-array (rope rope-array &optional n))

(defmethod rope-insert-array ((rope rope-node) rope-array &optional (n 0))
  (cond ((and (null (aref rope-array n))
	      (<= (node-len rope) (memo-fibbon (+ n 2))))
	 (setf (aref rope-array n) rope))
	((aref rope-array n)
	 (let ((new-rope (rope-concat (aref rope-array n) rope)))
	   (setf (aref rope-array n) nil)
	   (rope-insert-array new-rope rope-array n)))
	((> (node-len rope) (memo-fibbon (+ n 2)))
	 (rope-insert-array rope rope-array (1+ n)))))

(defmethod rope-rebalance ((rope rope-concat))
  (let ((ropes-array (make-array 64 :initial-element nil)))
    (rope-walk rope (rcurry #'rope-insert-array ropes-array))
    (reduce #'rope-concat ropes-array)))

(defgeneric rope-maybe-rebalance (rope))

(defmethod rope-maybe-rebalance ((rope rope-leaf))
  rope)

(defmethod rope-maybe-rebalance ((rope rope-concat))
  (cond
    ((< (node-len rope) (memo-fibbon (+ (depth rope) 2)))
     (rope-rebalance rope))
    (t (let ((new-left (rope-maybe-rebalance (left rope))) ;;add some conditionals here as above
	     (new-right (rope-maybe-rebalance (right rope))))
	 (if (and (eql new-left (left rope))
		  (eql new-right (right rope)))
	     rope
	     (make-instance 'rope-concat
			    :left new-left
			    :right new-right
			    :node-len (+ (node-len new-left) (node-len new-right))
			    :depth (1+ (max (depth new-left)
					    (depth new-right)))))))))

(defgeneric rope-concat (rope1 rope2))

(defmethod rope-concat ((rope1 null) rope2)
  rope2)

(defmethod rope-concat (rope1 (rope2 null))
  rope1)

(defmethod rope-concat (rope1 rope2)
  (rope-concat (make-rope rope1)
	       (make-rope rope2)))

(defmethod rope-concat ((rope1 rope-leaf) (rope2 rope-leaf))
  (if (< (+ (node-len rope1) (node-len rope2)) *short-leaf-length*)
      (make-rope (concatenate 'string (str rope1) (str rope2)))
      (make-instance 'rope-concat
		     :left rope1
		     :right rope2
		     :node-len (+ (node-len rope1) (node-len rope2))
		     :depth 1)))

(defmethod rope-concat ((rope1 rope-concat) (rope2 rope-concat))
  (rope-maybe-rebalance (make-instance 'rope-concat
				       :left rope1
				       :right rope2
				       :node-len (+ (node-len rope1) (node-len rope2))
				       :depth (1+ (max (depth rope1)
						       (depth rope2))))))

(defmethod rope-concat ((rope1 rope-concat) (rope2 rope-leaf))
  (if (typep (right rope1) 'rope-leaf)
      (let ((new-right (rope-concat (right rope1) rope2)))
	(make-instance 'rope-concat
		       :left (left rope1)
		       :right new-right
		       :node-len (+ (node-len (left rope1))
				    (node-len new-right))
		       :depth (1+ (max (depth rope1)
				       (depth new-right)))))
      (rope-maybe-rebalance (make-instance 'rope-concat
					   :left rope1
					   :right rope2
					   :node-len (+ (node-len rope1) (node-len rope2))
					   :depth (1+ (max (depth rope1)
							   (depth rope2)))))))

(defgeneric rope-to-string (rope))

(defmethod rope-to-string ((rope rope-node))
  (with-output-to-string (str)
      (rope-walk rope #'(lambda (x)
			  (princ (str x) str)))))

(defgeneric rope-elt (rope i))

(defmethod rope-elt ((rope rope-leaf) i)
  (char (str rope) i))

(defmethod rope-elt ((rope rope-concat) i)
  (if (< i (node-len (left rope)))
      (rope-elt (left rope) i)
      (rope-elt (right rope) (- i (node-len (left rope))))))

(defgeneric make-rope-iterator (rope))

(defmethod make-rope-iterator ((rope rope-leaf))
  (let ((i -1))
    #'(lambda ()
	(when (< i (node-len rope))
	  (char (str rope) (incf i))))))

(defmethod make-rope-iterator ((rope rope-concat))
  (let ((stack nil)
	(i -1))
    (rope-walk rope #'(lambda (x)
			(push x stack)))
    (setf stack (nreverse stack))
    #'(lambda ()
	(cond
	  ((null stack) nil)
	  ((= (1+ i) (node-len (car stack)))
	   (pop stack)
	   (when stack
	     (setf i 0)
	     (char (str (car stack)) 0)))
	  (t (char (str (car stack)) (incf i)))))))

(defmacro do-rope ((var rope) &body body)
  (let ((rope-iterator (gensym)))
   `(let ((,rope-iterator (make-rope-iterator ,rope)))
      (iter (for ,var next (funcall ,rope-iterator))
	    (while ,var)
	    ,@body))))

(defgeneric rope-substring (rope &key start end))

(defmethod rope-substring ((rope rope-leaf) &key (start 0) end)
  (let ((start (max start 0))
	(end (if (null end)
		 (node-len rope)
		 (min (node-len rope) end))))
    (make-rope (subseq (str rope) start end))))

(defmethod rope-substring ((rope rope-concat) &key (start 0) end)
  (let ((left (if (and (<= start 0)
		       (>= end (node-len (left rope))))
		  (left rope)
		  (rope-substring (left rope) :start start :end end)))
	(right (cond
		 ((and (<= start (node-len (left rope)))
		       (>= end
			   (+ (node-len (left rope))
			      (node-len (right rope)))))
		  (right rope))
		 ((or (<= end (node-len (left rope)))
		      (>= start (node-len rope)))
		  nil)
		 (t (rope-substring (right rope)
				    :start (- start (node-len (left rope)))
				    :end (- end (node-len (left rope))))))))
    (rope-concat left right)))
