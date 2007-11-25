(in-package :ropes)

(defclass rope-node ()
  ((node-len :accessor node-len :initform 0 :initarg :node-len)))

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
		 :right (make-rope (right source))))
