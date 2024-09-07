(defpackage :ropes/test
  (:use :cl :fiveam :fiveam-matchers :ropes))

(in-package :ropes/test)
(test basic
  (let* ((s1 "This is a simple rope to test the basic operations")
         (r1 (make-rope s1)))
    (assert-that (rope-to-string r1)
                 (equal-to "This is a simple rope to test the basic operations"))
    (assert-that (rope-to-string (substring r1 :start 3 :end 9))
                 (equal-to "s is a"))
    ))

;;; Test a bigger string to make sure that this stuff really works. We
;;; will assembple a rope from smaller pieces.
(test substring
  (let* ((contents '("This is the first part of the long string that "
            "will comprise this rope. I found in random testing "
            "that a 300+ character string caused us problems "
            "when trying to extract a substring that happened "
            "to span 2 leafs in the tree. This shouldn't be a "
            "problem so we test for it here."))

         (r (reduce
            (lambda (accum item) (rope-concat accum item))
              contents :initial-value (make-rope "")))
         (tot-len (length (rope-to-string r))))

    ;; Make sure that the concatenations worked.
    (assert-that (rope-to-string r)
                 (equal-to (apply #'uiop:strcat contents)))

    ;; Result in one leaf, the left-most
    (assert-that (rope-to-string (substring r :start 1 :end 3))
                 (equal-to "hi"))

    ;; Result in one leaf, the right-most
    (assert-that (rope-to-string (substring r :start (- tot-len 5)
                                                   :end tot-len))
                 (equal-to "here."))

    (assert-that (rope-to-string (substring r :start 30 :end 50))
                 (equal-to "long string that wil"))

    ;; Involves all leaves
    (assert-that (rope-to-string (substring r :start 2 :end (- tot-len 1)))
                 (equal-to (subseq (rope-to-string r) 2 (- tot-len 1))))

    ;; A different way to turn a rope into a string, but a good test
    ;; for do-rope
    (let ((st ""))
      (do-rope (c r)
        (setf st (uiop:strcat st c)))
      (assert-that st
                   (equal-to (rope-to-string r))))
    ))

(defun repl (str n)
  (let ((s ""))
    (dotimes (i n s)
      (setf s (concatenate 'string s str)))))

(test deletion
  (let* ((contents (repl "0123456789abcdefghijklmnopqrstuvwxyz" 10))
         (r (make-rope contents))
         (d (rope-remove r :start 10 :end 15)))
    (assert-that (rope-to-string (substring d :start 0 :end 31))
                 (equal-to "0123456789fghijklmnopqrstuvwxyz"))
    (assert-that (rope-to-string r)
                 (equal-to contents))))
