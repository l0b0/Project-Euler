(defpackage :net.mathschallenge.p1-code
  (:use :common-lisp)
  (:export :divisible-by-any
	   :divisibles
	   :sequence-list
	   :sum-multiples))

(defpackage :net.mathschallenge.p1-solve
  (:use :common-lisp :net.mathschallenge.p1-code))

(defpackage :net.mathschallenge.p6-code
  (:use :common-lisp)
  (:export :square
	   :sum-squares
	   :square-sum))

(defpackage :net.mathschallenge.p6-solve
  (:use :common-lisp :net.mathschallenge.p6-code)
  (:import-from :net.mathschallenge.p1-code :sequence-list))

