(in-package :net.mathschallenge.p2-code)

;;Get the Nth fibonacci number (based on logarithmic time algorithm at http://en.wikipedia.org/wiki/Fibonacci_number)
(defun fibonacci (n)
  (let ((pair fibpair(n))))
    (last pair))

(defun fibonacci-pair (n)
  ()
    

;;List of all fibonacci numbers below max
(defun fibonacci-below (max)
  (

;;List of integers from min to max
(defun sequence-list (min max)
  (loop for i from min upto max collect i))

;;Is X divisible by any of these numbers?
(defun divisible-by-any (x divisors)
  (if
   (=
    (mod x
	 (first divisors))
    0)
   T
   (if
    (equal (rest divisors) nil)
    nil
    (divisible-by-any x
		      (rest divisors)))))

;;Return only those numbers divisible by (at least) one of the divisors
(defun divisibles (int-list divisors)
  (remove-if-not #'(lambda (x) (divisible-by-any x divisors)) int-list))

;;Sum all the divisible numbers
(defun sum-multiples(divisors min max)
  (reduce #'+ (divisibles (sequence-list min max) divisors)))
