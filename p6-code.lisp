(in-package :net.mathschallenge.p6-code)

;;Square
(defun square(x)
  (* x x))

;;Sum of the squares of a sequence
(defun sum-squares(numbers)
  (reduce #'+ (mapcar #'square numbers)))

;;Square of the sum of a sequence
(defun square-sum(numbers)
  (square (reduce #'+ numbers)))