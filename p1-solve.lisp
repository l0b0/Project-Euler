(load (compile-file "packages.lisp")) ;Defines packages

(load (compile-file "p1-code.lisp")) ;Necessary functions to solve this problem

(in-package :net.mathschallenge.p1-solve)

;;Set according to website
(let ((divisors `(5 3))
      (lower-limit 1)
      (upper-limit 999) ;Up to but NOT including
      )
  ;;Run!
  (net.mathschallenge.p1-code:sum-multiples divisors lower-limit upper-limit))