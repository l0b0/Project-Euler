(load (compile-file "packages.lisp")) ;Defines packages

(load (compile-file "p6-code.lisp")) ;Necessary functions to solve this problem

(load (compile-file "p1-code.lisp")) ;Necessary functions to solve this problem

(in-package :net.mathschallenge.p6-solve)

;;Set according to website
(let* ((lower-limit 1)
       (upper-limit 100)
       (numbers (net.mathschallenge.p1-code:sequence-list lower-limit upper-limit)))
  ;;Run!
  (- (net.mathschallenge.p6-code:square-sum numbers) (net.mathschallenge.p6-code:sum-squares numbers)))