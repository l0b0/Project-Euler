(load (compile-file "packages.lisp")) ;Defines packages

(load (compile-file "p2-code.lisp")) ;Necessary functions to solve this problem

(in-package :net.mathschallenge.p2-solve)

;;Set according to website
(let (
      (lower-limit 1)
      (upper-limit 1000) ;Should be 1,000,000
      )
  ;;Run!
  (net.mathschallenge.p2-code:))