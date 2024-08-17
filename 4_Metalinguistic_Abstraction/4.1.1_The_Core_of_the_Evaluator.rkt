#lang racket

(require "evaluator.rkt")

;the-global-environment
(define e0 (setup-environment))


;test
(eval '(define (add x y) (+ x y)) e0) ;=> 'ok
(eval '(add 1 2) e0) ;=> 3

(eval '(((lambda (x) (lambda (y) (- x y))) 3) 4) e0) ;=> -1


#|Exercise 4.1
Notice that we cannot tell whether the metacircular evaluator evaluates operands from left to right or
from right to left. Its evaluation order is inherited from the underlying Lisp: If the arguments to cons
in list-of-values are evaluated from left to right, then list-of-values will evaluate operands from left
to right; and if the arguments to cons are evaluated from right to left, then list-of-values will evaluate
operands from right to left.
Write a version of list-of-values that evaluates operands from left to right regardless of the order of
evaluation in the underlying Lisp|#

;test
(define test-env (setup-environment))

(eval '(define (test-order x y) (cons x y)) test-env) ;=> 'ok
(eval '(test-order (display "left") (display "right")) test-env) ;=> leftright'(#<void> . #<void>)