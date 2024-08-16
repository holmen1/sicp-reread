#lang racket

(require "evaluator.rkt")

;the-global-environment
(define e0 (setup-environment))


;test
(eval '(define (add x y) (+ x y)) e0) ;=> 'ok
(eval '(add 1 2) e0) ;=> 3

(eval '(((lambda (x) (lambda (y) (- x y))) 3) 4) e0) ;=> -1