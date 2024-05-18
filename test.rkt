#lang racket

(define x (cons 1 3))
(define y (cons 5 6))
(define z (cons x y))

(car (cdr z)) ;5