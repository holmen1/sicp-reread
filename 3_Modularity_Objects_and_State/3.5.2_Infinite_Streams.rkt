#lang racket

(require "streams.rkt")

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (show x)
  (display-line x)
  x)

(define x
  (stream-map show
              (integers-starting-from 0)))

(stream-ref x 5)
; 0
; 1
; 2
; 3
; 4
; 55


(define integers (integers-starting-from 1))
;; Using integers we can define other infinite streams, such as the stream of integers that are not divisible by 7:

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

;; Then we can find integers not divisible by 7 simply by accessing elements of this stream:
(stream-ref no-sevens 100) ; => 117
(stream-ref no-sevens 101) ; => 118
(stream-ref no-sevens 102) ; => 120


;; In analogy with integers, we can define the infinite stream of Fibonacci numbers:

(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

;; fibs is a pair whose car is 0 and whose cdr is a promise to evaluate (fibgen 1 1).
;; When we evaluate this delayed (fibgen 1 1), it will produce a pair whose car is 1 and whose cdr is
;; a promise to evaluate (fibgen 1 2), and so on

(stream-ref fibs 5) ; => 5
(stream-ref fibs 6) ; => 8
(stream-ref fibs 7) ; => 13