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