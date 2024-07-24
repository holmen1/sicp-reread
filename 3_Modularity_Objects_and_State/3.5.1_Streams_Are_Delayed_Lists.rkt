#lang sicp

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map-old proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
  (stream-map-old proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
  (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream low
                 (stream-enumerate-interval (+ low 1) high))))

#|Exercise 3.50
Complete the following definition, which generalizes stream-map to allow procedures that take multiple arguments|#

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream (apply proc (map stream-car argstreams))
                 (apply stream-map (cons proc
                                         (map stream-cdr argstreams))))))



#|Exercise 3.51
In order to take a closer look at delayed evaluation, we will use the following procedure,
which simply returns its argument after printing it:|#
(define (show x)
  (display-line x)
  x)

#|What does the interpreter print in response to evaluating each expression in the following sequence?|#
(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))

(stream-ref x 5)
; 0
; 1
; 2
; 3
; 4
; 55

(stream-ref x 7)
; 6
; 77