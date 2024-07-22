#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

#|Exercise 3.50
Complete the following definition, which generalizes stream-map to allow procedures that take multiple arguments|#

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream (apply proc (map stream-car argstreams))
                 (apply stream-map (cons proc
                                         (map stream-cdr argstreams))))))