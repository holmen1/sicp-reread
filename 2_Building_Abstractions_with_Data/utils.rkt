#lang racket

(provide attach-tag operations put get type-tag contents)

;; Define a hash table to store the operations
(define operations (make-hash))
(define (put key1 key2 value)
    (hash-set! operations (list key1 key2) value))
(define (get key1 key2)
    (hash-ref operations (list key1 key2)
                         (lambda () (error "Key not found" (list key1 key2)))))

;; Define a function to attach a type tag to a value
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
    contents
    (cons type-tag contents)))

;; Define a function to retrieve the type tag from a tagged datum
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

;; Define a function to retrieve the contents from a tagged datum
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: CONTENTS" datum))))
