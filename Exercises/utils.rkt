#lang racket

(provide attach-tag put get apply-generic type-tag contents)

;; Define a hash table to store the operations
(define operations (make-hash))
(define (put key1 key2 value)
    (hash-set! operations (list key1 key2) value))
(define (get key1 key2)
    (hash-ref operations (list key1 key2)
                         (lambda () (error "Key not found" (list key1 key2)))))

;; Define a generic function to apply an operation to multiple arguments
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error "No method for these types: APPLY-GENERIC"
                (list op type-tags))))))

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
