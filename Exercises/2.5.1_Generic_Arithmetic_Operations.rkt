#lang racket


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

;; Define specific arithmetic operations using the generic apply-generic function
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;; Define a function to attach a type tag to a value
(define (attach-tag type-tag contents)
        (cons type-tag contents))

;; Define a function to retrieve the type tag from a tagged datum
(define (type-tag datum)
        (if (pair? datum)
                (car datum)
                (error "Bad tagged datum: TYPE-TAG" datum)))

;; Define a function to retrieve the contents from a tagged datum
(define (contents datum)
        (if (pair? datum)
                (cdr datum)
                (error "Bad tagged datum: CONTENTS" datum)))

;; Define a function to install the scheme-number package
(define (install-scheme-number-package)
        (define (tag x) (attach-tag 'scheme-number x))
        (put 'add '(scheme-number scheme-number)
                (lambda (x y) (tag (+ x y))))
        (put 'sub '(scheme-number scheme-number)
                (lambda (x y) (tag (- x y))))
        (put 'mul '(scheme-number scheme-number)
                (lambda (x y) (tag (* x y))))
        (put 'div '(scheme-number scheme-number)
                (lambda (x y) (tag (/ x y))))
        (put 'make 'scheme-number (lambda (x) (tag x)))
        'done)

;; Define a function to create a scheme-number
(define (make-scheme-number n)
        ((get 'make 'scheme-number) n))

;; Test the add-scheme-number function
(install-scheme-number-package)

(define (add-scheme-number x y)
        (apply-generic 'add x y))

(add-scheme-number (make-scheme-number 5) (make-scheme-number 6)) ;'(scheme-number . 11)




