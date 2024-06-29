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

;; scheme-number package
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


;; rational-package
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
    (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
    (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
    (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
    (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
    (lambda (n d) (tag (make-rat n d))))
  (put 'numer 'rational
    (lambda (x) (numer x)))
  (put 'denom 'rational
    (lambda (x) (denom x)))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;; Test the add-scheme-number function
(install-rational-package)

(define (add-rational-number x y)
  (apply-generic 'add x y))

(add-rational-number (make-rational 1 3) (make-rational 2 3)) ;'(rational 1 . 1)



;; Complex package

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (let ((square (lambda (x) (* x x))))
      (sqrt (+ (square (real-part z))
               (square (imag-part z))))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
    (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
    (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (let ((square (lambda (x) (* x x))))
      (cons (sqrt (+ (square x) (square y)))
            (atan y x))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
    (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
    (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
    (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
    (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
    (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
    (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
    (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
    (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)


(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; Test the add-complex function
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(define z1
  (make-complex-from-real-imag 4 3)) ;'(complex rectangular 4 . 3)

(magnitude z1) ;5

(define (add-complex x y)
  (apply-generic 'add x y))

(add-complex (make-complex-from-real-imag 1 3)
             (make-complex-from-real-imag 2 3)) ;'(complex rectangular 3 . 6)


#|Exercise 2.78:
In fact, however, all Lisp implementations do have a type system, which they use internally.
Primitive predicates such as symbol? and number? determine whether data objects have particular types.
Modify the definitions of type-tag, contents, and attach-tag from Section 2.4.2 so that our generic
system takes advantage of Scheme’s internal type system. That is to say, the system should work as
before except that ordinary numbers should be represented simply as Scheme numbers rather than as pairs
whose car is the symbol scheme-number|#

(attach-tag 'scheme-number 1) ;1
(type-tag 1) ;'scheme-number
(add-scheme-number (make-scheme-number 5) (make-scheme-number 6)) ;11


#|Exercise 2.79
Define a generic equality predicate equ? that tests the equality of two numbers, and install it in
the generic arithmetic package. This operation should work for ordinary numbers, rational numbers, and complex numbers|#

(define (equ? x y) (apply-generic 'equ? x y))

(define (install-arithmetic-package)
  ;; internal procedures
   (define (numer x) ((get 'numer 'rational) x)) 
   (define (denom x) ((get 'denom 'rational) x))
   (define (real z) ((get 'real-part '(complex)) z)) 
   (define (imag z) ((get 'imag-part '(complex)) z)) 
  ;; interface to rest of the system
  (put 'equ? '(scheme-number scheme-number) =)
  (put 'equ? '(rational rational)
    (lambda (x y) (and (= (numer x) (numer y))
                       (= (denom x) (denom y)))))
  (put 'equ? '(complex complex)
    (lambda (x y) (and (= (real x) (real y))
                       (= (imag x) (imag y)))))                
  'done)

(install-arithmetic-package)

;; display hash for debug
(for-each (lambda (pair)
            (printf "~a\n" pair))
          (hash->list operations))
; ((imag-part (polar)) . #<procedure:imag-part>)
; ((numer rational) . #<procedure:...etic_Operations.rkt:107:4>)
; ((imag-part (rectangular)) . #<procedure:imag-part>)
; ((real-part (rectangular)) . #<procedure:real-part>)
; ((sub (complex complex)) . #<procedure:...etic_Operations.rkt:204:4>)
; ((add (scheme-number scheme-number)) . #<procedure:...etic_Operations.rkt:49:7>)
; ((magnitude (complex)) . #<procedure:magnitude>)
; ((mul (scheme-number scheme-number)) . #<procedure:...etic_Operations.rkt:53:7>)
; ((real-part (complex)) . #<procedure:real-part>)
; ((add (rational rational)) . #<procedure:...etic_Operations.rkt:97:4>)
; ((equ? (complex complex)) . #<procedure:...etic_Operations.rkt:274:4>)
; ((equ? (rational rational)) . #<procedure:...etic_Operations.rkt:271:4>)
; ((equ? (scheme-number scheme-number)) . #<procedure:=>)
; ((sub (rational rational)) . #<procedure:...etic_Operations.rkt:99:4>)
; ((mul (complex complex)) . #<procedure:...etic_Operations.rkt:206:4>)
; ((real-part (polar)) . #<procedure:real-part>)
; ((add (complex complex)) . #<procedure:...etic_Operations.rkt:202:4>)
; ((angle (polar)) . #<procedure:angle>)
; ((div (rational rational)) . #<procedure:...etic_Operations.rkt:103:4>)
; ((div (complex complex)) . #<procedure:...etic_Operations.rkt:208:4>)
; ((angle (complex)) . #<procedure:angle>)
; ((make-from-mag-ang rectangular) . #<procedure:...etic_Operations.rkt:154:4>)
; ((make-from-real-imag rectangular) . #<procedure:...etic_Operations.rkt:152:4>)
; ((make-from-mag-ang complex) . #<procedure:...etic_Operations.rkt:212:4>)
; ((mul (rational rational)) . #<procedure:...etic_Operations.rkt:101:4>)
; ((make-from-real-imag complex) . #<procedure:...etic_Operations.rkt:210:4>)
; ((imag-part (complex)) . #<procedure:imag-part>)
; ((denom rational) . #<procedure:...etic_Operations.rkt:109:4>)
; ((make-from-mag-ang polar) . #<procedure:...etic_Operations.rkt:177:4>)
; ((magnitude (polar)) . #<procedure:magnitude>)
; ((magnitude (rectangular)) . #<procedure:magnitude>)
; ((angle (rectangular)) . #<procedure:angle>)
; ((make scheme-number) . #<procedure:...etic_Operations.rkt:56:28>)
; ((make rational) . #<procedure:...etic_Operations.rkt:105:4>)
; ((sub (scheme-number scheme-number)) . #<procedure:...etic_Operations.rkt:51:7>)
; ((div (scheme-number scheme-number)) . #<procedure:...etic_Operations.rkt:55:7>)
; ((make-from-real-imag polar) . #<procedure:...etic_Operations.rkt:175:4>)

(equ? (make-scheme-number 5) (make-scheme-number 6)) ;#f
(equ? (make-scheme-number 5) (make-scheme-number 5)) ;#t
(equ? (make-rational 1 3) (make-rational 2 3)) ;#f
(equ? (make-rational 1 3) (make-rational 1 3)) ;#t
(equ? (make-complex-from-real-imag 1 3)
      (make-complex-from-real-imag 2 3)) ;#f
(equ? (make-complex-from-real-imag 1 3)
      (make-complex-from-real-imag 1 3)) ;#t