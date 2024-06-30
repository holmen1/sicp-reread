#lang racket
;; Moularized arithmetic operations from 2.5.1
(require "utils.rkt")
(require "arithmetic_operations.rkt")
(require "number.rkt")
(require "rational.rkt")
(require "complex.rkt")

(install-scheme-number-package)


;; Test rational numbers
(install-rational-package)

(add (make-rational 1 3) (make-rational 2 3)) ;'(rational 1 . 1)
(=zero? (make-rational 0 3)) ;#t
(equ? (make-rational 1 3) (make-rational 2 3)) ;#f

;; Test complex numbers
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(for-each (lambda (pair)
            (printf "~a\n" pair))
          (hash->list operations))

(make-complex-from-real-imag 1 3)

(define (magnitude z) (apply-generic 'magnitude z))
(magnitude (make-complex-from-real-imag 4 3)) ;5


(add (make-complex-from-real-imag 1 3)
     (make-complex-from-real-imag 2 3)) ;'(complex rectangular 3 . 6)

