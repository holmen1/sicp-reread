#lang racket
;; Moularized arithmetic operations from 2.5.1
(require "complex.rkt")
(require "arithmetic_operations.rkt")
(require "number.rkt")
(require "rational.rkt")

(install-scheme-number-package)


;; Test rational numbers
(install-rational-package)

(add (make-rational 1 3) (make-rational 2 3)) ;'(rational 1 . 1)
(=zero? (make-rational 0 3)) ;#t
(equ? (make-rational 1 3) (make-rational 2 3)) ;#f

;; Test complex numbers
(install-complex-package)

(magnitude (make-complex-from-real-imag 4 3)) ;5
(add (make-complex-from-real-imag 1 3)
     (make-complex-from-real-imag 2 3)) ;'(complex rectangular 3 . 6)
(equ? (make-complex-from-real-imag 1 3)
      (make-complex-from-real-imag 2 3)) ;#f
(=zero? (make-complex-from-real-imag 0 0)) ;#t

