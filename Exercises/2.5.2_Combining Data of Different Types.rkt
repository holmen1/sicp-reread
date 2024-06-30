#lang racket
;; Moularized arithmetic operations from 2.5.1
(require "utils.rkt" "rational.rkt" "arithmetic_operations.rkt")



;; Test rational numbers
(install-rational-package)

(add (make-rational 1 3) (make-rational 2 3)) ;'(rational 1 . 1)
(=zero? (make-rational 0 3)) ;#t
(equ? (make-rational 1 3) (make-rational 2 3)) ;#f

