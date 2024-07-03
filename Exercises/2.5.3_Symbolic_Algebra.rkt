#lang racket

(require "polynomial.rkt")
(require "arithmetic_operations.rkt")
(require "number.rkt")

(install-scheme-number-package)
;(require "utils.rkt") ; for debug


(install-polynomial-package)

; (for-each (lambda (pair)
;             (printf "~a\n" pair))
;           (hash->list operations))

(make-polynomial 'x '((2 0)))             ;'(polynomial x (2 0))
(make-polynomial 'x '((2 0) (1 1) (0 6))) ;'(polynomial x (2 0) (1 1) (0 6))

(=zero? (make-polynomial 'x '())) ;#t
(=zero? (make-polynomial 'x '((2 0)))) ;#t
(=zero? (make-polynomial 'x '((2 1) (1 0)))) ;#f

(add '(polynomial x (2 2)) '(polynomial x (2 3))) ;'(polynomial x (2 5))
(add '(polynomial x (2 0) (1 1) (0 6)) '(polynomial x (2 0) (1 1) (0 6))) ;'(polynomial x (1 2) (0 12))