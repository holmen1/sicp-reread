#lang racket

(require "polynomial.rkt")
(require "arithmetic_operations.rkt")
;(require "utils.rkt") ; for debug


(install-polynomial-package)

; (for-each (lambda (pair)
;             (printf "~a\n" pair))
;           (hash->list operations))

(make-polynomial 'x '((2 0))) ;'(polynomial x (2 0))

(=zero? (make-polynomial 'x '())) ;#t
(=zero? (make-polynomial 'x '((2 0)))) ;#t
(=zero? (make-polynomial 'x '((2 1) (1 0)))) ;#f