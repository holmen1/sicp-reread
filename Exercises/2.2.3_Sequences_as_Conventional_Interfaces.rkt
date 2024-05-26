#lang racket

(define (square x)
  (* x x))

(define tree (list 1 (list 2 (list 3 4) 5) (list 6 7))) ;'(1 (2 (3 4) 5) (6 7))

;; original
(define (sum-odd-squares-o tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares-o (car tree))
                 (sum-odd-squares-o (cdr tree))))))

(sum-odd-squares-o tree) ;84


(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

;; original
(define (even-fibs-o n)
  (define (next k)
    (if (> k n)
        null
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(even-fibs-o 10) ;'(0 2 8 34)


;; Modularize

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5)) ;'(1 3 5)


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5)) ;15


(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7) ;'(2 3 4 5 6 7)


(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5)) ;'(1 2 3 4 5)


(define (sum-odd-squares tree)
  (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

(sum-odd-squares tree) ;84


(define (even-fibs n)
  (accumulate cons null (filter even? (map fib (enumerate-interval 0 n)))))

(even-fibs 10) ;'(0 2 8 34)

#|Exercise 2.33
Fill in the missing expressions to complete
the following definitions of some basic list-manipulation
operations as accumulations:|#
(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(my-map square (list 1 2 3 4 5)) ;'(1 4 9 16 25)


(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(my-append (list 1 2 3) (list 4 5 6)) ;'(1 2 3 4 5 6)


(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(my-length (list 1 2 3 (list 4 5))) ;4


#|Exercise 2.34
Evaluating a polynomial in x at a given value of x can be formulated as an accumulation. We evaluate the polynomial
anxn + an 1xn 1 +    + a1x + a0

using a well-known algorithm called Horner’s rule, which structures the computation as
(. . . (anx + an 1)x +    + a1)x + a0|#

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;;For example, to compute 1+3x +5x3 +x5 at x = 2 you would evaluate
(horner-eval 2 (list 1 3 0 5 0 1)) ;79


#|Exercise 2.35
Redefine count-leaves from Section 2.2.2 as an accumulation|#
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

(count-leaves '(1 (2 (3 4) 5) (6 7))) ;7
