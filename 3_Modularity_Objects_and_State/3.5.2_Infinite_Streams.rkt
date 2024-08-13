#lang racket

(require "streams.rkt")

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (show x)
  (display-line x)
  x)

(define x
  (stream-map show
              (integers-starting-from 0)))

(stream-ref x 5)
; 0
; 1
; 2
; 3
; 4
; 55


(define integers (integers-starting-from 1))
;; Using integers we can define other infinite streams, such as the stream of integers that are not divisible by 7:

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

;; Then we can find integers not divisible by 7 simply by accessing elements of this stream:
(stream-ref no-sevens 100) ; => 117
(stream-ref no-sevens 101) ; => 118
(stream-ref no-sevens 102) ; => 120


;; In analogy with integers, we can define the infinite stream of Fibonacci numbers:

(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

;; fibs is a pair whose car is 0 and whose cdr is a promise to evaluate (fibgen 1 1).
;; When we evaluate this delayed (fibgen 1 1), it will produce a pair whose car is 1 and whose cdr is
;; a promise to evaluate (fibgen 1 2), and so on

(stream-take fibs 7) ; => '(0 1 1 2 3 5 8)


;; Sieve of Eratosthenes

(define (sieve stream)
  (cons-stream (stream-car stream)
               (sieve (stream-filter
                        (lambda (x) (not (divisible? x (stream-car stream))))
                        (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(stream-take primes 10) ; => '(2 3 5 7 11 13 17 19 23 29)
(stream-ref primes 50) ; => 233


;; Defining streams implicitly

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define integers-i
  (cons-stream 1 (add-streams ones integers-i)))
(stream-take integers-i 10) ; => '(1 2 3 4 5 6 7 8 9 10)


(define fibs-i
  (cons-stream
    0
    (cons-stream 1 (add-streams (stream-cdr fibs-i) fibs-i))))
(stream-take fibs-i 10) ; => '(0 1 1 2 3 5 8 13 21 34)


(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define double (cons-stream 1 (scale-stream double 2)))
(stream-take double 6) ; => '(1 2 4 8 16 32)


;; Primes implicit

(define primes-i
  (cons-stream
  2
  (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes-i))

(define (square x) (* x x))

(stream-take primes-i 10) ; => '(2 3 5 7 11 13 17 19 23 29)
(stream-ref primes-i 50) ; => 233


#|Exercise 3.54
Define a procedure mul-streams, analogous to add-streams, that produces the elementwise product of
its two input streams. Use this together with the stream of integers to complete the following
definition of the stream whose nth element (counting from 0) is n + 1 factorial:
|#

(define (mul-streams s1 s2) (stream-map * s1 s2))

(define factorials
  (cons-stream 1
               (mul-streams factorials
                            integers)))

;; test
(stream-take factorials 6); => '(1 1 2 6 24 120)



#|Exercise 3.55
Define a procedure partial-sums that takes as argument a stream S and returns the stream whose elements
are S0, S0 + S1, S0 + S1 + S2 , . . ..
For example, (partial-sums integers) should be the stream 1, 3, 6, 10, 15, . . .
|#

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))

;; test
(stream-take (partial-sums factorials) 5); => '(1 2 4 10 34)



#|Exercise 3.56
A famous problem, first raised by R. Hamming, is to enumerate, in ascending order with no repetitions,
all positive integers with no prime factors other than 2, 3, or 5. One obvious way to do this is to
simply test each integer in turn to see whether it has any factors other than 2, 3, and 5. But this is
very inefficient, since, as the integers get larger, fewer and fewer of them fit the requirement. As an
alternative, let us call the required stream of numbers S and notice the following facts about it.

• S begins with 1
• The elements of (scale-stream S 2) are also elements of S
• The same is true for (scale-stream S 3) and (scalestream 5 S)
• These are all the elements of S

Now all we have to do is combine elements from these sources. For this we define a procedure merge that
combines two ordered streams into one ordered result stream, eliminating repetitions:|#

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car) (cons-stream s1car
                                                (merge (stream-cdr s1)
                                                       s2)))
                  ((> s1car s2car) (cons-stream s2car
                                                (merge s1
                                                       (stream-cdr s2))))
                  (else (cons-stream s1car
                                     (merge (stream-cdr s1)
                                            (stream-cdr s2)))))))))

;; en the required stream may be constructed with merge, as follows:
(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))

;; test
(stream-take S 12); => '(1 2 3 4 5 6 8 9 10 12 15 16)



#|Exercise 3.59
In Section 2.5.3 we saw how to implement a polynomial arithmetic system representing polynomials
as lists of terms. In a similar way, we can work with powerseries represented as infinite streams.
We will represent the series a0 + a1x + a2x2 + a3x3 + . . . as the stream whose elements are the
coefficients a0, a1, a2, a3, . . .

a. The integral of the series a0 + a1x + a2x2 + a3x3 + . . . is the series c + a0x + 1/2a1x2 + 1/3a2x3 + 1/4a3x4 + . . . ,
where c is any constant. Define a procedure integrate-series that takes as input a stream a0, a1, a2, . . .
representing a power series and returns the stream a0, 1/2 a1, 1/3 a2, . . . of coefficients of the
non-constant terms of the integral of the series. (Since the result has no constant term, it doesn’t
represent a power series; when we use integrate-series, we will cons on the appropriate constant.)|#

(define (integrate-series S)
  (stream-map / S
                integers))

;; test
(stream-take
  (integrate-series (integers-starting-from 10))
  6)
; => '(10 11/2 4 13/4 14/5 5/2)

#|b. The function x -> e^x is its own derivative. This implies that e^x and the integral of e^x are the
same series, except for the constant term, which is e0 = 1. Accordingly, we can generate the series for
e^x as |#
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

;; Show how to generate the series for sine and cosine, starting from the facts that the derivative of sine is cosine and the derivative of cosine is the negative of sine:

(define cosine-series (cons-stream 1 (integrate-series sine-series)))
(define sine-series (cons-stream 0 (scale-stream (integrate-series cosine-series)
                                                 -1)))