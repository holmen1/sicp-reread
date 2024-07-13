#lang racket

(define random-init 905)

(define (rand-update x)
  (let ((a 16807)
        (m 2147483647))
    (modulo (* a x) m)))

(define rand (let ((x random-init))
  (lambda () (set! x (rand-update x))
             x)))

;test rand
(rand) ;=> 15210335
(rand) ;=> 89546352


;;;Monte Carlo
;Approximate π using the fact that 6/π 2 is the probability that two integers chosen at random will
;have no factors in common; that is, that their greatest common divisor will be 1

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1)
                              (+ trials-passed 1)))
          (else (iter (- trials-remaining 1)
                      trials-passed))))
  (iter trials 0))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
  
(estimate-pi 1000000) ;=> 3.140232108785684


#|Exercise 3.5
Monte Carlo integration is a method of estimating definite integrals by means of Monte Carlo simulation.
Consider computing the area of a region of space described by a predicate P (x , y) that is true for
points (x , y) in the region and false for points not in the region.
To estimate the area of the region described by such a predicate, begin by choosing a rectangle that
contains the region. The desired integral is the area of that portion of the rectangle that lies in
the region. We can estimate the integral by picking, at random, points (x , y) that lie in the rectangle,
and testing P (x , y) for each point to determine whether the point lies in the region. If we try this
with many points, then the fraction of points that fall in the region should give an estimate of the
proportion of the rectangle that lies in the region. Hence, multiplying this fraction by the area of
the entire rectangle should produce an estimate of the integral.

Implement Monte Carlo integration as a procedure estimate-integral that takes as arguments a predicate P,
upper and lower bounds x1, x2, y1, and y2 for the rectangle, and the number of trials to perform in order
to produce the estimate. Your procedure should use the same monte-carlo procedure that was used above
to estimate π.
Use your estimate-integral to produce an estimate of π by measuring the area of a unit circle|#

(random-seed 905)

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* range (random)))))

;; x, y inside unit circle?
(define (P x y)
  (<= (+ (* x x) (* y y)) 1))

(define (estimate-integral predicate x1 x2 y1 y2 trials)
  (let ((test (lambda ()
                (predicate (random-in-range x1 x2) (random-in-range y1 y2)))))
    (* (exact->inexact (monte-carlo trials test))
       (- x2 x1)
       (- y2 y1))))

;test
(estimate-integral P -1 1 -1 1 1000000) ;=> 3.147216


;; Monte Carlo integration

(define (below-sin x y)
  (<= y (sin x)))

(estimate-integral below-sin 0 pi 0 1 10000000) ; => 2.000820670810921
