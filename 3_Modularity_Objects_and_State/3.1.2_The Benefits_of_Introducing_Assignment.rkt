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

(estimate-pi 10000000) ;=> 3.141708938373916
pi