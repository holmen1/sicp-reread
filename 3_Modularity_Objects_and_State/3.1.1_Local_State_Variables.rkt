#lang racket


#|Exercise 3.1: An accumulator is a procedure that is called repeatedly with a single numeric argument
and accumulates its arguments into a sum. Each time it is called, it returns the currently accumulated sum.
Write a procedure make-accumulator that generates accumulators, each maintaining an independent sum|#
    

(define (make-accumulator init)
    (lambda (incr)
            (begin (set! init (+ init incr))
                   init)))

;test
(define A (make-accumulator 5))
(A 10) ;15
(A 10) ;25


#|Exercise 3.2
In sofware-testing applications, it is useful to be able to count the number of times a given procedure
is called during the course of a computation. Write a procedure make-monitored that takes as input a
procedure, f, that itself takes one input. The result returned by make-monitored is a third procedure,
say mf, that keeps track of the number of times it has been called by maintaining an internal counter.
If the input to mf is the special symbol how-many-calls?, then mf returns the value of the counter.
If the input is the special symbol reset-count, then mf resets the counter to zero. For any other input,
mf returns the result of calling f on that input and increments the counter|#

(define (make-monitored f)
  (let ((counter 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) counter)
            ((eq? x 'reset-count) (set! counter 0))
            (else (set! counter (+ counter 1))
                  (f x))))))

;test
(define s (make-monitored sqrt))
(s 100) ;=> 10
(s 'how-many-calls?) ;=> 10
(s 9) ;=> 3
(s 'how-many-calls?) ;=> 2
(s 'reset-count)
(s 100) ;=> 10
(s 'how-many-calls?) ;=> 1

