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

(define sinus (make-monitored sin))
(sinus (* (/ pi 4) (sinus 'how-many-calls?))) ;=> 0
(sinus (* (/ pi 4) (sinus 'how-many-calls?))) ;=> 0.7071067811865475
(sinus (* (/ pi 4) (sinus 'how-many-calls?))) ;=> 1.0
(sinus (* (/ pi 4) (sinus 'how-many-calls?))) ;=> 0.7071067811865476
(sinus (* (/ pi 4) (sinus 'how-many-calls?))) ;=> 1.2246467991473532e-16
(sinus (* (/ pi 4) (sinus 'how-many-calls?))) ;=> -0.7071067811865475
(sinus (* (/ pi 4) (sinus 'how-many-calls?))) ;=> -1.0
(sinus (* (/ pi 4) (sinus 'how-many-calls?))) ;=> -0.7071067811865477
(sinus (* (/ pi 4) (sinus 'how-many-calls?))) ;=> -2.4492935982947064e-16



#|Exercise 3.3
Modify the make-account procedure so that it creates password-protected accounts. That is, make-account
should take a symbol as an additional argument, as in (define acc (make-account 100 'secret-password))

The resulting account object should process a request only if it is accompanied by the password with
which the account was created, and should otherwise return a complaint|#

(define (make-account password balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (eq? p password)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request: MAKE-ACCOUNT" m)))
      (lambda (dummy) "Incorrect password")))
  dispatch)

;test
(define acc (make-account 'secret-password 100))

((acc 'secret-password 'withdraw) 40) ;=> 60
((acc 'some-other-password 'deposit) 50) ;=> "Incorrect password"
((acc 'secret-password 'withdraw) 70) ;=> "Insufficient funds"
((acc 'secret-password 'deposit) 60) ;=> 120
((acc 'secret-password 'withdraw) 120) ;=> 0
