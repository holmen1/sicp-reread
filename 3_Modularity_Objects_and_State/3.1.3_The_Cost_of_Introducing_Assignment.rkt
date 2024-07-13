#lang racket

(define (make-account password balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (add-pwd pwd)
    (set! password (cons pwd
                         password)))
  (define (authenticate pwd)
    (if (not (pair? password))
      (eq? pwd password) 
      (or (eq? pwd (car password))
          (eq? pwd (cdr password)))))
  (define (dispatch p m)
    (if (authenticate p)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'add-pwd) add-pwd)
            (else (error "Unknown request: MAKE-ACCOUNT" m)))
      (lambda (dummy) "Incorrect password")))
  dispatch)

#|Exercise 3.7
Consider the bank account objects created by make-account, with the password modification described
in Exercise 3.3. Suppose that our banking system requires the ability to make joint accounts. Define
a procedure make-joint that accomplishes this. make-joint should take three arguments. The first is a
password-protected account. The second argument must match the password with which the account was defined
in order for the make-joint operation to proceed. The third argument is a new password. make-joint is
to create an additional access to the original account using the new password. For example, if peter-acc
is a bank account with password open-sesame, then

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

will allow one to make transactions on peter-acc using the
name paul-acc and the password rosebud|#

(define (make-joint acc pwd new-pwd)
  ((acc pwd 'add-pwd) new-pwd)
  acc)

;test
(define peter-acc (make-account 'open-sesame 100))
((peter-acc 'open-sesame 'withdraw) 10) ;=> 90

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((paul-acc 'rosebud 'withdraw) 10) ;=> 80
((peter-acc 'open-sesame 'withdraw) 10) ;=> 70
((peter-acc 'sesame 'withdraw) 10) ;=> "Incorrect password"


#|Exercise 3.8
When we defined the evaluation model in Section 1.1.3, we said that the first step in evaluating an
expression is to evaluate its subexpressions. But we never specified the order in which the sub-
expressions should be evaluated (e.g., left to right or right to left). When we introduce assignment,
the order in which the arguments to a procedure are evaluated can make a difference to the result.
Define a simple procedure f such that evaluating

(+ (f 0) (f 1))

will return 0 if the arguments to + are evaluated from left to right but will return 1 if the arguments
are evaluated from right to left|#

(define f
  (let ((init 1))
    (lambda (incr)
            (begin (set! init (* init incr))
                   init))))

(define f2
  (let ((init 1))
    (lambda (incr)
            (begin (set! init (* init incr))
                   init))))


(+ (f 0) (f 1)) ; => 0, revealing this racket + evaluates left to right
(+ (f2 1) (f2 0)) ; => 1