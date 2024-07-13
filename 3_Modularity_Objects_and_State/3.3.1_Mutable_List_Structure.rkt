#lang sicp

#|Exercise 3.12|#

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z       ; => (a b c d)
(cdr x) ; => (b)

(define w (append! x y))
w       ; => (a b c d)
(cdr x) ; => (b c d)



#|Exercise 3.13
Consider the following make-cycle procedure, which uses the last-pair procedure defined in Exer-cise 3.12|#

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;;Draw a box-and-pointer diagram that shows the structure c created by
(define c (make-cycle (list 'a 'b 'c)))

;(last-pair c) ; => infinite loop...


#|Exercise 3.14
The following procedure is quite useful, although obscure.

loop uses the “temporary” variable temp to hold the old value of the cdr of x, since the set-cdr!
on the next line destroys the cdr. Explain what mystery does in general.

Suppose v is defined by (define v (list 'a 'b 'c 'd)). Draw the box-and-pointer diagram that represents
the list to which v is bound. Suppose that we now evaluate (define w (mystery v)).
Draw box-and-pointer diagrams that show the structures v and w after evaluating this expression.
What would be printed as the values of v and w?|#

(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (cdr x)))
        (set-cdr! x y)
        (loop temp x))))
  (loop x '()))

(define u (list 'a 'b 'c 'd))
(define v (mystery u))
u ; => (a)
v ; => (d c b a)