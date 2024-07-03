#lang racket

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2 . args)
  (define (inner-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
            (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (iter a rest)
    (cond ((null? rest) a)
          ((null? (cdr rest)) (inner-sum a (car rest)))
          (else (inner-sum a (iter (car rest) (cdr rest))))))
  (inner-sum a1 (iter a2 args)))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (make-product m1 m2 . args)
  (define (inner-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (iter a rest)
    (cond ((null? rest) a)
          ((null? (cdr rest)) (inner-product a (car rest)))
          (else (inner-product a (iter (car rest) (cdr rest))))))
  (inner-product m1 (iter m2 args)))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponentiation b e)
  (cond ((=number? b 0) 0)
        ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
          (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
        ((exponentiation? exp)
          (make-product
            (make-product (exponent exp)
                          (make-exponentiation (base exp) (- (exponent exp) 1)))
            (deriv (base exp) var)))
        (else
          (error "unknown expression type: DERIV" exp))))

;test
(deriv '(+ x 3) 'x)                     ;1
(deriv '(* x y) 'x)                     ;'y
(deriv '(* (* x y) (+ x 3)) 'x)         ;'(+ (* x y) (* y (+ x 3)))


#|Exercise 2.56
Show how to extend the basic differentiator to handle more kinds of expressions.
For instance, implement the differentiation rule
d (u ** n) / dx = n u ** (n - 1) du / dx
by adding a new clause to the deriv program and defining appropriate procedures
exponentiation?, base, exponent, and make-exponentiation.
(You may use the symbol ** to denote exponentiation.) Build in the rules that anything
raised to the power 0 is 1 and anything raised to the power 1 is the thing itself.|#

;test
(deriv '(** x 4) 'x)                    ;'(* 4 (** x 3))
(deriv '(** (* 2 (+ 2 (** x 2))) 2) 'x) ;'(* (* 2 (* 2 (+ 2 (** x 2)))) (* 2 (* 2 x)))


#|Exercise 2.57
Extend the differentiation program to handle sums and products of arbitrary numbers of (two or more) terms.
Then the last example above could be expressed as
(deriv '(* x y (+ x 3)) 'x)|#

;test
(deriv '(+ x y (+ x 3)) 'x) ;2
(deriv '(* x y (+ x 3)) 'x) ;'(+ (* x y) (* y (+ x 3)))


#|Exercise 2.58
Suppose we want to modify the differentiation program so that it works with ordinary mathematical
notation, in which + and * are infix rather than prefix operators.
Since the differentiation program is defined in terms of abstract data, we can modify it to work with
different representations of expressions solely by changing the predicates, selectors, and constructors
that define the representation of the algebraic expressions on which the differentiator is to operate.

a. Show how to do this in order to differentiate algebraic expressions presented in infix form, such as
(x + (3 * (x + (y + 2)))).
To simplify the task, assume that + and * always take two arguments and that expressions are fully parenthesized|#

(define (infix-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))
(define (i-sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define (i-addend s) (car s))
(define (i-augend s) (caddr s))


(define (infix-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))
(define (i-product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (i-multiplier p) (car p))
(define (i-multiplicand p) (caddr p))


(define (i-deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((i-sum? exp) (infix-sum (i-deriv (i-addend exp) var)
                                 (i-deriv (i-augend exp) var)))
        ((i-product? exp)
          (infix-sum
            (infix-product (i-multiplier exp)
                           (i-deriv (i-multiplicand exp) var))
            (infix-product (i-deriv (i-multiplier exp) var)
                           (i-multiplicand exp))))
        (else
          (error "unknown expression type: I-DERIV" exp))))

;test
(i-deriv '(x + (3 * (x + (y + 2)))) 'x) ;4