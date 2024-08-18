#lang racket

(require "evaluator.rkt")

;the-global-environment
(define e0 (setup-environment))


#|Exercise 4.4
Recall the definitions of the special forms and and or from Chapter 1:
• and: The expressions are evaluated from left to right.
If any expression evaluates to false, false is returned; any remaining expressions are not evaluated.
If all the expressions evaluate to true values, the value of the last expression is returned. If there
are no expressions then true is returned.
• or: The expressions are evaluated from left to right.
If any expression evaluates to a true value, that value is returned; any remaining expressions are not
evaluated. If all expressions evaluate to false, or if there are no expressions, then false is returned.

Install and and or as new special forms for the evaluator by defining appropriate syntax procedures and
evaluation procedures eval-and and eval-or|#

;test
(eval '(and true true true) e0) ;=> #t
(eval '(and true true false) e0) ;=> #f
(eval '(or false true true) e0) ;=> #t
(eval '(or false false) e0) ;=> #f
(eval '(or) e0) ;=> #f
(eval '(and) e0) ;=> #t


#|Exercise 4.6
let expressions are derived expressions, because

(let ((⟨var1⟩ ⟨exp1⟩) . . . (⟨varn⟩ ⟨expn⟩))
  ⟨body⟩)

is equivalent to

((lambda (⟨var1⟩ . . . ⟨varn⟩)
    ⟨body⟩)
  ⟨exp1⟩
  . . .
  ⟨expn⟩)

Implement a syntactic transformation let->combination that reduces evaluating let expressions to evaluating
combinations of the type shown above, and add the appropriate clause to eval to handle let expressions|#

;test
(eval '(define (add x y)
         (let ((a x) (b y)) (+ a b))) e0) ;=> ok
(eval '(add 1 2) e0) ;=> 3


#|Exercise 4.8
“Named let” is a variant of let that has the form (let ⟨var⟩ ⟨bindings⟩ ⟨body⟩)
The ⟨bindings⟩ and ⟨body⟩ are just as in ordinary let, except that ⟨var⟩ is bound within ⟨body⟩
to a procedure whose body is ⟨body⟩ and whose parameters are the variables in the ⟨bindings⟩.
Thus, one can repeatedly execute the ⟨body⟩ by invoking the procedure named ⟨var⟩. For example,
the iterative Fibonacci procedure (Section 1.2.2) can be rewritten using named let as follows:

(define (fib n)
  (let fib-iter ((a 1) (b 0) (count n))
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1)))))

Modify let->combination of Exercise 4.6 to also support named let|#


;(let fib-iter ((a 1) (b 0) (count n)) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))
;(begin (define fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter 1 0 n))

;test
(eval '(define (fib n)
        (let fib-iter ((a 1) (b 0) (count n))
            (if (= count 0)
                b
                (fib-iter (+ a b) a (- count 1))))) e0) ;=> ok
(eval '(fib 6) e0) ;=> 8


;; Original Scheme, showing both versions:
(define fib
  (lambda (n)
    (begin (define fib-iter 
             (lambda (a b count)
               (if (= count 0)
                   b
                   (fib-iter (+ a b) a (- count 1)))))
           (fib-iter 1 0 n))))

(define (fib-derived n)
  (let fib-iter ((a 1) (b 0) (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))

(fib 120)           ;5358359254990966640871840
(fib-derived 120)   ;5358359254990966640871840


#|Exercise 4.9
Many languages support a variety of iteration constructs, such as do, for, while, and until.
In Scheme, iterative processes can be expressed in terms of ordinary procedure calls,
so special iteration constructs provide no essential gain in computational power.
On the other hand, such constructs are often convenient. Design some iteration constructs,
give examples of their use, and show how to implement them as derived expressions|#

;test
; (eval '(define items (cons 57 (cons 321 (cons 88 '())))) e0)
; (eval '(for (lambda (x)
;               (newline)
;               (display x))
;             items)
;         e0)


