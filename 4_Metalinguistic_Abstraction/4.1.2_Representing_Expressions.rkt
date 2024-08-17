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


