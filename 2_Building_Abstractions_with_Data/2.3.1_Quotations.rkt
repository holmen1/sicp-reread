#lang racket


(define a 1)
(define b 2)

(list a b)      ;'(1 2)
(list 'a 'b)    ;'(a b)
(list 'a b)     ;'(a 2)
(car '(a b c))  ;'a
(cdr '(a b c))  ;'(b c)


;;  eq?
(eq? 2 2)               ;#t
(eq? 'a 'a)             ;#t
(eq? '(a b c) '(a b c)) ;#f

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))            ;#f
(memq 'apple '(x (apple sauce) y apple pear)) ;'(apple pear)
(memq 'c '(a b c d e f))                      ;'(c d e f)


#|Exercise 2.53
What would the interpreter print in response to evaluating each of the following expressions?|#

(list 'a 'b 'c)                         ;'(a b c)
(list (list 'george))                   ;'((george))
(cdr '((x1 x2) (y1 y2)))                ;'((y1 y2))
(cadr '((x1 x2) (y1 y2)))               ;'(y1 y2)
(pair? (car '(a short list)))           ;#f
(memq 'red '((red shoes) (blue socks))) ;#f
(memq 'red '(red shoes blue socks))     ;'(red shoes blue socks)


#|Exercise 2.54
Two lists are said to be equal? if they contain equal elements arranged in the same order.
For example,
(equal? '(this is a list) '(this is a list)) is true, but
(equal? '(this is a list) '(this (is a) list)) is false.

To be more precise, we can define equal? recursively in terms of the basic eq? equality of
symbols by saying that a and b are equal? if they are both symbols and the symbols are eq?,
or if they are both lists such that (car a) is equal? to (car b) and (cdr a) is equal? to (cdr b).
Using this idea, implement equal? as a procedure|#

(define (equal? l1 l2)
  (cond ((eq? l1 l2) #t)
        ((or (null? l1) (null? l2)) #f)
        ((equal? (cdr l1) (cdr l2)) #t)
        (else #f)))

;test
(equal? '(this is a list) '(this is a list))      ;#t
(equal? '(this is a list) '(this (is a) list))    ;#f
(equal? '(this (is a) list) '(this (is a) list))  ;#t


#|Exercise 2.55
Eva Lu Ator types to the interpreter the expression
(car ''abracadabra)
To her surprise, the interpreter prints back quote. Explain|#

(car ''abracadabra)             ;'quote
(car '(my-quote 'abracadabra))  ;'my-quote


