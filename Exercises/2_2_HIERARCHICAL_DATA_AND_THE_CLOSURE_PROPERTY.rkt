#lang racket


(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))
(list-ref squares 3)


(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items))))
)

(define odds (list 1 3 5 7 9))
(length odds)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

#|
Exercise 2.17: Define a procedure last-pair that returns the list
that contains only the last element of a given (nonempty) list
|#

(define (last-pair list)
  (let ((tail (cdr list)))
  (if (null? tail)
    list
    (last-pair tail))))


(last-pair (list 23 72 149 34))
;(34)

#|
Exercise 2.18:
Define a procedure reverse that takes a list as argument
and returns a list of the same elements in reverse order:
(reverse (list 1 4 9 16 25))
(25 16 9 4 1)
|#

 (define (reverse items) 
   (if (null? (cdr items)) 
       items 
       (append (reverse (cdr items)) 
               (cons (car items) '())))) 

 (reverse (list 1 4 9 16 25))


#|
Exercise 2.20:
(same-parity 1 2 3 4 5 6 7)
'(1 3 5 7)
(same-parity 2 3 4 5 6 7)
'(2 4 6)
|#

(define (same-parity x . args)
  (define parity (if (odd? x) odd? even?))
  (define (iter rest)
    (if (null? rest)
      rest
      (if (parity (car rest))
        (cons (car rest) (iter (cdr rest)))
        (iter (cdr rest)))))
  (cons x (iter args))
)


#|
  Exercise 2.21
  The procedure square-list takes a list of numbers as argument
  and returns a list of the squares of those numbers.

(square-list (list 1 2 3 4))
(1 4 9 16)
|#

(define (square-list-rec items)
  (if (null? items)
  null
  (cons ((lambda (x) (* x x)) (car items))
        (square-list-rec (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

#|Exercise 2.23|#

; (for-each f items)

(define (for-each f items)
  (define (iter l)
    (cond ((null? l) #t)
          (else (f (car l))
                (iter (cdr l)))))
  (iter items))

;test
(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))

;57
;321
;88#t

#|      Trees     |#

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

; test
(define x (cons (list 1 2) (list 3 4)))
(length x) ;3
(count-leaves x) ;4
(list x x) ;(((1 2) 3 4) ((1 2) 3 4))
(length (list x x)) ;2
(count-leaves (list x x)) ;8