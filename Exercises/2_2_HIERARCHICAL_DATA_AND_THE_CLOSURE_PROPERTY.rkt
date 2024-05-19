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


#|Exercise 2.25|#
; pick 7

;'(1 3 (5 7) 9)
;'((7))
;'(1 (2 (3 (4 (5 (6 7))))))

(define l1 (list 1 3 (list 5 7) 9))
(define l2 (list (list 7)))
(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr (cdr l1))))) ;7
(car (car l2)) ;7
(car (cdr (cadr (cadr (cadr (cadr (cadr l3))))))) ;7


#| Exercise 2.27
Modify your reverse procedure of Exercise
2.18 to produce a deep-reverse procedure that takes a list
as argument and returns as its value the list with its ele-
ments reversed and with all sublists deep-reversed as well.
For example,
(define x (list (list 1 2) (list 3 4)))
x
((1 2) (3 4))
(reverse x)
((3 4) (1 2))
(deep-reverse x)
((4 3) (2 1))
|#

;; Here's reverse for reference:
(define (reverse2 items)
  (define (rev-imp items result)
    (if (null? items)
        result
        (rev-imp (cdr items) (cons (car items) result))))
  (rev-imp items null))


(define (deep-reverse items)
  (define (iter l res)
    (cond ((null? l) res)
          ((pair? (car l))
           (iter (cdr l)
                 (cons (deep-reverse (car l))
                       res)))
          (else (iter (cdr l)
                      (cons (car l)
                            res)))))
  (iter items null)
  )

(define y (list (list 1 2) (list 3 4))) ; '((1 2) (3 4))

(reverse y) ;'((3 4) (1 2))
(deep-reverse y) ;'((4 3) (2 1))

#| Exercise 2.28
Write a procedure fringe that takes as argument a tree (represented as a list)
and returns a list whose elements are all the leaves of the tree arranged in
left-to-right order. For example,

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(1 2 3 4)
(fringe (list x x))
(1 2 3 4 1 2 3 4)
|#

(define (fringe tree)
  (define (cata items acc)
    (cond ((null? items) acc)
          ((not (pair? items)) (cons items acc))
          (else (cata (car items)
                      (cata (cdr items) acc)))))
  (cata tree null))


;test
(define z (list (list 1 2) (list 3 4)))
(fringe z) ;'(1 2 3 4)
(fringe (list z z)) ;'(1 2 3 4 1 2 3 4)


#| Exercise 2.29
A binary mobile consists of two branches, a left branch and a right branch.
Each branch is a rod of a certain length, from which hangs either a weight
or another binary mobile. We can represent a binary mobile using compound data
by constructing it from two branches (for example, using list):

(define (make-mobile left right)
  (list left right))

A branch is constructed from a length (which must be a number) together with a structure,
which may be either a number (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

a. Write the corresponding selectors left-branch and right-branch, which return the branches of a mobile,
and branch-length and branch-structure, which return the components of a branch.

b. Using your selectors, define a procedure total-weight that returns the total weight of a mobile.

c. A mobile is said to be balanced if the torque applied by its top-left branch is equal to that
applied by its top-right branch (that is, if the length of the left rod multiplied by the weight
hanging from that rod is equal to the corresponding product for the right side) and if each of the
submobiles hanging off its branches is balanced. Design a predicate that tests whether a binary
mobile is balanced.

d. Suppose we change the representation of mobiles so that the constructors are

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

How much do you need to change your programs to convert to the new representation?
|#

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (cond ((null? mobile) 0) 
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

;test
(define a (make-mobile (make-branch 2 3) (make-branch 2 3))) 
(total-weight a)

(define b (make-branch 2 1))
(define c (make-branch 2 b))
(define m (make-mobile a c))

;(total-weight m) ;3

(define (torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch)))) 

(define (balanced? mobile)
  (if (not (pair? mobile)) 
    true 
    (and (= (torque (left-branch mobile)) (torque (right-branch mobile))) 
            (balanced? (branch-structure (left-branch mobile))) 
            (balanced? (branch-structure (right-branch mobile)))))) 




;test

  
 (define d (make-mobile (make-branch 10 a) (make-branch 12 5))) 
 
  
 (balanced? d) 
    