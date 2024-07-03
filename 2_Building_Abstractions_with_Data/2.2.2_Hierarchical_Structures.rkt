#lang racket

#|As an example, compare the
length procedure of Section 2.2.1 with the count-leaves procedure,
which returns the total number of leaves of a tree:|#

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


#|Exercise 2.25
Give combinations of cars and cdrs that
will pick 7 from each of the following lists:
(1 3 (5 7) 9)
((7))
(1 (2 (3 (4 (5 (6 7))))))|#

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
  (iter items null))

(define y (list (list 1 2) (list 3 4))) ; '((1 2) (3 4))

(reverse y) ;'((3 4) (1 2))
(deep-reverse y) ;'((4 3) (2 1))



#| Exercise 2.28
Write a procedure fringe that takes as argument a tree (represented as a list)
and returns a list whose elements are all the leaves of the tree arranged in
left-to-right order. For example

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(1 2 3 4)
(fringe (list x x))
(1 2 3 4 1 2 3 4)|#

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
(total-weight a) ;6

;test performance
(define (test-performance mobile)
  (display "Performance of total-weight: ")
  (time (total-weight mobile)))

(define (make-large-mobile size)
  (if (= size 0)
      1
      (make-mobile (make-branch size (make-large-mobile (- size 1)))
                   (make-branch size (make-large-mobile (- size 1))))))

(define large-mobile (make-large-mobile 20))

(test-performance large-mobile)
;;Performance of total-weight: cpu time: 96 real time: 102 gc time: 27
;;1048576


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
(balanced? d) ;#t

;;Mapping over trees

(define (scale-tree tree factor)
  (cond ((null? tree) null)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define t (list 1 (list 2 (list 3 4) 5) (list 6 7))) ;'(1 (2 (3 4) 5) (6 7))

(scale-tree t 10) ;(10 (20 (30 40) 50) (60 70))


;; Using higher-order procedure
(define (scale-tree-m tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-m sub-tree factor)
             (* sub-tree factor)))
       tree))

(scale-tree-m t 10) ;'(10 (20 (30 40) 50) (60 70))

#| Exercise 2.30: Define a procedure square-tree analogous to the square-list procedure of Exercise 2.21 |#

(define (square-tree tree)
    (cond ((null? tree) null)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

t
(square-tree t) ;'(1 (4 (9 16) 25) (36 49))

(define (square-tree-m tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

(square-tree-m t) ;'(1 (4 (9 16) 25) (36 49))


#|Exercise 2.31
Abstract your answer to Exercise 2.30 to produce a procedure tree-map with the property that square-tree could be defined as

(define (square-tree tree) (tree-map square tree))|#

(define (square x)
  (* x x))

(define (tree-map f tree)
    (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (f sub-tree)))
         tree))

(define (square-tree-a tree) (tree-map square tree))
(square-tree-a t) ;'(1 (4 (9 16) 25) (36 49))

#|Exercise 2.32
We can represent a set as a list of distinct elements, and we can represent the set of all subsets of the set as a list of lists.

For example, if the set is (1 2 3), then the set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)).

Complete the following definition of a procedure that generates the set of subsets of a set and give a clear explanation of why it works:|#

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (ss) (cons (car s) ss))
                     rest)))))
#|
The set of all subsets of a given set is the union of:

    the set of all subsets excluding the first number.
    the set of all subsets excluding the first number, with the first number re-inserted into each subset. |#

(subsets '(1 2 3)) ;'(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
