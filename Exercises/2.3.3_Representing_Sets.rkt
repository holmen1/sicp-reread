#lang racket


;;; Sets as unordered lists
; One way to represent a set is as a list of its elements in which no element appears more than once.
; The empty set is represented by the empty list.

(define (make-unordered-set . es)
  (if (null? es)
    '()
    es))

(define (element-of-unordered-set? x set)
  (cond ((null? x) #t)
        ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-unordered-set? x (cdr set)))))

;test
(make-unordered-set 1 5 'e1 4)          ;'(1 5 e1 4)
(make-unordered-set)                    ;'()
(element-of-unordered-set? 2 '(1 5 e1 4))         ;#f
(element-of-unordered-set? '(e1 4) '(1 5 (e1 4))) ;#t
(element-of-unordered-set? '() '(1 5 e1 4))       ;#t
(element-of-unordered-set? '() '())               ;#t


(define (adjoin-unordered-set x set)
  (if (element-of-unordered-set? x set)
      set
      (cons x set)))

(define (intersection-unordered-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-unordered-set? (car set1) set2)
          (cons (car set1) (intersection-unordered-set (cdr set1) set2)))
        (else (intersection-unordered-set (cdr set1) set2))))


#|Exercise 2.59
Implement the union-unordered-set operation for the unordered-list representation of sets|#

(define (union-unordered-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-unordered-set? (car set1) set2)
          (union-unordered-set (cdr set1) set2))
        (else (union-unordered-set (cdr set1) (adjoin-unordered-set (car set1) set2)))))

(union-unordered-set '(5 4 3) '(1 2 3)) ;'(4 5 1 2 3)
(union-unordered-set '(3 2 1) '(1 2 3)) ;'(1 2 3)
(union-unordered-set '(5 4 3) '())      ;'(5 4 3)
(union-unordered-set '() '(1 2 3))      ;'(1 2 3)


;;; Sets as binary trees
; We can represent trees by using lists. Each node will be a list of three items:
; the entry at the node, the left subtree, and the right subtree.
; A left or a right subtree of the empty list will indicate that there is no subtree connected there.

(define (make-tree entry left right)
  (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
          (element-of-set? x (left-branch set)))
        ((> x (entry set))
          (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
          (make-tree (entry set)
                     (adjoin-set x (left-branch set))
                     (right-branch set)))
        ((> x (entry set))
          (make-tree (entry set)
                     (left-branch set)
                     (adjoin-set x (right-branch set))))))

;Converts tree to ordered list
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

;Converts an ordered list to a balanced binary tree
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry
                               left-tree
                               right-tree)
                    remaining-elts))))))))

;test
(define u-tree (foldl (lambda (x tree) (adjoin-set x tree))
                 '()
                 '(1 3 5 7 9 11)))
u-tree ;'(1 () (3 () (5 () (7 () (9 () (11 () ()))))))

(define o-list (tree->list u-tree))
o-list  ;'(1 3 5 7 9 11)

(define b-tree (list->tree o-list))
b-tree  ;'(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
;                 (5
;     (1                        (9 
; ()    (3 () ())      (7 () ())  (11 () ())

