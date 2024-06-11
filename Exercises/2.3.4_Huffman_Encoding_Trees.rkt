#lang racket


#| The algorithm for generating a Huffman tree is very simple. The idea is to arrange the tree
so that the symbols with the lowest frequency appear farthest away from the root. Begin with the set
of leaf nodes, containing symbols and their frequencies, as determined by the initial data from which
the code is to be constructed. Now find two leaves with the lowest weights and merge them to produce
a node that has these two nodes as its left and right branches.
The weight of the new node is the sum of the two weights. Remove the two leaves from the original
set and replace them by this new node. Now continue this process. At each step, merge two nodes with
the smallest weights, removing them from the set and replacing them with a node that has these two
as its left and right branches. The process stops when there is only one node left, which is the root
of the entire tree.  |#

;;; Representing Huffman trees

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))

;; Selectors
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))

(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))

;test
(make-code-tree (make-leaf 'D 1) (make-leaf 'C 1))
; '((leaf D 1) (leaf C 1) (D C) 2)
(symbols '((leaf D 1) (leaf C 1) (D C) 2)) ;'(D C)
(left-branch '((leaf D 1) (leaf C 1) (D C) 2)) ;'(leaf D 1)
(left-branch (left-branch '((leaf D 1) (leaf C 1) (D C) 2)))


;; The decoding procedure
(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits)
            '()
            (let ((next-branch (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch)
                          (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))

(define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit: CHOOSE-BRANCH" bit))))


;; Sets of weighted elements
;In our representation of trees, each non-leaf node contains a set of symbols, which we have represented as a simple list

(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set)
    (adjoin-set x (cdr set))))))

#| ;; Ordered leaf set
;The following procedure takes a list of symbol-frequency pairs and constructs an initial ordered
;set of leaves, ready to be merged according to the Huffman algorithm
(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair) ; symbol
                                   (cadr pair)) ; frequency
                        (make-leaf-set (cdr pairs))))))
;test
(make-leaf-set '((A 4) (B 2) (C 1) (D 1))) 
;'((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4)) |#


#|Exercise 2.67
Define an encoding tree and a sample message.
Use the decode procedure to decode the message, and give the result|#

(define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree (make-leaf 'B 2)
                                    (make-code-tree (make-leaf 'D 1)
                                                    (make-leaf 'C 1)))))
; '((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree) ;'(A D A B B C A)

;                 (A B D C) 8
; (leaf A 4)                      (B D C) 4
;                     (leaf B 2)              (D C) 2
;                                     (leaf D 1)      (leaf C 1)


#|Exercise 2.68
The encode procedure takes as arguments a message and a tree and produces the list of bits that gives the encoded message

encode-symbol is a procedure, which you must write, that returns the list of bits that encodes a given symbol
according to a given tree. You should design encode-symbol so that it signals an error if the symbol is not
in the tree at all. Test your procedure by encoding the result you obtained in Exercise 2.67 with the
sample tree and seeing whether it is the same as the original sample message|#

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
    (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
    (if (not (member symbol (symbols tree)))
        (error "symbol not in tree: ENCODE-SYMBOL" symbol)
        (cond ((leaf? tree) '())
              ((member symbol (symbols (left-branch tree)))
                (cons 0 (encode-symbol symbol (left-branch tree))))
              (else
                (cons 1 (encode-symbol symbol (right-branch tree)))))))

;test
(encode (decode '(0 1 1 0 0 1 0 1 0 1 1 1 0) sample-tree) sample-tree)
;'(0 1 1 0 0 1 0 1 0 1 1 1 0)

(encode '(D) sample-tree) ;'(1 1 0)
(encode '(E) sample-tree) ;symbol not in tree: ENCODE-SYMBOL 'E






