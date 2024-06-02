#lang racket



(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))


#|Exercise 2.42
The “eight-queens puzzle” asks how to place eight queens on a chessboard so that no queen is in check from any other|#

(define empty-board '())

(define (make-position row col)
  (cons row col))

(define (adjoin-position new-row k rest-of-queens)
  (cons (make-position new-row k) rest-of-queens))

(define (row position)
  (car position))

(define (col position)
  (car position))

(define (queen-k positions)
  (list-ref positions 0))

(define (safe? k positions)
  (let ((queen-k-row (row (car positions))))
    (accumulate (lambda (p tail)
                  (and (not (= queen-k-row (row p))) tail))
                #t
                (cdr positions))))



(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;;WORK


;(define (queen-k positions)
;  (list-ref positions 0))
;(queen-k '((2 . 4) (4 . 3) (4 . 2) (4 . 1))) ;'(2 . 4)

(accumulate (lambda (p tail)
              (cons (not (= 1 (row p))) tail))
               null ;#t
               '((1 . 3) (1 . 2) (2 . 1)))
;'(#f #f #t)

(safe? 4 '((2 . 4) (1 . 3) (1 . 2) (1 . 1)))
(safe? 4 '((2 . 4) (1 . 3) (2 . 2) (1 . 1)))

(queens 5)