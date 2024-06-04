#lang racket

#|
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))



#|Exercise 2.44
Define the procedure up-split used by cornersplit.
It is similar to right-split, except that it switches the roles of below and beside|#

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
|#


#|Exercise 2.45: right-split and up-split can be expressed
as instances of a general spliing operation. Define a pro-
cedure split  with the property that evaluating

(define right-split (split beside below))
(define up-split (split below beside))

produces procedures right-split and up-split with the same behaviors as the ones already defined


(define (split first-split second-split)
  (define (splitter painter n)
    (if (= n 0)
        painter
        (let ((smaller (splitter painter (- n 1))))
          (first-split painter (second-split smaller smaller)))))
  splitter)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))
|#


#|Exercise 2.46
A two-dimensional vector v running from the origin to a point can be represented as a pair consisting
of an x-coordinate and a y-coordinate. Implement a data abstraction for vectors by giving a constructor make-vect
and corresponding selectors xcor-vect and ycor-vect. In terms of your selectors and constructor, implement proce-
dures add-vect, sub-vect, and scale-vect that perform the operations vector addition, vector subtraction, and mul-
tiplying a vector by a scalar:

(x1 , y1) + (x2 , y2) = (x1 + x2 , y1 + y2),
(x1 , y1) - (x2 , y2) = (x1 - x2 , y1 - y2),
s(x , y) = (sx , sy)
|#

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect u v)
  (cons (+ (xcor-vect u) (xcor-vect v))
        (+ (ycor-vect u) (ycor-vect v))))

(define (sub-vect u v)
  (cons (- (xcor-vect u) (xcor-vect v))
        (- (ycor-vect u) (ycor-vect v))))

(define (scale-vect s v)
  (cons (* s (xcor-vect v))
        (* s (ycor-vect v))))

;;test
(define u (make-vect 1 2))
(define v (make-vect 3 4))

(add-vect u v) ;'(4 . 6)
(sub-vect u (scale-vect 0.5 v)) ;'(-0.5 . 0.0)



    

