#lang sicp

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
  (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream low
                 (stream-enumerate-interval (+ low 1) high))))

#|Exercise 3.50
Complete the following definition, which generalizes stream-map to allow procedures that take multiple arguments|#

(define (stream-map-m proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream (apply proc (map stream-car argstreams))
                 (apply stream-map-m (cons proc
                                         (map stream-cdr argstreams))))))

;; test
(define numbers (stream-enumerate-interval 1 5))

(stream-for-each display
  (stream-map-m + numbers numbers))
; => 246810done


#|Exercise 3.51
In order to take a closer look at delayed evaluation, we will use the following procedure,
which simply returns its argument after printing it:|#
(define (show x)
  (display-line x)
  x)

#|What does the interpreter print in response to evaluating each expression in the following sequence?|#
(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))

(stream-ref x 5)
; 0
; 1
; 2
; 3
; 4
; 55

(stream-ref x 7)
; 6
; 77


#|Exercise 3.52|#
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
          (cons-stream (stream-car stream)
                       (stream-filter pred
                                      (stream-cdr stream))))
  (else (stream-filter pred (stream-cdr stream)))))


;; Consider the sequence of expressions
(define sum 0)
(display (string-append "sum: " (number->string sum) "\n"))
;sum: 0

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
(display (string-append "sum: " (number->string sum) "\n"))
;sum: 1

(define y (stream-filter even? seq))
(display (string-append "sum: " (number->string sum) "\n"))
;sum: 6

(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
(display (string-append "sum: " (number->string sum) "\n"))
;sum: 10

(stream-ref y 7) ; => 136
(display (string-append "sum: " (number->string sum) "\n"))
;sum: 136

(display-stream z)
; 10
; 15
; 45
; 55
; 105
; 120
; 190
; 210done
(display (string-append "sum: " (number->string sum) "\n"))
; sum: 210

;; What is the value of sum after each of the above expressions is evaluated?
;; What is the printed response to evaluating the stream-ref and display-stream expressions?
;; Would these responses differ if we had implemented (delay ⟨exp⟩) simply as (lambda () ⟨exp⟩)
;; without using the optimization provided by memo-proc? Explain.