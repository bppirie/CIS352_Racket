#lang racket

;; Bradley Pirie
;; Lab 01

;;Function double that doubles a value
(define (double x)
  (* x 2))

;;Function that returns the "sum of squares"
(define (sum-of-squares x y)
  (+ (* x x) (* y y)))

;;Function that checks pythagorean triplet
(define (pythagorean-triplet x y z)
  (if (equal? (* z z) (sum-of-squares x y))
      "Yes"
      "No"))

;;Function that returns sqrt/square of a number
(define (square x)
  (cond [(> x 0) (sqrt x)]
        [(< x 0) (* x x)]
        [else 0]))