#lang racket

;; numbers are represented as:
;; (lambda (f) (lambda (x) (f ... (f x))))
;; where there are n calls to f.
(define zero (lambda (f) (lambda (x) x)))
(define one  (lambda (f) (lambda (x) (f x))))
(define two  (lambda (f) (lambda (x) (f (f x)))))

;; do add1 n times, starting from 0
;; (add1 (add1 ... (add1 0) ...))
(define (church->nat n)
  ((n add1) 0))

(define succ
  ;; the *argument*
  (lambda (n)
    ;; the thing we're *returning* should do f "n+1 times"
    ;; ((n f) x) "applies f n times" and returns a result
    ;; 
    (lambda (f) (lambda (x) (f ((n f) x))))))

(define plus
  (lambda (n)
    (lambda (k) 
      (lambda (f) (lambda (x) ((k f) ((n f) x)))))))

(define mult
  (lambda (n)
    (lambda (k)
      (lambda (f) (lambda (x) (((n k) f) x))))))

(displayln (format "we computed ~a" (church->nat ((plus two) two))))