#lang racket

;; Recursion Notes

(define consls (cons 42 (cons -1 (cons 3 (cons 1 (cons 7 '()))))))

(define (length ls)
  (if (equal? (cdr ls) '())
      1
      (+ 1 (length (cdr ls)))))

(length consls)


;; Lambda Notes

((lambda (x) (+ x 3)) 2) ;; Procedure that takes in x and adds 3

(exact? (sqrt 4))

(define (g f) (lambda (x) (* 2 (* 2 (f x)))))
(define foo (g (lambda (y) y)))
(define (h f) (lambda (x) (f (f x))))
(define bar (h (g (lambda (x) (* 3 x)))))

;; Cons Diagrams and Boxes

'(this (is an) s expression)
'(leaf 42) ;; Binary tree leaf
'(node 13 (leaf -2) (leaf 42))
(define (binary-tree? t)
  (cond
    [(and (equal? (length t) 2) (equal? (first t) 'leaf) (integer? (second t))) #t]
    [(and (equal? (length t) 4) (equal? (first t) 'node) (integer? (second t)) (binary-tree? (third t)) (binary-tree? (fourth t))) #t]
    [else #f]))

;; Mapping

(define (map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (map f (cdr lst)))))

(define (square-list-values lst)
  (map (lambda (x) (* x x)) lst))

(define hg '(E X E O X E O E O))

