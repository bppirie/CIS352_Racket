#lang racket
;; Folds

(define (fold reducer init lst) ;; 2 argument reducer, direct style fold
  (match lst
    ['() init]
    [`(,hd . ,tl) (reducer hd (fold reducer init tl))]))

(define (fold-sum lst) ;; Sums the list
  (fold (lambda (next-element acc) (+ next-element acc)) 0 lst))

(define (fold-product lst) ;; Multiplies the list
  (fold (lambda (next-element acc) (* next-element acc)) 1 lst))

(define (fold-filter f lst) ;; Filters the list
  (fold (lambda (next-element acc) (if (f next-element)
                                       (cons next-element acc)
                                       acc)) '() lst))


(define (foldl reducer acc lst) ;; Fold left, tail recursive
  (match lst
    ['() acc]
    [`(,hd . ,tl) (foldl reducer (reducer hd acc) tl)]))


(define (foldl-reverse l) ;; Tail recursive foldl reverse
  (foldl (lambda (next-element acc) (cons next-element acc)) '() l))

