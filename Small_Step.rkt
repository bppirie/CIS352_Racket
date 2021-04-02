#lang racket
(define value? integer?)


(define (expr? e)
  (match e
    [(? rational? n) #t]
    [`(plus ,(? expr? e0) ,(? expr? e1)) #t]
    [`(div ,(? expr? e0) ,(? expr? e1)) #t]
    [`(not ,(? expr? e-guard)) #t]
    [`(if ,(? expr? e0) ,(? expr? e1) ,(? expr? e2)) #t]
    [_ #f]))

(define/contract (step e)
  (-> (lambda (x) (and (expr? x) (not (value? x)))) expr?)
  (match e
    [`(plus ,(? number? n0) ,(? number? n1)) (+ n0 n1)]
    [`(plus ,(? number? n0) ,(? expr? e1)) `(plus ,n0 ,(step e1))]
    [`(plus ,(? expr? e0) ,(? expr? e1)) `(plus ,(step e0) ,e1)]
    [`(div ,(? number? n0) ,(? number? n1)) (/ n0 n1)]
    [`(div ,(? number? n0) ,(? expr? e1)) `(div ,n0 ,(step e1))]
    [`(div ,(? expr? e0) ,(? expr? e1)) `(div ,(step e0) ,e1)]
    ;; write the rules for not and if here...
    [_ (error (format "not sure how to step ~a" e))]))



;; Lambda Calculus
(define (expr1? e)
  (match e
    [(? symbol? x) #t]
    [`(lambda (,(? symbol? x)) ,(? expr1? e-body)) #t]
    [`(,(? expr1? e0) ,(? expr1? e1)) #t]
    [_ #f]))

(define (free-vars e)
  (match e
    [(? symbol? x) (set x)]
    [`(lambda (,x) ,e-body) (set-remove (free-vars e-body) x)]
    [`(,e0 ,e1) (set-union (free-vars e0) (free-vars e1))]))

;; use capture-avoiding substitution to replace x with e-tgt in e-source
;; follow 
(define (subst e-source x e-tgt)
  (match e-source
    [(? symbol? y) #:when (equal? x y) e-tgt]
    [(? symbol? y) y]
    [`(,e0 ,e1) `(,(subst e0 x e-tgt) ,(subst e1 x e-tgt))]
    [`(lambda (,y) ,e-body) #:when (equal? x y) e-source] ;; leave it alone
    [`(lambda (,y) ,e-body) #:when (not (set-member? (free-vars e-tgt) y))
       `(lambda (,y) ,(subst e-body x e-tgt))]
    [_ (error "No capture-avoiding substitution possible.")]))

(define (foo a)
  ((lambda (x) (x x)) ((lambda (y) y) (lambda (z) z))))