#lang racket
;; Quasi quote
(define x 5)
`(,x y 3) ; = (list x 'y 3) - Quasi quote
(let ([x0 1] [x1 2] [y0 3] [y1 4])
   `(square (point ,x0 ,y0) (point ,x1 ,y1)))
`(point ,(+ 1 x) ,(- 1 x)) ; , splices expression in

(define (mk-point x y) ; Make point using quasi quote
  `(point ,x ,y))

;; Pattern Matching
(match '(1 2 3)
  [`(,a ,b) b]
  [`(,a . ,b) b]) ; Returns '(2 3) as b

(define (match-foo z)
   (match z
     ['hello 'goodbye]
     [(? number? n) (+ n 1)]
     [(? nonnegative-integer? n) (+ n 2)] ; Never runs because it is a number
     [(cons x y) x] ; Also checks cons cell
     [`(,a0 ,a1 ,a2) (+ a1 a2)] ; Matches list of three
     [_ "error"])) ; Matches everything

(define (tree? t)
  (match t
    ['empty #t]
    [`(leaf ,v) #t]
    [`(binary ,(? tree?) ,(? tree?)) #t]
    [_ #f]))

(define/contract (tree-min t) ;; Contract on a function
  (-> tree? any/c) ;; Takes a tree and produces any value
  (match t
    ['empty (error "no minof empty tree")]
    [`(leaf ,v) v]
    [`(binary ,t0 t1) (tree-min t0)])) ; Providing incorrect input results in contract error


;; Tail Calls and Tail Recursion
(let ([x 3]) 'body) ; Body is in tail position
(if ('grd)
    'thn
    'else) ;; Thn and else are in tail position because they are the last thing we need to do

;; A function is tail recursive when every recursive call is a tail call

(define (fac-tail n acc)
  (if (= n 0)
      acc
      (fac-tail (- n 1) (* n acc))))

(define (fib-helper n nth n+1th)
  (if (equal? n 0)
      nth
      (fib-helper (- n 1) n+1th (+ nth n+1th))))

(define (length-tail l)
  (define (h l acc)
    (match l
      ['() acc]
      [`(,hd . ,tl) (h tl (+ 1 acc))]))
  (h l 0))

(define (min-tree-value t)
  (match t
    ['() (error "you messed up")]
    [`(leaf ,v) v]
    [`(node ,_ ,l ,_) (min-tree-value l)]))

