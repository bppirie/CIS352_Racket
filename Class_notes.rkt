#lang racket

(displayln "Hello World!") ;Displays line of text

(define (calculate-dif y0 y1) ;Define a function
  (- y1 y0))   ;(define (-function name- -parameters-)
                  ;(-calculation/body of function-) )

(exact? 2.3) ;Predicate that checks if 2.3 is an exact number

;; String "asdf"
;; Char   #\a
;; Symbol 'H

(if (equal? (+ 2 4) 6) ;Check if
    "Yes" ;Return if true
    "No") ;Return if false

(define (foo) ;Function foo
  1)

(define (collatz x)
  (if (equal? 0 (modulo x 2)) ;Modulo use
      (/ x 2)
      (add1 (* 3 x)))) ;add1 adds 1

(let ([z 2][y 3]) ;Let z = 2, y = 3 in scope and compute
  (+ z y))
(let* ([z 2][y z]) ;Let z = 2, y = z in scope and compute
  (+ z y))

(cond [(= 1 1) (displayln 'false)]
      [(= 2 3) (displayln 'false)]
      [else    (displayln 'else)])

(define tax 1.05) ;Variable tax is equal to 1.05 
(define (total) (let ([x 50]) (* x tax)))

;;Use let when you have chained defns...

;; Lists
'() ;empty list
null ;empty list

(define xs (cons 0 1)) ;Set xs as a pair

(car xs) ;Evaluates to left element = 0
(cdr xs) ;Evaluates to right element = 1

(define lst (list 1 2 3)) ;= '(1 2 3)

(empty? lst) ;checks if empty
(car lst) ; gets first
(cdr lst) ; gets rest of list

(length lst) ;gives length
(list-ref lst 2) ;gives element at 2 index
(append lst '(6 7)) ;appends 2 lists
(reverse lst) ; reverses lst
(member 5 lst) ; checks if element is in list

(cons 5 lst) ;Concats 5 to the front of list\

;;Vectors
(vector 1 2 3) ;;Creates vector = '#(1 2 3)