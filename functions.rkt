#lang racket

(provide flatten-list)
(provide cartesian-product)
(provide zigzag)
(provide encrypt)
(provide decrypt)

(define (flattenHelper l1 r)
   (for/list ([i l1])
    (cond  [(empty? l1) ]
           [ (list? i) (set! r (flattenHelper i r))]
           [ else (set! r (append r (list i)))])
     ) r
  )
  
(define (flatten-list l1)
  (flattenHelper l1 '() ))


(define (cartesian-product l1 l2)
 (define r '())
 (cond [(or (empty? l2) (empty? l1)) '()]
        [else
         (for/list ([i l1])
          (for/list ([j l2])
            (set! r (append r (list (list i j))  ))))
         r ]))
 

(define (downwardDiag i j)
  (even? (+ i j)))

(define (upwardDiag i j)
  (odd? (+ i j)))

(define (zigzagHelp n i j k)
 (cond
     [ (= i j 0) n]
     [ (and (= i 0) (upwardDiag i j)) (zigzagHelp (+ 1 n) 0 (- j 1) k) ]
     [ (and (= j 0) (downwardDiag i j)) (zigzagHelp (+ 1 n) (- i 1) 0 k)]
     [ (and (> j 0) (and (= i k) (downwardDiag i j)))(zigzagHelp (+ 1 n) i (- j 1) k)]
     [ (and (> i 0) (and (= j k) (upwardDiag i j)))(zigzagHelp (+ 1 n) (- i 1) j k)]
     [ (upwardDiag i j) (zigzagHelp (+ 1 n) (- i 1) (+ j 1) k)]
     [ (downwardDiag i j) (zigzagHelp (+ 1 n) (+ i 1) (- j 1) k)]))

(define (zigzag n)
  (cond
       [(= n 0) '()]
       [(= n 1) '((0))]
       [else
        (for/list ([i n])
          (for/list ([j n])
            (zigzagHelp 0 i j (- n 1))
            ))]))

;(integer->char 65) is A so 90 must be Z
;n is from 1 to 25
;(integer->char 97) is a so 122 must be z
;We want to ignore all characters that aren't letters like spaces, periods, apostrophes etc...
(define (encrypt n s)
   ( list->string (for/list ([i (in-string s)])
                   (cond
                   [  (<  64 (char->integer i) 91 ) (integer->char (+ (modulo (+ (- (char->integer i) 65) n)  26 ) 65))]  
                   [  (<  96 (char->integer i) 123) (integer->char (+ (modulo (+ (- (char->integer i) 97) n)  26 ) 97))]
                   [else i]
        ))))

(define (decrypt n s)
   ( list->string (for/list ([i (in-string s)])
                 (cond
                   [  (<  64 (char->integer i) 91 ) (integer->char (+ (modulo (- (- (char->integer i) 65) n)  26 ) 65))]  
                   [  (<  96 (char->integer i) 123) (integer->char (+ (modulo (- (- (char->integer i) 97) n)  26 ) 97))]
                   [else i]          
        ))))
    