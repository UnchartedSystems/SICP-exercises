#lang sicp

; 2.17
(define (last-pair l)
  (define (iter l)
    (if (null? (cdr l) )
        (car l)
        (iter (cdr l))))
  (if (null? l)
      l
      (iter l)))


 (last-pair (list))
 (last-pair (list 1 2 3))

; 2.18
(define (reverse l)
  (define (iter ol nl)
    (if (null? ol)
        nl
        (iter (cdr ol) (cons (car ol) nl))))
  (iter l (list)))

(reverse (list))
(reverse (list 1 2 3))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;; Original Tree-Recursive Form of Count Change
(define (count-change amount)
  (cc amount 2))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (= kinds-of-coins 0))
         0)
        (else
         (+ (cc amount (- kinds-of-coins 1))
            (cc (- amount (first-denomination
                           kinds-of-coins))
                kinds-of-coins)))))

(define (test kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 15)))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 10)
(count-change 15)
(count-change 20)
(count-change 25)
(count-change 30)
(count-change 35)
(count-change 40)

;; Iterative Count-Change
;(define (iter-count amount coin-type))

; Count the set of ways a set of numbers can represent a number
; 5 = 2
; 10 = 4

; 5 = 2
; 5:1
; 1:5

; 10 = 4
; 10:1
; 5:2
; 5:1 + 1:5
; 1:10

; 15 = 6
; 10:1 + 5:1
; 10:1 + 1:5
; 5:3
; 5:2 + 1:5
; 5:1 + 1:10
; 1:15

; 20 = 9
; 10:2
; 10:1 + 5:2
; 10:1 + 5:1 + 1:5
; 10:1 + 1:10
; 5:4
; 5:3 + 1:5
; 5:2 + 1:10
; 5:1 + 1:15
; 1:20

; 25 = 13
; 25:1
; 10:2 + 5:1
; 10:2 + 1:5
; 10:1 + 5:3
; 10:1 + 5:2 + 1:5
; 10:1 + 5:1 + 1:10
; 10:1 + 1:15
; 5:5
; 5:4 + 1:5
; 5:3 + 1:10
; 5:2 + 1:15
; 5:1 + 1:20
; 1:25

;Coins 1,5
; 3,4,5,6,7,8,9
;Coins 1,5,10
; 4,6,9,12,16,20,25
;Coins: 1,5,10,15
; 4,6,9,13,18,24,31
