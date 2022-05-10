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


 ;; (last-pair (list))
 ;; (last-pair (list 1 2 3))

; 2.18
(define (reverse l)
  (define (iter ol nl)
    (if (null? ol)
        nl
        (iter (cdr ol) (cons (car ol) nl))))
  (iter l (list)))

;; (reverse (list))
;; (reverse (list 1 2 3))

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
            (cc (- amount (test
                           kinds-of-coins))
                kinds-of-coins)))))

(define (test kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 2)
        ((= kinds-of-coins 3) 3)
        ((= kinds-of-coins 4) 30)))

(count-change 5)
(count-change 10)
(count-change 15)
(count-change 20)
(count-change 25)
(count-change 30)
(count-change 35)
(count-change 40)
(count-change 45)
(count-change 50)
(count-change 55)
(count-change 60)
(count-change 65)
(count-change 70)
(count-change 75)
(count-change 80)
(count-change 85)
(count-change 90)
(count-change 95)
(count-change 100)
