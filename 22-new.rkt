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

(define (get i l)
  (if (= i 1)
      (car l)
      (get (- i 1) (cdr l))))

(define (count-change amount coins)
  (define (iter a coin coins)
    (cond ((= a 0) 1)
          ((or (< a 0) (= coin 0)) 0)
          (else (+ (iter a (- coin 1) coins)
                   (iter (- a (get coin coins)) coin coins)))))
  (iter amount (length coins) (reverse coins)))

(count-change 100 us-coins)
(count-change 100 uk-coins)
