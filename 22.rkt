#lang sicp

; Continued Pairs

; Closure Property - ability to continiously combine elements and combinations into greater combinations

(define test-sequence (cons 1 (cons 2 (cons 3 (cons 4 -1)))))

(define one-four (list 1 2 3 4))

(car (cdr one-four))

(define broken-list (cons one-four 8))
; This list has no null terminator

(define (last-pair list1)
  (let ((next (cdr list1)))
    (if (null? next)
        (car list1)
        (last-pair next))))

(last-pair one-four)

(define nil '())

(define (reverse-test list1)
  (define (iter remainder output)
    (if (null? remainder)
        output
        (iter (cdr remainder) (cons (car remainder) output))))
  (iter list1 nil))

(reverse-test '(1 3 5 7 9))

(define (reverse list1)
  (define (rev-iter rlist)
    (if (null? (cdr rlist))
        (car rlist)
        (cons (rev-iter (cdr rlist)) (car rlist))))
  (rev-iter list1))

(define (even? x)
  (= (remainder x 2) 0))

(define (same? x y)
  (or (and x y)
      (and (not x) (not y))))

(define (same-parity x . l)
  (let ((parity (even? x)))
    (define (iter numbers)
      (if (null? numbers)
          nil
          (let ((num (car numbers)))
            (if (same? (even? num) parity)
                (cons num (iter (cdr numbers)))
                (iter (cdr numbers))))))
    (iter l)))

(same-parity 2 1 3 5 4 8 9 1 3 0 6 4)

(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square x) (* x x))
(define (square-list-map items)
  (map square items ))

(square-list (list 2 5 0 8 7))
(square-list-map (list 2 5 0 8 7))

(define (square-list-bad items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              ;; iterative cons order means cons order is inverse with list order
              (cons (square (car things))
                    answer))))
  (iter items nil))


(square-list-bad (list 2 5 0 8 7))

(define (square-list-bad2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              ;cons prepends x to listable, here list is being prepended to item
              (cons answer
                    (square
                     (car things))))))
  (iter items nil))

(square-list-bad2 (list 2 5 0 8 7))


(define (for-each f items)
  (define (do items)
    (f (car items))
    (iter (cdr items)))
  (define (iter items)
    (if (null? items)
        true
        (do items)))
  (iter items))

(for-each display (list 2 5 0 8 7))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(length (cons (list 1 2 3) (list 4 5 6)))
(count-leaves (cons (list 1 2 3) (list 4 5 6)))

(append (list 1 2 3) (list 4 5 6))
(cons (list 1 2 3) (list 4 5 6))
(list (list 1 2 3) (list 4 5 6))

(define (shallow-reverse l)
  (if (null? l)
       l
      (append (shallow-reverse (cdr l)) (list (car l)))))

(define (deep-reverse l)
  (if (null? l)
      l
      (if (list? (car l))
          (append (deep-reverse (cdr l)) (list (deep-reverse (car l))))
          (append (deep-reverse (cdr l)) (list (car l))))))

(define nest-l (list 1 (list 2 (list 3 4) 5) 6 (list 7)))
nest-l
(shallow-reverse nest-l)
(shallow-reverse (list 1 2 3 4 5))
(deep-reverse nest-l)

(define (fringe t)
  (if (null? t)
      t
      (if (list? (car t))
          (append (fringe (car t)) (fringe (cdr t)))
          (cons (car t) (fringe (cdr t))))))

(fringe (list 1 2 3 4 5))
(fringe nest-l)
