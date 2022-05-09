#lang sicp

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
