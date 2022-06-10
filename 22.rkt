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

;; 2.29 - Big Exercise

(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

(define test-mobile
  (make-mobile (make-branch 2 (make-mobile (make-branch 2 (make-mobile (make-branch 1 1)
                                                                       (make-branch 1 1)))
                                           (make-branch 1 4)))
               (make-branch 1 (make-mobile (make-branch 3 3)
                                           (make-branch 1 9)))))

(define (first l)
  (car l))

(define (second l)
  (car (cdr l)))

(define (left-branch mobile)
  (first mobile))

(define (right-branch mobile)
  (second mobile))

(define (get-length branch)
  (first branch))

(define (get-structure branch)
  (second branch))

(define (mobile-or-branch? t)
  (pair? (first t)))

(define (weigh-branch branch)
  (if (not (pair? (second branch)))
      (second branch)
      (total-weight? (second branch))))

(define (total-weight? mobile)
  (+ (weigh-branch (left-branch mobile))
     (weigh-branch (right-branch mobile))))

(total-weight? test-mobile)

(define (branch-balance? branch)
  (* (get-length branch)
     (if (not (pair? (get-structure branch)))
         (get-structure branch)
         (total-weight? (get-structure branch)))))

(define (is-balanced? mobile)
  (= (branch-balance? (left-branch mobile))
     (branch-balance? (right-branch mobile))))

(define (has-mobile? branch)
  (pair? (get-structure branch)))

; this is a stupid, quick, and dirty solution. Will eliminate much redundant compute when I have more time!
(define (total-balanced? mobile)
  (and (is-balanced? mobile)
       (if (has-mobile? (left-branch mobile))
           (total-balanced? (get-structure (left-branch mobile)))
           true)
       (if (has-mobile? (right-branch mobile))
           (total-balanced? (get-structure (right-branch mobile)))
           true)))

(total-balanced? test-mobile)


(define (cons-mobile left right)
  (cons left right))

(define (cons-branch len structure)
  (cons len structure))


(define test-cons-mobile
  (cons-mobile (cons-branch 2 (cons-mobile (cons-branch 2 (cons-mobile (cons-branch 1 1)
                                                                       (cons-branch 1 1)))
                                           (cons-branch 1 4)))
               (cons-branch 1 (cons-mobile (cons-branch 3 3)
                                           (cons-branch 1 9)))))
test-mobile
test-cons-mobile

(define (firs l)
  (car l))

(define (secons l)
  (cdr l))

(define (cleft-branch mobile)
  (firs mobile))

(define (cright-branch mobile)
  (secons mobile))

(define (cget-length branch)
  (firs branch))

(define (cget-structure branch)
  (secons branch))

(define (cmobile-or-branch? t)
  (pair? (firs t)))

(define (cweigh-branch branch)
  (if (not (pair? (secons branch)))
      (secons branch)
      (ctotal-weight? (secons branch))))

(define (ctotal-weight? mobile)
  (+ (cweigh-branch (cleft-branch mobile))
     (cweigh-branch (cright-branch mobile))))

(ctotal-weight? test-cons-mobile)

(define (cbranch-balance? branch)
  (* (cget-length branch)
     (if (not (pair? (cget-structure branch)))
         (cget-structure branch)
         (ctotal-weight? (cget-structure branch)))))

(define (cis-balanced? mobile)
  (= (cbranch-balance? (cleft-branch mobile))
     (cbranch-balance? (cright-branch mobile))))

(define (chas-mobile? branch)
  (pair? (cget-structure branch)))

; this is a stupid, quick, and dirty solution. Will eliminate much redundant compute when I have more time!
(define (ctotal-balanced? mobile)
  (and (cis-balanced? mobile)
       (if (chas-mobile? (cleft-branch mobile))
           (ctotal-balanced? (cget-structure (cleft-branch mobile)))
           true)
       (if (chas-mobile? (cright-branch mobile))
           (ctotal-balanced? (cget-structure (cright-branch mobile)))
           true)))

(ctotal-balanced? test-cons-mobile)
