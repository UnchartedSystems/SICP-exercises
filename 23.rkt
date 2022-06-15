#lang sicp

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (* tree factor))
        (else
         (cons (scale-tree (car tree)
                           factor)
               (scale-tree (cdr tree)
                           factor)))))

(define (scale-tree-map tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(define ex-tree (list 1
                      (list 2 (list 3 4) 5)
                      (list 6 7)))

(scale-tree ex-tree 10)
(scale-tree-map ex-tree 10)


(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (* sub-tree sub-tree)))
       tree))


(square-tree ex-tree)
(square-tree-map ex-tree)

;; 2.31

(define (square x) (* x x))

(define (tree-map f t)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       t))

(define (higher-order-square-tree tree)
  (tree-map square tree))

(tree-map inc ex-tree)

(define (contains? x l)
  (cond ((null? l) #f)
         ((eq? x (car l)) #t)
         (#t (contains? x (cdr l)))))

(define (set l)
  (define (iter ol nl)
    (cond ((null? ol) nl)
          ((contains? (car ol) nl) (iter (cdr ol) nl))
          (else (iter (cdr ol) (cons (car ol) nl)))))
  (reverse (iter l `())))

(set `(1 2 1 4 2 3 7 7 8 4 5))
(set `(1 2 3 7 8 5))


;Why is there a difference here?? Will investigate
;ahh, represents an empty list, so (list nil) is `(`())
(append (list nil) (list nil))
(append `() `())


;; Generate all possible subsets of a set of numbers
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

; How does this interesting piece of code work?
; It works recursively to fetch an empty list at the base case
; then as it climbs back down the stack it takes what has been returned recursively
; doubles it, and adds the first num in the original argument of that function
; so for any set of n elements the number of subsets of that set is 2ⁿ

(subsets `(1 2 3))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else  (filter predicate
                       (cdr sequence)))))

(define (accumulate f base sequence)
  (if (null? sequence)
      base
      (f (car sequence)
          (accumulate f
                      base
                      (cdr sequence)))))


(define (map2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil sequence))

 (define (append2 seq1 seq2)
   (accumulate cons seq2 seq1))

(define (length2 sequence)
  (accumulate (lambda (x y) (inc y)) 0 sequence))

(map2 `(1 2 3))
(append2 `(1 2 3) `(4 5 6))
(length2 `(1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

(define (sum-odd-squares1 tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares1
                  (car tree))
                 (sum-odd-squares1
                  (cdr tree))))))

(define (sum-odd-squares2 tree)
  (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (fib n)
  (define (fib-iter a b count)
    (cond ((= count 0) a)
          (else (fib-iter (+ a b) a (- count 1)))))
  (fib-iter 1 0 n))

(define (even-fibs1 n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define (even-fibs n)
  (accumulate cons nil (filter even? (map fib (enumerate-interval 0 n)))))

