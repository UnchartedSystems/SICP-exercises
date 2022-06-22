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




(map2 inc `(1 2 3))
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


(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                 (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))
(+ (* (+ (* (+ (* (+ (* (+ (* (+ (* 0 2) 1) 2) 0) 2) 5) 2) 0) 2) 3) 2) 1)

(define (count-leaves-ex x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves-ex (car x))
                 (count-leaves-ex (cdr x))))))

;; Without null checking empty lists will be counted, unlike original
(define (count-leaves t)
  (accumulate + 0 (map (lambda (e) (if (null? e)
                                       0
                                       (if (pair? e)
                                           (count-leaves e)
                                           1)))
                       t)))

(count-leaves-ex (cons (list 1 2 3 `()) (list 4 (list 5 6))))
(count-leaves (cons (list 1 2 3 `()) (list 4 (list 5 6))))

(define (accumulate-ex f base sequence)
  (if (null? sequence)
      base
      (f (car sequence)
         (accumulate f base (cdr sequence)))))

 (define (accumulate-n f base seqs)
   (if (null? (car seqs))
       nil
       (cons (accumulate f base (map car seqs))
             (accumulate-n f base (map cdr seqs)))))

(define s (list `(1 2 3) `(4 5 6) `(7 8 9) `(10 11 12)))
(accumulate-n + 0 s)

(define m-ex (list `(1 2 3 4) `(4 5 6 6) `(6 7 8 9)))

(accumulate-n (lambda (x y) (cons (inc x) y)) `() m-ex)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product (car m-ex) (car (cdr m-ex)))

;; When multiplying a matrix by a vector,
;; each row of the matrix is multiplied by the vector
;; (using the dot product procedure described above).
(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(matrix-*-vector m-ex `(1 2 3 4))

(define (transpose mat)
  (accumulate-n cons `() mat))

(transpose m-ex)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mv) (matrix-*-vector cols mv)) m)))

(matrix-*-matrix m-ex (list `(2 5 7) `(3 6 8) `(4 7 9) `(5 7 10)))

(define (fold-right f base sequence)
  (if (null? sequence)
      base
      (f (car sequence)
         (fold-right f base (cdr sequence)))))

(define (fold-left f base sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (f result (car rest))
              (cdr rest))))
  (iter base sequence))

;; Changes not only direction of application but order of function parameters? why?

(fold-right - 0 (list 1 2 1))
(fold-left  - 0 (list 1 2 1))
(fold-right list nil (list 1 2 3))
(fold-left  list nil (list 1 2 3))


(define (reverse-r sequence)
  (fold-right
   (lambda (x r) (append r (list x))) nil sequence))

(define (reverse-l sequence)
  (fold-left
   (lambda (r x) (append (list x) r)) nil sequence))

(reverse-r `(1 2 3))
(reverse-l `(1 2 3))

;; Next up is Nested Mappings
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime? x)
  (define (iter seq)
    (if (null? seq)
        true
        (if (= 0 (modulo x (car seq)))
            false
            (iter (cdr seq)))))
  (iter (enumerate-interval 2 (/ x 2))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))


(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))


(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter
        prime-sum?
        (flatmap
         (lambda (i)
           (map (lambda (j)
                  (list i j))
                (enumerate-interval
                 1
                 (- i 1))))
         (enumerate-interval 1 n)))))


(define (permutations s)
  (if (null? s)   ; empty set?
      (list nil)  ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p)
                        (cons x p))
                      (permutations
                       (remove x s))))
               s)))


(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (unique-pairs n)
  (if (< n 2)
      nil
      (filter (lambda (s) (= 2 (length s))) (subsets (reverse (enumerate-interval 1 n))))))

(unique-pairs 5)

(define (prime-sum-pairs-simp n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(prime-sum-pairs 9)
(prime-sum-pairs-simp 9)
