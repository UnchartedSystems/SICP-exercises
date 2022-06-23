#lang sicp

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define (accumulate f base sequence)
  (if (null? sequence)
      base
      (f (car sequence)
         (accumulate f
                     base
                     (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))


(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else  (filter predicate
                       (cdr sequence)))))

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

(define (unique-seqs size n)
  (filter (lambda (s) (= size (length s)))
          (subsets (reverse (enumerate-interval 1 n)))))

(unique-pairs 5)

(define (prime-sum-pairs-simp n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-seqs 2 n))))

(prime-sum-pairs 9)
(prime-sum-pairs-simp 9)

(define (ordered-summing-triplets s n)
  (filter (lambda (seq) (= s (accumulate + 0 seq)))
          (map reverse (unique-seqs 3 n))))

(ordered-summing-triplets 10 10)

(define (ordered-sums seq-size n)
  (filter (lambda (seq) (= n (accumulate + 0 seq)))
          (map reverse (unique-seqs seq-size n))))

(ordered-sums 5 20)
