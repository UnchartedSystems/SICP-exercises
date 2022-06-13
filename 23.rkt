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


;; Generate all possible subsets of a list of numbers
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest "REPLACE THIS STRING!" rest))))