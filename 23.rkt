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
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))


(square-tree ex-tree)
(square-tree-map ex-tree)

;; 2.31
