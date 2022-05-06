
(define (sqr-sum x y)
  (+ (* x x) (* y y)))

(define (triple-sort a b c)
  (if (< a b)
      (if (< a c)
          (sqr-sum b c)
          (sqr-sum a b))
      (triple-sort b c a)))
