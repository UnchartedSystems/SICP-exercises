(define (cycle-tester)
  (+ 1 1))

(define (inc a)
  (+ 1 a))

(define (cube a)
  (* a a a))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (slow-sum-cubes a b)
  (slow-sum cube a inc b))

(define (fast-sum-cubes a b)
  (fast-sum cube a inc b))

(define (slow-sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (slow-sum term (next a) next b))))

(define (fast-sum term a next b)
  (define (cycle term a next b sum)
    (if (> a b)
        sum
        (cycle term (next a) next b (+ sum (term a)))))
  (cycle term a next b 0))


(define (product-recur term a b)
  (define (pass-thru sum)
    (display sum)
    (newline)
    sum)
  (define (prod-iter term a b)
    ;(display sum)
    (if (= b 0)
        0
        (pass-thru (+ (term (prod-iter term a (- b 1)) a b)))))
  (prod-iter term a b))

(define (product term a b)
  (define (prod-iter term a b sum)
    (display sum)
    (newline)
    (if (= b 0)
        sum
        (prod-iter term a (- b 1) (term sum a b))))
  (prod-iter term a b 0))

  (define (multi a b)
    (define (multi-term sum a b)
      (+ sum a))
    (product-recur multi-term a b))

(define (fact a)
  (define (fact-iter sum a b)
    (+ sum b))
  (product fact-iter a a))


(define (accumulate finish initial term a next b)
  (define (accum-iter a b sum)
    (if (finish sum a b)
        sum
        (accum-iter (next a) b (term sum a b))))
  (accum-iter a b initial))

(define (multi-acc a b)
  (define (multi-term sum a b)
    (+ sum b))
  (define (multi-finish sum a b)
    (= a 0))
  (define (multi-next a)
    (- a 1))
  (accumulate multi-finish 0 multi-term a multi-next b))

(define (slow-sum-acc a b)
  (define (cubes-finish sum a b)
    (> a b))
  (accumulate cubes-finish 0 cube a inc b))


