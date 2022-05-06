(define (times a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

                                        ;Square Root Formula
                                        ; 1. Start with a guess
                                        ; 2. Divide x / guess
                                        ; 3. guess = average guess & result
                                        ; 4. start at 2 with new guess
                                        ; 5. have a citeria for stopping

(define (abs x)
  (if (< 0 x)
      x
      (* x -1)))

(define (average x y)
  (/ (+ x y) 2))

(define (check x y)
  (> 0.1 (abs (- x y))))



(define (root target)
  (define (sqroot x y)
    (define (checker guess result)
      (if (check guess result)
          result
          (sqroot x (average guess result))))
    (checker (/ x y) y))
  (sqroot target 1))

(define (sqroot x y)
  (define (checker guess result)
    (if (check guess result)
        result
        (sqroot x (average guess result))))
  (checker (/ x y) y))

(define (sqrt x)
  (define (sqrt-guess guess)
    (define (sqrt-check result guess)
      (if (check result guess)
          result
          (sqrt-guess (average result guess))))
    (define (sqrt-iter guess)
      (sqrt-check (/ x guess) guess))
    (sqrt-iter guess))
  (sqrt-guess 1))

;Note = exp kickstarts process with u = 1 n = n-1 p = b

(define (exp b n)
  (exp-iter b (- n 1) 1 b))

(define (exp-iter b n u p)
  (cond ((<= u n) (exp-iter b (- n u) (* u 2) (* p p)))
        ((= n 0) p)
        ((> u n) (exp-iter b (- n 1) (+ u 1) (* p b)))))

(define (exp-test b n)
  (exp-count b (- n 1) 1 b 0))

(define (exp-count b n u p count)
  (cond ((<= u n) (exp-count b (- n u) (* u 2) (* p p) (+ count 1)))
        ((= n 0) count)
        ((> u n) (exp-count b (- n 1) (+ u 1) (* p b) (+ count 1)))))


(define (multi x y)
  (multi-iter x (- y 1) 1 x))

(define (multi-iter x y u p)
  (cond ((<= u y) (multi-iter x (- y u) (* u 2) (+ p p)))
        ((= y 0) p)
        ((> u y) (multi-iter x (- y 1) (+ u 1) (+ p x)))))


(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (cond ((= count 0) a)
        (else (fib-iter (+ a b) a (- count 1)))))


(define (fprep a b p q)
  (if true (+ (* b q) (* a q) (* a p))
           (+ (* a q) (* b p))))

(define (p) (p))


(define (applicative-test x y) (if (= x 0) x y))


(define  (expmod base exp m)
  (cond  ((= exp 0) 1)
         ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
         (else (remainder (* base (expmod base (- exp 1) m)) m))))


(define  (exptest base exp m)
  (cond  ((= exp 0) 1)
         ((even? exp) (square (exptest base (/ exp 2) m)))
         (else (* base (exptest base (- exp 1) m)))))



(define (divisor x y)
  (cond ((= y 0) x)
        (else (divisor y (remainder x y)))))

(define (smallest-divisor n)
  (fast-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))


(define (prime? n)
  (= n (smallest-divisor n)))


(define (timed-prime-test n)
  (newline)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (ranged-prime-test n count start-time)
  (newline)
  (start-prime-test n start-time)
  (if (= 0 count) (- (runtime) start-time)
                  (ranged-prime-test (+ n 1) (- count 1) start-time)))

(define (fast-divisor n factor)
  (cond ((> (square factor) n) n)
        ((divides? factor n) factor)
        ((= factor 2) (fast-divisor n 3))
        (else (fast-divisor n (+ factor 2)))))
