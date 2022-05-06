(define (identity x) x)

(define (old-sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (int-sum a b)
  (define (next a)
    (+ 1 a))
  (sum identity a next b))

(define (factorial n)
  (product identity 1 (lambda (x) (+ x 1)) n))

(define (recur-sum term a next b)
  (if (> a b) 0
      (+ (term a) (sum term (next a) next b))))

(define (recur-product term a next b)
  (if (> a b) 1
      (* (term a) (product term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b ))))

(define (sum term a next b)
  (define (combiner x y)
    (+ x y))
  (accumulate combiner 0 term a next b))

(define (product term a next b)
  (define (combiner x y)
    (* x y))
  (accumulate combiner 1 term a next b))

; let vars are computed outside of the namespace of the let
(define (let-test x y)
  (let ((x 2)
        (y (* y y)))
    (+ x y)))

(define (lambda-test x y)
  (let ((x (* x x))
        (y (* y y)))
    ((lambda (x y) (+ x y)) x y)))

(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt-fp x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define (fp-debug f guess)
  (define debug-f (lambda (x) (let ((result (f x)))
                                (display result)
                                (newline)
                                result)))
  (fixed-point debug-f guess))

(define (exp-solve num)
  (fp-debug (lambda (x) (average x (/ (log num) (log x)))) 10000.0))

(define (frac-c a b k)
  (define (frac i)
    (if (> i k)
        (/ (a i) (b i))
        (/ (a i) (+ (b i) (frac (+ i 1))))))
  (frac 1))

(define (e-pattern i)
  (if (< i 3)
      i
      (if (= 0 (modulo (- i 2) 3))
          (* 2 (+ 1 (/ (- i 2) 3)))
          1)))

; Interesting structure
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (exponent x e)
  (define (exp-iter p y)
    (if (= y 1)
        p
        (exp-iter (* p x) (- y 1))))
  (exp-iter x e))

(define (root x e)
  (fixed-point (average-damp (lambda (y) (/ x (exponent y (- e 1))))) 1.0))

(define (inc x)
  (+ x 1))

(define (double f)
  (lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated func num)
  (define (repeat f n)
    (if (= n 1)
        f
        (repeat (lambda (x) (func (f x))) (- n 1))))
  (repeat func num))

(define (square x)
  (* x x))

(define (smooth f)
  (lambda (x) (/ (+ (f (- x 1))
                      (f x)
                      (f (+ x 1)))
                   3)))

(define (smoother f)
  (lambda (x y) (/ (+ (f (- x y))
                      (f x)
                      (f (+ x y)))
                   3)))


(define (iterative-improve improve check)
  (define (iter guess num)
    (display num)
    (newline)
    (if (check guess num)
        guess
        (iter (improve guess num) num)))
  (lambda (x) (iter 1 x)))

(define (sqr-check guess num)
  (< (abs (- (/ num guess) guess)) 0.000001))

(define (sqr-improve guess num)
  (average guess (/ num guess)))
