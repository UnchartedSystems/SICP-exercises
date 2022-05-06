;;; Definition of Square Root with free auxillary function definitions
(define (sqrt-v1 x)
  (sqrt-iter 1 x))

(define (improve-sqrt sqrt-guess num)
    (/ (+ sqrt-guess (/ num sqrt-guess)) 2))

(define (approx-sqrt? sqrt-guess num)
    (< (abs (- (/ num sqrt-guess) sqrt-guess)) 0.001))

; Accurate to 3 decimal places
(define (sqrt-iter sqrt-guess num)
    (if (approx-sqrt? sqrt-guess num)
        sqrt-guess
        (sqrt-iter (improve-sqrt sqrt-guess num) num)))


;;; Free Utility Functions for next code blocks
(define (square x)
    (* x x))

(define (average x y)
    (/ (+ x y) 2))


;;; Block Definition of Square root with auxillary functions locally bound
;;; with variable passing and a newly binded x within auxillary functions
(define (sqrt-v2 x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))


;;; Block Definition of Square root with auxillary functions locally bound
;;; x is a free variable internal to sqrt.
(define (sqrt-v3 x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;; Full uncomposed sqrt function
(define (sqrt-bad guess x)
    (if (< (abs (- x (* guess guess))) 0.001)
        guess
        (sqrt-bad (/ (+ (/ x guess) guess) 2) x)))