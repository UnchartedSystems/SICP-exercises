
(define (make-rat x y) (cons x y))

(define (numer pair) (car pair))
(define (denom pair) (cdr pair))

(define (one-half) (make-rat 1 2))
(define (one-third) (make-rat 1 3))


; Rational Number Math Ops

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (multi-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div2-rat x y)
  (make-rat (/ (numer x) (numer y))
            (/ (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (cons-proc x y)
  (lambda (l) (l x y)))

(define (car-proc l)
  (l (lambda (x y) x)))

(define (cdr-proc l)
  (l (lambda (x y) y)))

; Exercise 2.5
; 1 , 3 , 9 , 27 , 81 , 243
; 1 , 2 , 4 , 8 , 16 , 32 , 64 , 128 , 256
; Iteration #1 - Bruteforce check
; Get int - exponentiate 2 or 3, subtract from int, and check if it matches 2 or 3 exponentiated.
; Recursively exponentiate opposite num of target num, and subtract from full int, then check against target int
;

(define (car-pro int)
  (define (iter product)
    (let ((check (check product)))
      (display check)
      (if check
          check
          (if (> product int)
              (error "argument does not define a pair - car-pro")
              (iter (* product 3))))))
  (define (check product)
    (define (check-iter accum a)
      (cond ((= (* accum product) int) a)
            ((< (* accum product) int) (check-iter (* accum 2) (+ a 1)))
            (else (= 1 0))))
    (display "Checked!")
    (check-iter 1 0))
  (iter 1))

(define (prime-cons int a b)
  (define (iter product)
    (let ((check (check product)))
      (display check)
      (if check
          check
          (if (> product int)
              (error "argument does not define a pair - car-pro")
              (iter (* product a))))))
  (define (check product)
    (define (check-iter accum a)
      (cond ((= (* accum product) int) a)
            ((< (* accum product) int) (check-iter (* accum b) (+ a 1)))
            (else (= 1 0))))
    (display "Checked!")
    (check-iter 1 0))
  (iter 1))

(define (prime-car int)
  (prime-cons int 3 2))

(define (prime-cdr int)
  (prime-cons int 2 3))

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define (addition x y)
  (lambda (f) (lambda (v) ((x f) ((y f) v)))))

;; Progression of "simple" Lambda Calculus
;; (((two one) square) 2)
;; (((lambda (x) (one (one x))) square) 2)
;; ((one (one square)) 2)
;; ((one (lambda (x) (square x))) 2)
;; ((lambda (x) ((lambda (x) (square x)) x)) 2)
;; ((lambda (x) (square x)) 2)
;; (square 2)
;; This follows exponentiation - 1^2

(define (make-interval a b) (cons a b))

(define (lower-bound  interval) (min (car interval) (cdr interval)))
(define (upper-bound  interval) (max (car interval) (cdr interval)))

(define (negate-interval interval)
  (make-interval (* -1 (lower-bound interval)) (* -1 (upper-bound interval))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (let ((y (negate-interval y)))
    (add-interval x y)))

(define (pos? x) (< 0 x))
(define (neg? x) (> 0 x))

(define (old-mul-interval x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    (let ((i1 (* x1 y1))
          (i2 (* x1 y2))
          (i3 (* x2 y1))
          (i4 (* x2 y2)))
      (make-interval (min i1 i2 i3 i4)
                     (max i1 i2 i3 i4)))))

(define (mul-interval x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    (cond ((and (pos? x1) (pos? y1))
           (make-interval (* x1 y1) (* x2 y2)))
          ((and (pos? x2) (pos? y1))
           (make-interval (* x1 y2) (* x2 y2)))
          ((and (pos? x1) (pos? y2))
           (make-interval (* x2 y1) (* x2 y2)))
          ((and (neg? x2) (pos? y2))
           (make-interval (* x1 y2) (* x2 y1)))
          ((and (pos? x2) (neg? y2))
           (make-interval (* x1 y2) (* x2 y1)))
          ((and (neg? x2) (neg? y2))
           (make-interval (* x1 y1) (* x2 y2)))
          ((and (pos? x2) (pos? y2))
           (let ((i1 (* x1 y1))
                 (i2 (* x1 y2))
                 (i3 (* x2 y1))
                 (i4 (* x2 y2)))
             (make-interval (min i1 i2 i3 i4)
                            (max i1 i2 i3 i4)))))))

(define (interval-div x y)
  (mul-interval x (make-interval (/ 1.0 (lower-bound y))
                                 (/ 1.0 (upper-bound y)))))

(define (make-center-percent c p)
  (let ((w (* c p)))
  (make-interval (- c w)
                 (+ c w))))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (- (upper-bound i) (center i)))

(define (interval-percent i)
  (/ (width i) (center i)))

(define (simple-multi-percent x y)
  (+ (interval-percent x) (interval-percent y)))
