;;; The Ackermann Function
(define (A x y)                         ; A
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)                ; A-x
                 (A x (- y 1))))))      ; A-y


;;; How many ways can $1 be given in half dollars, quarters, dimes, nickels, pennies (h, q, d, n, p)
; first choice is at 5 cents. 1n = 5p

;;; Default recursive method of getting change count: 292

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

;;; An implementation I made that determines the amount of tree recursive nodes made in this process: 15499

; (define (cc amount kinds-of-coins)
;   (cond ((= amount 0) 1)
;         ((or (< amount 0) (= kinds-of-coins 0)) 1)
;         (else (+ 1
;                  (cc amount
;                      (- kinds-of-coins 1))
;                  (cc (- amount
;                         (first-denomination kinds-of-coins))
;                      kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))


; A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an iterative process.

(define (func-11 n)
    (define (func-iter x total depths)
        (if (> x 3)
            total
            (func-iter
                (+ x 1)
                (+ total (* x (- n x))))))
    (if (< n 3)
        n
        (func-iter 1 0 1)
    ))

; This solution is based on wrong assumptions that n would not be checked against 3 on repeated calls, or that it would call itself

(define (func-recur n)
    (if (< n 3)
        n
        (+ (func-recur (- n 1))
            (* 2 (func-recur (- n 2)))
            (* 3 (func-recur (- n 3))))))

; (define (func-test n x) ;broken
;     (if (= n x)
;         1
;         (+  (func-test (- n 1))
;             (func-test (- n 2))
;             (func-test (- n 3)))))

(define (func-cond n)
    (cond   ((< n 3) n)
            (else   (+  (func-cond (- n 1))
                        (* 2 (func-cond (- n 2)))
                        (* 3 (func-cond (- n 3)))))))

(define (func-leaves n)
    (cond   ((< n 3) 1)
            (else   (+  (func-leaves (- n 1))
                        (func-leaves (- n 2))
(                        (func-leaves (- n 3))))))                        
)
(define (func-test n x)
    (cond   ((= n x) 1)
            ((< n x) 0) 
            (else   (+  (func-test (- n 1) x)
                        (func-test (- n 2) x)
                        (func-test (- n 3) x)))))

(define (func-fast c)
    (define (func-iter x y z c)
        (cond  ((= c 0) z)
               (else (func-iter y z (+ (* 3 x) (* 2 y) z) (- c 1)))))
    (cond  ((= c 1) 1)
           ((= c 2) 2)
           (else (func-iter 0 1 2 (- c 2)))))

;;; Recursive Pascal's Triangle Function

(define (pascal r c)
    (cond  ((or (= c 1) (= c r)) 1)
           (else (+ (pascal (- r 1) c) (pascal (- r 1) (- c 1))))))

;;; Methods of exponentiation
; Recursive exponentiation: O(n) space and time
(define (expt-recur b n)
  (if (= n 0)
      1
      (* b (expt-recur b (- n 1)))))

; Iterative function: runs at O(n) with O(1) space
 (define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                (- counter 1)
                (* b product))))


;Fast Recur using manipulation of exponent. O(log(n)) space and time.
(define (fast-expt-recur b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt-recur b (/ n 2))))
        (else (* b (fast-expt-recur b (- n 1))))))


(define (even? n)
  (= (remainder n 2) 0))

;;; My own iterative fast exponentiation algorithm using an invariant quantity

; BROKEN first edition: handles exponents wrongly by not paying attention to exponent application order
(define (fast-expt b n)
    (define (fast-expt-iter n a)
        (cond   ((= n 0) a)
                ((= a 1) (fast-expt-iter (- n 1) (* a b)))
                ((even? n) (fast-expt-iter (/ n 2) (* a a)))
                (else (fast-expt-iter (- n 1) (* a b)))))
    (fast-expt-iter n 1))


; New idea that is NOT INVARIANT
(define (fast-exp b n)
    (define (fast-iter a m)
        (cond   ((= m n) a)
                ((>= (- n m) m) (fast-iter (* a a) (* m 2)))
                (else (fast-iter (* a b) (+ m 1)))))
    (cond   ((or (= n 0) (= b 1)))
            (else (fast-iter b 1))))

; Note: This solution is actually WRONG!! It is not logarithmic! Try again!

; This test determines if fast-exp is logarithmic (Note, it is not)
(define (fast-exp-test b n)
    (define (fast-iter a m iters)
        (cond   ((= m n) iters)
                ((>= (- n m) m) (fast-iter (* a a) (* m 2) (+ iters 1)))
                (else (fast-iter (* a b) (+ m 1) (+ iters 1)))))
    (cond   ((or (= n 0) (= b 1)))
            (else (fast-iter b 1 1))))

; This() is to test my suspicion that the else line is what makes it not algorithmic (Note: I was right)
(define (fast-exp-t2 b n)
    (define (fast-iter a m iters)
        (cond   ((= m n) iters)
                ((>= (- n m) m) (fast-iter (* a a) (* m 2) (+ iters 1)))
                (else iters)))
    (cond   ((or (= n 0) (= b 1)))
            (else (fast-iter b 1 1))))  

; For invarianc have the full n used here be equal to (+ n m) as you tick down n. (NOTE: WRONG!)

; Got off stack overflow after, (* x x) looks odd. (Note: good reason for that!)
(define (pow x y)
  (define (powi acc x y)
    (cond
      ((= y 0) acc)
      ((odd? y) (powi (* acc x) x (- y 1)))
      (else (powi acc (* x x) (/ y 2)))))
  (powi 1 x y))

(define (pow-test x y)
  (define (powi acc x y iters)
    (cond
      ((= y 0) iters)
      ((odd? y) (powi (* acc x) x (- y 1) (+ iters 1)))
      (else (powi acc (* x x) (/ y 2) (+ iters 1)))))
  (powi 1 x y 1))  
; Notes: This solution is better than mine!

;;; Ultimately pow is the right solution, as it compresses not only squaring, but also odd number multiplication by using a base that takes into account previous squarings that offset the compressed divided exponent. My explanation is shit because I just started more deeply grokking it. This also works because every higher exponent result has every lower exponent result as a factor

;;; Create multiplication using addition logarithmically problem 1.17
;Textbook Starter Solution

; (define (* a b)
;   (if (= b 0)
;       0
;       (+ a (* a (- b 1)))))

; (define (fast-expt b n)
;   (cond ((= n 0) 1)
;         ((even? n) (square (fast-expt b (/ n 2))))
;         (else (* b (fast-expt b (- n 1))))))

;From the textbook
; "This algorithm takes a number of steps that is linear in b. Now suppose we include, together with  addition, operations double, which doubles an integer, and halve, which divides an (even) integer by 2. Using these, design a multiplication procedure analogous to fast-expt that uses a logarithmic number of steps."

(define (double x)
    (* x 2))

(define (halve x)
    (/ x 2))

(define (mult-recur a b)
    (define (iter b)
        (cond   ((= b 0) 0)
                ((even? b) (double (iter (halve b))))
                (else (+ a (iter (- b 1))))))
    (iter b))   

(define (mult-recur-test a b)
    (define (iter b i)
        (cond   ((= b 0) i)
                ((even? b) (iter (halve b) (+ i 1)))
                (else (iter (- b 1) (+ i 1)))))
    (iter b 1))   

(define (mult a b)
    (define (iter x a b)
        (cond   ((= b 0) x)   
                ((even? b) (iter x (double a) (halve b)))
                (else (iter (+ x a) a (- b 1)))))
    (iter 0 a b))

(define (mult-test a b)
    (define (iter x a b i)
        (cond   ((= b 0) i)   
                ((even? b) (iter x (double a) (halve b) (+ i 1)))
                (else (iter (+ x a) a (- b 1) (+ i 1)))))
    (iter 0 a b 1))

(define (mult-fast a b)
    (define (iter x a b)
        (cond   ((= b 0) x)   
                ((even? b) (iter x (double a) (halve b)))
                (else (iter (+ x a) a (- b 1)))))
    (if (> a b) (iter 0 a b) (iter 0 b a)))

(define (mult-fast-test a b)
    (define (iter x a b i)
        (cond   ((= b 0) i)   
                ((even? b) (iter x (double a) (halve b) (+ i 1)))
                (else (iter (+ x a) a (- b 1) (+ i 1)))))
    (if (> a b) (iter 0 a b 1) (iter 0 b a 1)))    

; Good answers, worked perfectly. 

; test zone to get back in the groove

