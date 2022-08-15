#lang racket

(define (square x) (* x x))
(define (cube x) (* x x x))
(define (average x y) (/ (+ x y) 2))
(define mod remainder)
(define (remainder-two-equal x remain)
  (if (= (remainder x 2) remain)
      #t
      #f))
(define (even? x) (remainder-two-equal x 0))
(define (odd? x) (remainder-two-equal x 1))
(define (divides? divisor num)
  (= (mod num divisor) 0))
(define (inc x) (+ 1 x))
(define (identity x) x)
(define (gcd a b)
  (cond [(not (and (> a 0) (> b 0))) (gcd (abs a) (abs b))]
        [(< a b) (gcd b a)]
        [(= (mod a b) 0) b]
        [else (gcd b (mod a b))]))
(define esp 0.00001)
(define (abslt a b y) (< (abs (- a b)) y))
          
      
(define (for-range proc start end)
    (if (= start end)
        (void)
        ((lambda ()
            (proc start)
            (for-range proc (+ start 1) end)))))

(define (for-list proc lst)
  (if (null? lst)
      (void)
      ((lambda ()
         (proc (car lst))
         (for-list proc (cdr lst))))))

(define (range-list start end)
  (define (iter now end lst)
    (if (= now end)
        lst
        (iter (+ now 1) end (append lst (list now)))))
  (iter start end '()))


; 1.3
(define (big-two-sum a b c)
    (define sum (+ a b c))
    (max (- sum a) (- sum b) (- sum c)))

; 1.8
(define (cube-good-enough? guess x)
    (< (abs (- (cube guess) x)) 0.001))

(define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-root-iter guess x)
    (if (cube-good-enough? guess x)
        guess
        (cube-root-iter (improve guess x) x)))

(define (cube-root x)
    (cube-root-iter 1.0 x))



;;; (define test
;;;     (for-range
;;;         (lambda (x) (writeln (cube (cube-root x))))
;;;         1
;;;         10))

; 1.16
(define (fast-expt x n)
  (cond [(= n 0) 1]
        [(even? n) (square (fast-expt x (/ n 2)))]
        [else (* x (fast-expt x (- n 1)))]))

(define (fast-exp x n)
  (define (iter now x n)
    (cond [(= n 0) now]
          [(even? n) (iter now (square x) (/ n 2))]
          [else (iter (* x now) x (- n 1))]))
  (iter 1 x n))

;(for-range
; (lambda (x) (writeln (fast-exp x 10)))
; 1
; 10)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n divisor)
  (cond [(> (square divisor) n) n]
        [(divides? divisor n) divisor]
        [else (find-divisor n (+ 1 divisor))]))

(define (is-prime? x)
  (= (smallest-divisor x) x))


(define (expmod base exp m)
  (define (iter now base exp m)
    (cond [(= 0 exp) now]
          [(even? exp) (iter now (mod (square base) m) (/ exp 2) m)]
          [else (iter (mod (* now base) m) base (- exp 1) m)]))
  (iter 1 base exp m))

(define (fermat-test n)
  (define (try a)
    (= (expmod a n n) a))
  (try (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond [(= times 0) true]
        [(fermat-test n) (fast-prime? n (- times 1))]
        [else #f]))

;(define carmichael 1105)
;(for-list
; (lambda (carmichael)
;   (displayln "\n\n\n\n\n*****")
;   (for-list
;    (lambda (x) (write x) (display "  ") (writeln (= (expmod x carmichael carmichael) x)))
;    (range-list 0 carmichael)))
; '(561 1105 1729 2465 2821 6601))


; 1.27
(define (all-same-remain num)
  (define (iter now)
    (cond [(= now num) #t]
          [(= (expmod now num num) now) (iter (+ 1 now))]
          [else #f]))
  (iter 1))

; 1.27 test
;(for-list
; (lambda (x)
;   (display x)
;   (display "  ")
;   (display (all-same-remain x))
;   (newline)
; )
; '(561 1105 1729 2465 2821 6601))



; 1.28
(define (miller-expmod base exp m)
  (define (iter now base exp m)
    (cond [(= 0 exp) now]
          [(even? exp) ((lambda ()
                          (define square-base-mod (mod (square base) m))
                          (if (and (= square-base-mod 1)
                                   (not (or (= base 1)
                                            (= base (- m 1)))))
                              -1
                              (iter now square-base-mod (/ exp 2) m))
                         ))]
          [else (iter (mod (* now base) m) base (- exp 1) m)]))
  (iter 1 base exp m))

(define (miller-rabin-test n)
  (define (try a)
    (define res (miller-expmod a (- n 1) n))
    (= res 1))
  (try (random 2 (- n 1))))

(define (miller-fast-prime? n times)
  (cond [(or (= n 2) (= n 3)) #t]
        [(= times 0) #t]
        [(miller-rabin-test n) (miller-fast-prime? n (- times 1))]
        [else #f]))

(define (prime? n)
  (miller-fast-prime? n (exact-ceiling (log n))))

; 1.28 test
; (for-range
;  (lambda (x)
;    (if
;     (miller-fast-prime? x (exact-ceiling (log x)))
;     (displayln x)
;     (void))
;  )
;  2
;  100)
; (displayln "****")
; (for-range
;  (lambda (x)
;    (if
;     (is-prime? x)
;     (displayln x)
;     (void))
;  )
;  2
;  100)

; 1.30
(define (my-sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; 1.31
(define (my-product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))
(define pi
  (let ((up (my-product
            (lambda(x)
            (if (even? x)
                (+ 2 x)
                (+ 1 x)))
            1
            inc
            10000))
        (down (my-product
               (lambda(x)
               (if (even? x)
                   (+ 1 x)
                   (+ x 2)))
               1
               inc
               10000)))
  (* 4 (/ up down))))


; 1.32
(define (accumulate combiner unit-val term next a b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a unit-val))
(define (sum term a next b)
  (accumulate + 0 term next a b))
(define (product term a next b)
  (accumulate * 1 term next a b))

; 1.33
(define (filtered-accumulate combiner unit-val term next filter a b)
  (define (iter a result)
    (cond [(> a b) result]
          [(filter (term a)) (iter (next a) (combiner result (term a)))]
          [else (iter (next a) (combiner result unit-val))]))
  (iter a unit-val))
(define (prime-sum a b)
  (filtered-accumulate + 0 identity inc prime? a b))
(define (coprime? a b)
  (= (gcd a b) 1))
(define (coprime-sum n)
  (filtered-accumulate + 0 identity inc
                       (lambda(x) (coprime? n x))
                       1 n))


(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) esp))
  (define (try guess)
    (let ([next (f guess)])
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; 1.35
(define fi
  (fixed-point (lambda(x) (+ 1.0 (/ 1.0 x))) 1))

; 1.36
(define (print-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) esp))
  (define (try guess)
    (displayln guess)
    (let ([next (f guess)])
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; (define log1000-fixed-point
;   (print-fixed-point (lambda(x) (/ (log 1000) (log x))) 2))
; (newline)
; (displayln "****")
; (define log1000-fixed-point-average
;   (print-fixed-point (lambda(x) (/ (+ x (/ (log 1000) (log x))) 2)) 2))



; 1.37
(define (cont-frac n d k)
  (define (iter i res)
    (if (= i 0)
        res
        (iter (- i 1) (/ (n i) (+ (d i) res)))))
  (iter k 0))
(define (cont-frac-recur n d k)
  (define (recu i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (recu (+ i 1))))))
  (recu 1))

;(cont-frac (lambda(i) 1.0) (lambda(i) 1.0) 10000)
;(cont-frac-recur (lambda(i) 1.0) (lambda(i) 1.0) 10000)

; 1.38
; (cont-frac
;  (lambda(i) 1.0)
;  (lambda(i)
;    (if (= (mod i 3) 2)
;        (* (+ (quotient i 3) 1) 2)
;        1))
;  10000)


; 1.39
(define (cont-frac-minus n d k)
  (define (iter i res)
    (if (= i 0)
        res
        (iter (- i 1) (/ (n i) (- (d i) res)))))
  (iter k 0))

(define (tan-cf x k)
  (cont-frac-minus
   (lambda (i) (square x))
   (lambda (i) (- (* i 2) 1))
   k))



(define (average-damp f)
  (lambda(x) (average x (f x))))

(define (damp-sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (newton-sqrt x)
  (newton-method (lambda(y) (- (square y) x))
                 1.0))

; 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))


; 1.43
(define (repeated-recur f times)
  (if (= times 1)
      f
      (compose f (repeated-recur f (- times 1)))))
(define (repeated f times)
  (define (iter cur times)
    (if (= times 1)
        cur
        (iter (compose f cur) (- times 1))))
  (iter f times))


; 1.44
(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
    (/ (+ (f x) (f (- x dx)) (f (+ x dx))) 3)))

(define (smooth-n f n)
  (repeated smooth n) f)


; 1.45



; 1.46
(define (iterative-improve good-enough? next)
  (lambda (x)
    (define (improve x)
      (if (good-enough? x)
          x
          (improve (next x))))
    (improve x)))

(define (improve-sqrt x)
  ((iterative-improve
   (lambda (y) (abslt (square y) x 0.00001))
   (lambda (y) (average y (/ x y))))
   x))
(define (improve-fixed-point f first-guess)
  ((iterative-improve
    (lambda (x) (abslt x (f x) 0.00001))
    f)
   first-guess))
   