#lang scheme

(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp) (if (same-variable? exp var)
                             1
                             0)]
        [(sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var))]
        [(product? exp) (make-sum (make-product (multiplier exp)
                                                (deriv (multiplicand exp) var))
                                  (make-product (deriv (multiplier exp) var)
                                                (multiplicand exp)))]
        [(exponentiation? exp) (make-product (make-product (exponent exp)
                                                           (make-exponentiation (base exp)
                                                                                (- (exponent exp) 1)))
                                             (deriv (base exp) var))]
        [else (error "unknown expression type -- DERIV" exp)]))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;(define (make-sum a1 a2) (list '+ a1 a2))
(define (=number? exp num) (and (number? exp) (= exp num)))
;(define (make-sum a1 a2)
;  (cond [(=number? a1 0) a2]
;        [(=number? a2 0) a1]
;        [(and (number? a1) (number? a2) (+ a1 a2))]
;        [else (list '+ a1 a2)]))
; 2.57
(define (make-sum now . rest)
  (if (null? rest)
      now
      (let ([rest-sum (apply make-sum rest)])
        (cond ([=number? now 0] rest-sum)
              ([=number? rest-sum 0] now)
              [(and (number? now) (number? rest-sum)) (+ now rest-sum)]
              [else (list '+ now rest-sum)]))))
                        
;(define (make-product m1 m2) (list '* m1 m2))
;(define (make-product m1 m2)
;  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
;        [(=number? m1 1) m2]
;        [(=number? m2 1) m1]
;        [(and (number? m1) (number? m2)) (* m1 m2)]
;        [else (list '* m1 m2)]))
; 2.57
(define (make-product now . rest)
  (cond [(null? rest) now]
        [(=number? now 0) 0]
        [else (let ([rest-product (apply make-product rest)])
                (cond [(=number? rest-product 0) 0]
                      [(=number? now 1) rest-product]
                      [(=number? rest-product 1) now]
                      [(and (number? now) (number? rest-product)) (* now rest-product)]
                      [else (list '* now rest-product)]))]))
    
      
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
; 2.56
(define (make-exponentiation base exponent)
  (cond [(= exponent 0) 1]
        [(= exponent 1) base]
        [else (list 'exponent base exponent)]))
(define (base exponentiation)
  (cadr exponentiation))
(define (exponent exponentiation)
  (caddr exponentiation))
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) 'exponent)))

;(deriv (make-product 'x (make-sum 'x 1)) 'x)