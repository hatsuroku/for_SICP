#lang scheme
(define (inc x) (+ x 1))
(define (square x) (* x x))
(define mod remainder)
(define (gcd a b)
  (cond [(not (and (> a 0) (> b 0))) (gcd (abs a) (abs b))]
        [(< a b) (gcd b a)]
        [(= (mod a b) 0) b]
        [else (gcd b (mod a b))]))
(define (identical x) x)
(define (k-th k lst)
  (define (iter now rest)
    (if (= now k)
        (car rest)
        (iter (+ now 1) (cdr rest))))
  (iter 1 lst))


; miller-rabin prime test
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



; 2.1
(define (make-rat n d)
  (let ([g (gcd n d)])
    (if (or (and (< n 0) (< d 0)) (and (> n 0) (< d 0)))
        (make-rat (- n) (- d))
        (cons (/ n g) (/ d g)))))

(define numer car)
(define denom cdr)

(define (print-rat x)
  (display (numer x))
  (display "/")
  (displayln (denom x)))

; (print-rat (make-rat 2 -10))


; 2.2
(define (make-segment a b)
  (cons a b))
(define start-segment car)
(define end-segment cdr)
(define (make-point x y)
  (cons x y))
(define x-point car)
(define y-point cdr)
(define (midpoint-segment seg)
  (make-point
   (/ (+ (x-point (start-segment seg)) (x-point (end-segment seg))) 2)
   (/ (+ (y-point (start-segment seg)) (y-point (end-segment seg))) 2)))

; 2.6
(define church-zero (lambda (f) (lambda (x) x)))
(define (church-add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define church-one (lambda (f) (lambda (x) (f x))))
(define church-two (lambda (f) (lambda (x) (f (f x)))))
(define (church-add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

; (((church-add-1 (church-add-1 (church-add-1  church-zero))) square) 2)
; (((church-add church-two church-two) square) 2)


; 2.17
(define (last-pair lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (last-pair (cdr lst))))
; (last-pair (list 23 72 149 34))


; 2.18
(define (reverse lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (append (reverse (cdr lst)) (list (car lst)))))
; (reverse (list 1 4 9 16 25))


; 2.19
(define (cc amount coin-values)
  (cond [(= amount 0) 1]
        [(or (< amount 0) (no-more? coin-values)) 0]
        [else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values))
                     coin-values))]))
(define (first-denomination coin-values)
  (car coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (no-more? coin-values)
  (null? coin-values))

;(cc 100 (identical (list 1 25 10 5 50)))


;2.20
(define (filter ok? seq)
  (cond [(null? seq) '()]
        [(ok? (car seq))
         (cons (car seq)
               (filter ok? (cdr seq)))]
        [else (filter ok? (cdr seq))]))
(define (same-parity first . rest)
  (cons first
        (filter (if (even? first)
              even?
              odd?)
          rest))
)

; 2.21
(define (square-list items)
  (if (null? items)
      items
      (cons (square (car items)) (square-list (cdr items)))))
(define (square-list-map items)
  (map square items))
; (square-list (list 1 2 3 4))
; (square-list-map (list 1 2 3 4))


; 2.22
(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (append answer (list (square (car things)))))))
  (iter items '()))
; (square-list-iter (list 1 2 3 4))

;2.23
(define (for-each proc lst)
  (if (null? lst)
      (void)
      ; lambda 作为顺序执行的媒介
      ((lambda ()
        (proc (car lst))
        (for-each proc (cdr lst))))))


(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))


;2.27
(define (deep-reverse lst)
  (cond ((null? lst) lst)
        ((list? (car lst)) (append (deep-reverse (cdr lst)) (list (reverse (car lst)))))
        (else (append (deep-reverse (cdr lst)) (list (car lst))))))

;2.28
(define (fringe tree)
  (cond [(null? tree) '()]
        [(not (list? (car tree))) (cons (car tree) (fringe (cdr tree)))]
        [else (append (fringe (car tree)) (fringe (cdr tree)))]))

;(define x '((1 2) (3 4)))
;(fringe (list x x))


;2.29
; 如果是通过 list 连接的话，只要更改选择函数就行了
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))
(define left-branch car)
(define right-branch cdr)
(define branch-length car)
(define branch-struct cdr)
(define (total-weight structure)
  (if (pair? structure)
      (+ (total-weight (branch-struct (left-branch structure)))
         (total-weight (branch-struct (right-branch structure))))
      structure))
(define (is-balanced? structure)
  (define (torque branch)
    (* (branch-length branch) (total-weight (branch-struct branch))))
  (= (torque (left-branch structure))
     (torque (right-branch structure))))

; (define top-struct
;  (make-mobile (make-branch 10 10)
;               (make-branch 4
;                            (make-mobile (make-branch 10 20)
;                                         (make-branch 10 5)))))
; (is-balanced? top-struct)
; (is-balanced? (branch-struct (right-branch top-struct)))


; 2.30
(define (square-tree tree)
  (cond [(null? tree) '()]
        [(pair? tree) (cons (square-tree (car tree))
                            (square-tree (cdr tree)))]
        [else (square tree)]))
(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       tree))

; (square-tree-map
;  (list 1
;        (list 2
;              (list 3 4)
;              5)
;        (list 6 7)))

; 2.31
(define (tree-map proc tree)
  (cond [(null? tree) '()]
        [(pair? tree) (cons (tree-map proc (car tree))
                            (tree-map proc (cdr tree)))]
        [else (proc tree)]))
(define (tree-map-square tree)
  (tree-map square tree))
;(tree-map-square
; (list 1
;       (list 2
;             (list 3 4)
;             5)
;       (list 6 7)))


; 2.32
(define (subsets s)
  (if (null? s)
      '(())
      (let ([rest (subsets (cdr s))])
        (append rest (map (lambda (subset)
                            (append subset (list (car s))))
                          rest)))))
;(subsets (list 1 2 3))


; filter 在 2.20 里
(define (accumulate op initial items)
  (if (null? items)
      initial
      (op (car items)
          (accumulate op initial (cdr items)))))
;(accumulate + 0 (list 1 2 3 4 5))
;(accumulate * 1 (list 1 2 3 4 5))
;(accumulate cons '() (list 1 2 3 4 5))


; 2.33
(define (acc-map proc seq)
  (accumulate (lambda (x y) (cons (proc x) y)) '() seq))
(define (acc-append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (acc-length lst)
  (accumulate (lambda (x y) (+ 1 y)) 0 lst))

; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

; 2.35
; 这个想了我好久，乱想的居然都给想出来了，草草
(define (acc-count-leaves tree)
  (accumulate +
              0
              (map (lambda (sub-tree)
                     (if (pair? sub-tree)
                         (acc-count-leaves sub-tree)
                         1))
                   tree)))

; (acc-count-leaves '(1 (2 (3 4) 5) (6 7)))

; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
;(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
;(accumulate-n + 0 s)


; 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (mv) (dot-product mv v)) m))
(define (transpose m)
  (accumulate-n cons '() m))
(define (matrix-*-matrix m n)
  (let ([cols (transpose n)])
    (map (lambda (v) (matrix-*-vector cols v)) m)))

;(define matrix '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
;(matrix-*-vector matrix '(1 1 1 1))
;(transpose matrix)
;(define a '((1 2) (3 4)))
;(define b '((1 0) (0 1)))
;(matrix-*-matrix b a)
;(matrix-*-matrix '((1 2) (3 4)) '((1) (2)))


; 2.38
(define (fold-left op initial seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial seq))
(define fold-right accumulate)

;(fold-right / 1 '(1 2 3))
;(fold-left / 1 '(1 2 3))
;(fold-right list '() '(1 2 3))
;(fold-left list '() '(1 2 3))


; 2.39
(define (fold-right-reverse seq)
  (fold-right (lambda (now acc)
                (append acc (list now)))
              '()
              seq))
(define (fold-left-rerverse seq)
  (fold-left (lambda (acc now)
               (cons now acc))
             '()
             seq))
;(fold-right-reverse '(1 2 3 4))
;(fold-left-rerverse '(1 2 3 4))


(define (enumerate-interval lo hi)
  (define (iter cur ret)
    (if (> cur hi)
        ret
        (iter (+ 1 cur) (append ret (list cur)))))
  (iter lo '()))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))
;(prime-sum-pairs 6)

(define (permutations s)
  (if (null? s)
      '(())
      (flatmap (lambda (x)
                 (map (lambda (subpermu) (cons x subpermu))
                      (permutations (remove x s))))
               s)))
(define (remove item seq)
  (filter (lambda (x) (not (= item x))) seq))
;(permutations '(1 2 3))


; 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
;(unique-pairs 10)


;2.41
(define (unique-three-tuple n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))
;(unique-three-tuple 6)
(define (three-tuple-equal s n)
  (filter (lambda (lst)
            (= (accumulate + 0 lst) s))
          (unique-three-tuple n)))
;(three-tuple-equal 10 6)



; 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
(define empty-board '())
(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))
(define (safe? k positions)
  (let ([k-item (k-th k positions)])
    (define (iter now rest)
      (if (= now k)
          #t
          (let ([now-item (car rest)])
            (if (or (= now-item k-item) (= (abs (- k-item now-item)) (abs (- k now))))
                #f
                (iter (+ now 1) (cdr rest))))))
    (iter 1 positions)))
;(queens 8)



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





(define (memq item x)
  (cond [(null? x) false]
        [(eq? item (car x)) x]
        [else (memq item (cdr x))]))


