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



(define (memq item x)
  (cond [(null? x) false]
        [(eq? item (car x)) x]
        [else (memq item (cdr x))]))

        