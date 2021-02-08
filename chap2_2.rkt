#lang sicp

(#%require sicp-pict)

(#%require rackunit)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter pred seq)
  (cond ((null? seq) nil)
        ((pred (car seq))
         (cons (car seq)
               (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (remove item seq)
  (filter (lambda (x) (not (= x item)))
          seq))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low 
            (enumerate-interval (inc low) high))))

;; Exercise 2.17
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(check-equal? (last-pair '(1)) '(1))

(check-equal? (last-pair '(1 2 3)) '(3))

(define (map1 proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;; Exercise 2.18
(define (reverse l)
  (define (iter acc rem)
    (if (null? rem)
        acc
        (iter (cons (car rem) acc)
              (cdr rem))))
  (iter '() l))

(check-equal? (reverse '()) '())
(check-equal? (reverse '(1)) '(1))
(check-equal? (reverse '(1 2 3)) '(3 2 1))

(reverse '(1 2 3))

;; Exercise 2.19
(define (cc amount coin-values)
  (define (no-more? l) (null? l))
  (define (except-first-denomination l) (cdr l))
  (define (first-denomination l) (car l))
  (cond ((= amount 0) 1)
        ((or (< amount 0) 
             (no-more? coin-values)) 
         0)
        (else
          (+ (cc 
               amount
               (except-first-denomination 
                 coin-values))
             (cc 
               (- amount
                  (first-denomination 
                    coin-values))
               coin-values)))))

(check-equal? (cc 100 (list 50 25 10 5 1)) 292)
(check-equal? (cc 100 (list 25 50 10 5 1)) 292)

;; Exercise 2.20
(define (same-parity x . y)
  (define check? (if (even? x) even? odd?))
  (define (iter acc rem)
    (cond ((null? rem) (reverse acc))
          ((check? (car rem)) (iter (cons (car rem) acc) (cdr rem)))
          (else (iter acc (cdr rem)))))
  (iter (list x) y))

(check-equal? (same-parity 1 2 3 4 5 6 7) '(1 3 5 7)) 
(check-equal? (same-parity 2 3 4 5 6 7) '(2 4 6)) 


;; Exercise 2.21
(define (square x) (* x x))
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list1 items)
  (map square items))

(check-equal? (square-list (list 1 2 3 4)) (list 1 4 9 16))
(check-equal? (square-list1 (list 1 2 3 4)) (list 1 4 9 16))

;; Exercise 2.23
(define (for-each proc items)
  (map proc items)
  #t)

;; Exercise 2.25
(check-equal? 7 (car (cdr (car (cdr (cdr '(1 3 (5 7) 9)))))))
(check-equal? 7 (car (car '((7)))))
(check-equal? 7 (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7)))))))))))))))))))

;; Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons x y)
(list x y)

;; Exercise 2.27
(define (deep-reverse l)
  (define (iter acc rem)
    (cond [(null? rem) acc]
          [(not (pair? rem)) rem]
          [else (iter
                  (cons (deep-reverse (car rem)) acc)
                  (cdr rem))]))
  (iter '() l))

(check-equal? (deep-reverse (list (list 1 2) (list 3 4))) '((4 3) (2 1)))


;; Exercise 2.28
(define (fringe-rec l)
  (cond [(null? l) '()]
        [(not (pair? l)) (list l)]
        [else (append (fringe-rec (car l))
                      (fringe-rec (cdr l)))]))

(define (fringe-iter l)
  (define (iter acc rem)
    (cond [(null? rem) acc]
          [(not (pair? rem)) (cons rem acc)]
          [else (iter (iter acc (cdr rem))
                      (car rem))]))
  (iter '() l))

(check-equal? (fringe-iter '(1 (((2))) (3) ((4 (((5))))))) '(1 2 3 4 5))
(check-equal? (fringe-rec '(1 (((2))) (3) ((4 (((5))))))) '(1 2 3 4 5))

;; Exercise 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (left-branch m)
  (car m))
(define (right-branch m)
  (car (cdr m)))

(define (branch-length b)
  (car b))
(define (branch-structure b)
  (car (cdr b)))

(define (branch-weight b)
  (let ([str (branch-structure b)])
    (if (not (pair? str))
        str
        (+ (branch-weight (left-branch str))
           (branch-weight (right-branch str))))))

(define (total-weight m)
  (define (branch-weight b)
    (let ([str (branch-structure b)])
      (if (not (pair? str))
          str
          (total-weight str))))
  (+ (branch-weight (left-branch m))
     (branch-weight (right-branch m))))

(check-equal? (total-weight (make-mobile (make-branch 1 2)
                                         (make-branch 3 
                                                      (make-mobile (make-branch 4 5)
                                                                   (make-branch 6 8)))))
              15)

(define (torque b)
  (* (branch-length b)
     (if (pair? (branch-structure b))
         (total-weight (branch-structure b))
         (branch-structure b))))

(check-equal? (torque (make-branch 4 5)) 20)

(define (mobile-balanced? m)
  (define (branch-balanced? b)
    (let ([str (branch-structure b)])
      (if (not (pair? str))
          #t
          (mobile-balanced? str))))
  (and (= (torque (left-branch m)) (torque (right-branch m)))
       (branch-balanced? (left-branch m))
       (branch-balanced? (right-branch m))))

 (define m1 (make-mobile 
             (make-branch 4 6) 
             (make-branch 5 
                          (make-mobile 
                           (make-branch 3 7) 
                           (make-branch 9 8)))))

(check-false (mobile-balanced? m1))

 (define m2 (make-mobile 
             (make-branch 4 6) 
             (make-branch 2 
                          (make-mobile 
                           (make-branch 5 8) 
                           (make-branch 10 4)))))

(check-true (mobile-balanced? m2))

;; Exercise 2.30
(define (square-tree-direct tree)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (* tree tree))
        (else
          (cons (square-tree-direct (car tree))
                (square-tree-direct (cdr tree))))))

(check-equal? (square-tree-direct (list 1 
                         (list 2 (list 3 4) 5) 
                         (list 6 7)))
              '(1 (4 (9 16) 25) (36 49)))

(define (square-tree-map tree)
  (map (lambda (tree)
         (if (pair? tree)
             (square-tree-map tree)
             (* tree tree)))
       tree))

(check-equal? (square-tree-map (list 1 
                                     (list 2 (list 3 4) 5) 
                                     (list 6 7)))
              '(1 (4 (9 16) 25) (36 49)))

;; Exercise 2.31
(define (tree-map fn t)
  (map (lambda (l)
         (if (pair? l)
             (tree-map fn l)
             (fn l)))
       t))

(check-equal? (tree-map
                (lambda (x) (* x x))
                (list 1 
                      (list 2 (list 3 4) 5) 
                      (list 6 7)))
              '(1 (4 (9 16) 25) (36 49)))

;; Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets '(1 2))

;; Exercise 2.33
(define (map-accum p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil
              sequence))

(define (append-accum seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-accum seq)
  (accumulate (lambda (x y) (+ y 1))
              0 seq))

(check-equal? (map-accum (lambda (x) (+ x 1)) '(1 2 3 4)) '(2 3 4 5))
(check-equal? (append-accum '(1 2) '(3 4)) '(1 2 3 4))
(check-equal? (length-accum '(1 4 8)) 3)

;; Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate
    (lambda (this-coeff higher-terms)
      (+ this-coeff (* higher-terms x)))
    0
    coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;; Exercise 2.35
(define (count-leaves t)
  (accumulate + 0
              (map
                (lambda (t)
                  (cond [(null? t) 0]
                        [(not (pair? t)) 1]
                        [else (count-leaves t)]))
                t)))

  
(check-equal? (count-leaves '(1 (((2 (((3 4)))))))) 4)

;; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

;; Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v))
       m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector m x)) m)))

(define matrix '((1 2 3 4)
                 (5 6 7 8)
                 (9 10 11 12)))

(check-equal? (matrix-*-vector matrix '(2 3 4 5)) '(40 96 152))
(check-equal? (transpose matrix) '((1 5 9) (2 6 10) (3 7 11) (4 8 12)))
(check-equal? (transpose (transpose matrix)) matrix)
(check-equal?(matrix-*-matrix matrix matrix) '((30 70 110) (70 174 278) (110 278 446)))

;; Exercie 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(accumulate / 1 (list 1 2 3))

(fold-left  / 1 (list 1 2 3))

(accumulate list nil (list 1 2 3))

(fold-left  list nil (list 1 2 3))

;; Exercise 2.39
(define (reverse-right sequence)
  (accumulate
    (lambda (x y) (append y (list x)))
    nil sequence))

(define (reverse-left sequence)
  (fold-left
    (lambda (x y) (cons y x))
    nil sequence))

(check-equal? (reverse-right '(1 2 3)) '(3 2 1))
(check-equal? (reverse-left '(1 2 3)) '(3 2 1))

;; Exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (x)
             (map (lambda (y)
                    (list x y))
                  (enumerate-interval 1 x)))
           (enumerate-interval 1 n)))

(check-equal? (unique-pairs 3) '((1 1) (2 1) (2 2) (3 1) (3 2) (3 3)))

;; Exercise 2.41
(define (unique-triples n)
  (flatmap (lambda (x)
             (flatmap (lambda (y)
                        (map (lambda (z)
                               (list x y z))
                             (enumerate-interval 1 y)))
                      (enumerate-interval 1 x)))
           (enumerate-interval 1 n)))

(define (triples n s)
  (define (valid-triple t)
    (> s (accumulate + 0 t)))
  (filter valid-triple (unique-triples n)))

(check-equal? (triples 4 7) '((1 1 1) (2 1 1) (2 2 1) (2 2 2) (3 1 1) (3 2 1) (4 1 1)))

;; Exercise 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions)
            (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (dec k))))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  (cons (cons new-row k) rest-of-queens))

(define empty-board '())

;; It takes advantage of knowing that the 'last' queen is always (car positions)
;; ignoring `k`
(define (safe? k positions)
  (define (all-t x y) (and x y))
  (accumulate all-t #t (map (lambda (x)
                              (not (collides? (car positions) x)))
                            (cdr positions))))

(define (collides? k1 k2)
  (let ((row (abs (- (car k1) (car k2))))
        (column (abs (- (cdr k1) (cdr k2)))))
    (cond ((zero? row) #t)
          ((zero? column ) #t)
          ((= row column) #t)
          (else #f))))

(check-true (collides? (cons 1 4) (cons 1 5))) ;;same row
(check-true (collides? (cons 4 1) (cons 2 1))) ;;same column

(check-false (collides? (cons 2 4) (cons 1 6))) ;;different row
(check-false (collides? (cons 2 4) (cons 3 1))) ;;different column

(check-true (collides? (cons 4 6) (cons 2 4))) ;;same diagonal
(check-true (collides? (cons 4 6) (cons 2 8))) ;;same diagonal
(check-true (collides? (cons 4 6) (cons 6 8))) ;;same diagonal
(check-true (collides? (cons 4 6) (cons 7 3))) ;;same diagonal

(check-false (collides? (cons 4 6) (cons 3 4))) ;;different diagonal
(check-false (collides? (cons 4 6) (cons 3 8))) ;;different diagonal
(check-false (collides? (cons 4 6) (cons 7 8))) ;;different diagonal
(check-false (collides? (cons 4 6) (cons 7 2))) ;;different diagonal

;; (queens 6)

;; Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

; I cant find the painter `wave` or `rogers`....
; (paint (up-split einstein 4))

;; Exercise 2.45
(define (split first second)
  (define (helper painter n)
    (if (= n 0)
        painter
        (let ((smaller (helper painter (dec n))))
          (first painter (second smaller smaller))))))

;; I am skipping the rest of this chapter... I cant run DrRacket
