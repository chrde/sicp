(use-modules 
  (srfi srfi-1)
  (srfi srfi-78) ;tests
  )
(define (inc . args) (apply 1+ args))
(define (dec . args) (apply 1- args))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define true #t)
(define false #f)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))

;; sorted
(define (element-of-sset? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-sset set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set 
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set 
                          (cdr set1) 
                          set2))
              ((< x2 x1) (intersection-set 
                          set1 
                          (cdr set2)))))))

;; tree
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-tset? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? 
          x 
          (left-branch set)))
        ((> x (entry set))
         (element-of-set? 
          x 
          (right-branch set)))))

(define (adjoin-tset x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree 
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

;; Exercise 2.59
(define (union-set set1 set2)
  (accumulate adjoin-set
              set1 set2))

(check (union-set '(1 2 3) '(4 5 6)) => '(4 5 6 1 2 3))

;; Execise 2.60
(define (adjoin-set1 x set)
  (cons x set))

(define (union-set1 set1 set2)
  (append set1 set2))

;; Exercise 2.61
(define (adjoin-sset? x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-sset? x (cdr set))))))

(check (adjoin-sset? 3 '(1 4 5)) => '(1 3 4 5))
(check (adjoin-sset? 4 '(1 4 5)) => '(1 4 5))
(check (adjoin-sset? 6 '(1 4 5)) => '(1 4 5 6))

;; Exercise 2.62
(define (union-sset set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((> (car set1) (car set2))
         (cons (car set2) (union-sset set1 (cdr set2))))
        ((< (car set1) (car set2))
         (cons (car set1) (union-sset (cdr set1) set2)))
        (else (cons (car set1) (union-sset (cdr set1) (cdr set2))))))

(check (union-sset '(1 3 5) '(2 4 6)) => '(1 2 3 4 5 6))
(check (union-sset '() '(2)) => '(2))
(check (union-sset '(1) '()) => '(1))
(check (union-sset '(2 4 6) '(2 4 6)) => '(2 4 6))

;; Exercise 2.65
(define (union-tset set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (list->tree (union-sset (tree->list set1)
                                      (tree->list set2))))))

(check (union-tset (list->tree '(1 3 5)) (list->tree '(2 4 6))) => (list->tree '(1 2 3 4 5 6)))
(check (union-tset (list->tree '()) (list->tree '(2))) => (list->tree '(2)))
(check (union-tset (list->tree '(1)) (list->tree '())) => (list->tree '(1)))
(check (union-tset (list->tree '(2 4 6)) (list->tree '(2 4 6))) => (list->tree '(2 4 6)))

;; Exercise 2.66
(define (key record) record)
(define (lookup-tset x set)
  (cond ((null? set) false)
        ((= x (key (entry set))) (entry set))
        ((< x (key (entry set)))
         (lookup-tset 
           x 
           (left-branch set)))
        ((> x (key (entry set)))
         (lookup-tset 
           x 
           (right-branch set)))))

(check (lookup-tset 3 (list->tree '(1 3 5))) => 3)
