(use-modules (srfi srfi-78))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch 
                  (car bits) 
                  current-branch)))
          (if (leaf? next-branch)
              (cons 
                (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
          (cons (car set)
                (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
          (make-leaf (car pair)    ; symbol
                     (cadr pair))  ; frequency
          (make-leaf-set (cdr pairs))))))

;; Exercise 2.67
(define sample-tree
  (make-code-tree 
    (make-leaf 'A 4)
    (make-code-tree
      (make-leaf 'B 2)
      (make-code-tree 
        (make-leaf 'D 1)
        (make-leaf 'C 1)))))

sample-tree

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(check (decode sample-message sample-tree) => '(A D A B B C A))

;; Exercise 2.68
(define (encode-symbol s tree)
  (if (leaf? tree)
      (if (eq? s (symbol-leaf tree))
          '()
          (error "bad symbol: ENCODE-SYMBOL: " s))
      (if (memq s (symbols (left-branch tree)))
          (cons 0 (encode-symbol s (left-branch tree)))
          (cons 1 (encode-symbol s (right-branch tree))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message)
                             tree)
              (encode (cdr message) tree))))

(check (decode (encode '(A D A B B C A) sample-tree) sample-tree) => '(A D A B B C A))

;; Exercise 2.69
(define (successive-merge set)
  (if (null? (cdr set))
      (car set)
      (successive-merge (adjoin-set (make-code-tree (car set)
                                                    (cadr set))
                                    (cddr set)))))

(define (generate-huffman-tree pairs)
  (successive-merge 
    (make-leaf-set pairs)))

(define x (make-leaf-set '((D 1) (C 1) (B 2) (A 4))))

;; Exercise 2.70
(define song-tree (generate-huffman-tree '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))
(define song '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(encode song song-tree)
