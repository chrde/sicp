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

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product 
             (multiplier exp)
             (deriv (multiplicand exp) var))
           (make-product 
             (deriv (multiplier exp) var)
             (multiplicand exp))))
        (else (error "unknown expression 
                      type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

;; Exercise 2.56
(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        (else (list '** base exp))))

(define (base x)
  (cadr x))
(define (exponent x)
  (caddr x))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product 
             (multiplier exp)
             (deriv (multiplicand exp) var))
           (make-product 
             (deriv (multiplier exp) var)
             (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (make-sum -1 (exponent exp))))
                       (deriv (base exp) var)))
        (else (error "unknown expression 
                      type: DERIV" exp))))

(deriv '(** (* 5 x) 2) 'x)

;; Exercise 2.57
(define (augend s) (accumulate make-sum 0 (cddr s)))
(define (multiplicand s) (accumulate make-product 0 (cddr s)))

(deriv '(* x y (+ x 3)) 'x)

;; Exercise 2.58
(define *operators* 
  '((+ 1 left)
    (- 1 left)
    (* 2 left)
    (** 3 right)))
(define *non-op* '(any 10 'left))
(define (op? x)
  (eq? (car (find-op x)) x))
(define (find-op x)
  (define (iter ops)
    (cond ((null? ops) *non-op*)
          ((eq? (caar ops) x) (car ops))
          (else (iter (cdr ops)))))
  (iter *operators*))
(define (associativity x)
  (caddr (find-op x)))
(define (precedence x)
  (cadr (find-op x)))

(define (lower-prec? op ops)
  (if (null? ops)
      #f
      (< (precedence op) (precedence (car ops)))))

(define (apply-op values op)
  (let ((first (car values))
        (second (cadr values))
        (rest (cddr values)))
    (cons (list second op first) rest)))

(define (apply-rec values ops)
  (if (null? ops)
      values
      (apply-rec (apply-op values (car ops))
                 (cdr ops))))

(define (shunting-yard values ops expr)
  (if (null? expr)
      (apply-rec values ops)
      (let* ((next (car expr))
             (rest (cdr expr)))
        (cond
          [(op? next)
           (if (lower-prec? next ops)
               (shunting-yard (apply-op values (car ops))
                              (cons next (cdr ops))
                              rest)
               (shunting-yard values
                              (cons next ops)
                              rest))]
          [(not (pair? next))
           (shunting-yard (cons next values) ops rest)]
          [else 
            (shunting-yard (cons (ast next) values)
                           ops
                           rest)]))))

(define (ast expr)
  (car (shunting-yard '() '() expr)))

;; todo associativity
