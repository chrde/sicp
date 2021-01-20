(use-modules (srfi srfi-78) ;tests
             )
(define (inc . args) (apply 1+ args))
(define (dec . args) (apply 1- args))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) 
          (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


;; Exercise 2.1
(define (sign a b)
  (cond ((and (> a 0) (> b 0)) 1)
        ((or (> a 0) (> b 0)) -1)
        (else 1)))

(define (make-rat n d)
  (let ((s (sign n d))
        (g (abs (gcd n d))))
    (cons (/ (* s n) g) 
          (/ (abs d) g))))

(print-rat (make-rat 1 -2))
(print-rat (make-rat 6 -9))

;; Exercise 2.2
(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment s)
  (make-point 
    (/ (+ (x-point (start-segment s)) (x-point (end-segment s))) 2)
    (/ (+ (y-point (start-segment s)) (y-point (end-segment s))) 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(let* ((p1 (make-point 0 0))
       (p2 (make-point 4 6))
       (s (make-segment p1 p2)))
  (check (midpoint-segment s) => (make-point 2 3)))

;; Exercise 2.3
(define (make-rect p1 p2)
  (cons p1 p2))

(define (make-rect-wh p w h)
  (make-rect p
             (make-point (+ w (x-point p))
                         (+ h (y-point p)))))

(define (width-rect r)
  (- (x-point (cdr r))
     (x-point (car r))))

(define (height-rect r)
  (- (y-point (cdr r))
     (y-point (car r))))

(define (perimeter-rect r)
  (+ (* 2 (width-rect r))
     (* 2 (height-rect r))))

(define (area-rect r)
  (* (width-rect r)
     (height-rect r)))

(check (area-rect (make-rect (make-point 1 1) (make-point 5 5))) => 16)

;; Exercise 2.4
(define (cons1 x y) 
  (lambda (m) (m x y)))

(define (car1 z) 
  (z (lambda (p q) p)))

(define (cdr1 z) 
  (z (lambda (p q) q)))

(check (car1 (cons1 1 2)) => 1)
(check (cdr1 (cons1 1 2)) => 2)

;; Exercise 2.5
(define (pairs-ab a b)
  (define (pow b e)
    (if (zero? e)
        1
        (* b (pow b (dec e)))))
  (* (pow 2 a)
     (pow 3 b)))

(define (pairs-ab-car p)
  (define (helper n a)
    (if (odd? n)
        a
        (helper (/ n 2) (inc a))))
  (helper p 0))

(define (pairs-ab-cdr p)
  (define (helper n a)
    (if (zero? (remainder n 3))
        (helper (/ n 3) (inc a))
        a))
  (helper p 0))

(check (pairs-ab-car (pairs-ab 5 6)) => 5)
(check (pairs-ab-cdr (pairs-ab 5 6)) => 6)

;; Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add x y)
  (lambda (f) (lambda (x) ((x f) ((m f) x)))))
