(define (square x) (* x x))

(define (inc . args) (apply 1+ args))
(define (dec . args) (apply 1- args))

(define (average x y)
  (/ (+ x y) 2))

;; 1.1.7 - Square roots by newton's method
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(sqrt 0.00005)

;; 1.2.2 - Fibonacci
(define (fib-rec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-rec (- n 1))
                 (fib-rec (- n 2))))))

(define (fib-iter n)
  (define (fib a b count)
    (if (= count 0)
        b
        (fib (+ a b) a (dec count))))
  (fib 1 0 n))

;; 1.2.2 - Counting exchange
(define (count-change amount)
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0)
               (= kinds-of-coins 0))
           0)
          (else
            (+ (cc amount (dec kinds-of-coins))
               (cc (- amount (first-denomination kinds-of-coins))
                   kinds-of-coins)))))
  (cc amount 5))


;; Exercise 1.7
(define (sqrt1 x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (improve guess) guess))
       (* guess 0.001)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(sqrt1 0.00005)

;; Exercise 1.8
(define (cube-root x)
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (good-enough? guess)
    (< (abs (- (improve guess) guess))
       (* guess 0.001)))
  (define (cube-root-iter guess)
    (if (good-enough? guess)
        guess
        (cube-root-iter (improve guess))))
  (cube-root-iter 1.0))

(cube-root 27)

;; Exercise 1.9
(define (plus_rec a b)
  (if (= a 0)
      b
      (inc (plus_rec (dec a) b))))

(define (plus_iter a b)
  (if (= a 0)
      b
      (plus_iter (dec a) (inc b))))

,trace (plus_iter 10 1)

,trace (plus_rec 10 1)

;; Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

,trace (A 1 10)

,trace (A 2 4)

,trace (A 3 3)

;; Exercise 1.11
(define (f-recur n)
  (if (< n 3)
      n
      (+ (f-recur (dec n))
         (* 2 (f-recur (- n 2)))
         (* 3 (f-recur (- n 3))))))

(f-recur 10)

(define (f-iter n)
  (define (f n1 n2 n3 count)
    (if (= n count)
        n1
        (f n2 n3 (+ n3 (* 2 n2) (* 3 n1)) (inc count))))
  (f 0 1 2 0))

(f-iter 10)
