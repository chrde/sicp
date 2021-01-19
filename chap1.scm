(use-modules (srfi srfi-78) ;tests
             (srfi srfi-19) ;time
             (ice-9 format))
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

;; 1.2.5 - Euclid's algorithm
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; 1.2.6 - Testing for primality
(define (prime? n)
  (define (smallest-divisor n)
    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (inc test-divisor)))))
    (define (divides? a b)
      (= (remainder b a ) 0))
    (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (fast-prime? n times)
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder (square (expmod base (/ exp 2) m))
                      m))
          (else
            (remainder (* base (expmod base (dec exp) m))
                       m))))
  (define (fermat-test n)
    (define (try-it a)
      (= (expmod a n n) a))
    (try-it (inc (random (dec n)))))
  (cond ((= times 0) #t)
        ((fermat-test n)
         (fast-prime? n (dec times)))
        (else #f)))

;; 1.3 - Integral
(define (cube x) (* x x x))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; 1.3.3 - Half-interval method
(define (search f neg-point pos-point)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  (let ((midpoint
          (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond
            ((positive? test-value) (search f neg-point midpoint))
            ((negative? test-value) (search f midpoint pos-point))
            (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value)
                (positive? b-value))
           (search f a b))
          ((and (negative? b-value)
                (positive? a-value))
           (search f b a))
          (else (error "Values are not of opposite sign" a b)))))

;; 1.3.3 Fixed-point
(define tolerance 0.00001)

(define debug #f)
(define (print value)
  (when debug
    (display value)
    (newline)))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (print guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; 1.3.4 Derivative
(define dx 0.0001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (sqrt2 x)
  (newtons-method
    (lambda (y)
      (- (square y) x))
    1.0))

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

;; Exercise 1.12
(define (pascal-triangle row col)
  (cond ((or (= 0 row) (= 0 col) (= row col)) 1)
        (else (+ (pascal-triangle (dec row) col)
                 (pascal-triangle (dec row) (dec col))))))

(check (pascal-triangle 0 0) => 1)
(check (pascal-triangle 0 5) => 1)
(check (pascal-triangle 5 0) => 1)
(check (pascal-triangle 5 5) => 1)

(check (pascal-triangle 4 3) => 4)
(check (pascal-triangle 4 2) => 6)

;; Exercise 1.16
(define (iter-expt b n)
  (define (iter-expt-iter acc b n)
    (cond ((= n 0) acc)
          ((even? n)
           (iter-expt-iter acc (square b) (/ n 2)))
          (else
            (iter-expt-iter (* acc b) b (dec n)))))
  (iter-expt-iter 1 b n))

(check (iter-expt 2 8) => 256)
(check (iter-expt 2 11) => 2048)
(check (iter-expt 3 7) => 2187)

;; Exercise 1.17
(define (fast-mult a b)
  (define (double n) (+ n n))
  (define (halve n) (/ n 2))
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (dec b))))))

(check (fast-mult 12 10) => 120)

;; Exercise 1.18
(define (iter-mult b n)
  (define (double n) (+ n n))
  (define (halve n) (/ n 2))
  (define (iter-mult-iter acc b n)
    (cond ((= n 0) acc)
          ((even? n)
           (iter-mult-iter acc (double b) (halve n)))
          (else
            (iter-mult-iter (+ acc b) b (dec n)))))
  (iter-mult-iter 0 b n))

(check (iter-mult 2 8) => 16)
(check (iter-mult 2 11) => 22)
(check (iter-mult 3 7) => 21)

;; Exercise 1.19
;; ,trace does not show functions defined within functions
(define (fast-fib n)
  (fast-fib-iter 1 0 0 1 n))
(define (fast-fib-iter a b p q count)
  (cond ((= count 0) 
         b)
        ((even? count)
         (fast-fib-iter a
                        b
                        (+ (* p p) (* q q))
                        (+ (* 2 p q) (* q q))
                        (/ count 2)))
        (else 
          (fast-fib-iter (+ (* b q) 
                            (* a q) 
                            (* a p))
                         (+ (* b p) 
                            (* a q))
                         p
                         q
                         (- count 1)))))
,trace (fast-fib 12)

;; Exercise 1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;; Exercise 1.22
(define (timed-prime-test fn n)
  (define (report-prime elapsed-time)
    (format #t "~%~a *** ~a(ms)" n (exact->inexact (/ (time-nanosecond elapsed-time) 1000000))))
  (define (start-prime-test n start-time)
    (when (fn n)
      (report-prime (time-difference (current-time) start-time))))
  (start-prime-test n (current-time)))

(define (search-for-primes fn min max)
  (define (iter current)
    (when (<= current max)
      (timed-prime-test fn current)
      (iter (+ current 2))))
  (iter (if (odd? min) min (inc min)))
  (newline))

(search-for-primes prime? 1000000000000000 1000000000000063)

;; Exercise 1.23
(define (prime1? n)
  (define (next n) (if (= n 2) 3 (+ n 2)))
  (define (smallest-divisor n)
    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (next test-divisor)))))
    (define (divides? a b)
      (= (remainder b a ) 0))
    (find-divisor n 2))
  (= n (smallest-divisor n)))

; Actually, it is slower??? wtf
(search-for-primes prime1? 1000000000000000 1000000000000063)

;; Exercise 1.24
(search-for-primes (lambda (p) (fast-prime? p 100)) 1000000000000000 1000000000000063)

;; Exercise 1.27
(define (all-primes? ns)
  (cond ((null? ns) #t)
        ((not (fast-prime? (car ns) 10000)) #f)
        (else (all-primes? (cdr ns)))))

(all-primes? '( 561 1105 1729 2465 2821 6601))

;; Exercise 1.28
(define (miller-rabin? n)
  (define (non-trivial-sqrt? x n)
    (cond ((= x 1) #f)
          ((= x (dec n)) #f)
          (else (= (remainder (square x) n) 1))))
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (let ((x (expmod base (/ exp 2) m)))
             (if (non-trivial-sqrt? x m)
                 0
                 (remainder (square x) m))))
          (else
            (remainder (* base (expmod base (dec exp) m))
                       m))))
  (define (miller-rabin-test a n)
    (cond ((= a 0) #t)
          ((= (expmod a (dec n) n) 1)
           (miller-rabin-test (dec a) n))
          (else #f)))
  (miller-rabin-test (dec n) n))

(check (miller-rabin? 561) => #f)
(check (fast-prime? 561 10000) => #t)

;; Exercise 1.29
(define (simpson-rule f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (simpson-term k)
    (* (cond ((= k 0) 1)
             ((= k n) 1)
             ((even? k) 2)
             (else 4))
       (y k)))
  (* (/ h 3) (sum simpson-term 0 inc n)))

(simpson-rule cube 0.0 1.0 100)
(simpson-rule cube 0.0 1.0 1000)
(integral cube 0 1 0.001)

;; Exercise 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (identity x) x)
(check  (sum-iter identity 1 inc 30) => (sum identity 1 inc 30))

;;Exercise 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter 1 1))

(define (factorial n)
  (product identity 1 inc n))
(define (factorial-iter n)
  (product-iter identity 1 inc n))
(check (factorial 5) => (factorial-iter 5))

(define (pi-aprox n)
  (define (pi-term n)
    (if (even? n)
        (/ (+ n 2) (inc n))
        (/ (inc n) (+ n 2))))
  (* 4.0 (product-iter pi-term 1 inc n)))
(pi-aprox 600000)

;; Exercise 1.32
(define (accumulate combiner null-value term a next b)
  (define (helper a)
    (if (> a b)
        null-value
        (combiner (term a)
                  (helper (next a)))))
  (helper a))

(define (accumulate-iter combiner null-value term a next b)
  (define (helper a result)
    (if (> a b)
        result
        (helper (next a) (combiner (term a) result))))
  (helper a null-value))

(check (accumulate * 1 identity 1 inc 5) => (factorial 5))
(check (accumulate-iter * 1 identity 1 inc 5) => (factorial 5))

;; Exercise 1.33
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (helper a)
    (define filtered-term (if (filter a) (term a) null-value))
    (if (> a b)
        null-value
        (combiner filtered-term (helper (next a)))))
  (helper a))

(define (filtered-accumulate-iter filter combiner null-value term a next b)
  (define (helper a result)
    (define filtered-term (if (filter a) (term a) null-value))
    (if (> a b)
        result
        (helper (next a) (combiner filtered-term result))))
  (helper a null-value))

(filtered-accumulate (lambda (x) #t) * 1 identity 1 inc 10)
(filtered-accumulate-iter (lambda (x) #t) * 1 identity 1 inc 10)

(filtered-accumulate odd? * 1 identity 1 inc 10)
(filtered-accumulate-iter odd? * 1 identity 1 inc 10)

;; Exercise 1.34
(define (f g) (g 2))
(f f)

;; Exercise 1.35
(define (golden-ratio)
  (fixed-point (lambda (x) (inc (/ 1 x))) 1.0))

(define debug #t)

(golden-ratio)

;; Exercise 1.36
(define (x-pow-x x)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) x))

(define (x-pow-x-avg x)
  (fixed-point (lambda (x) (average x
                                    (/ (log 1000) (log x))))
               x))

(x-pow-x 42)
(x-pow-x-avg 42)

;; Exercise 1.37
(define (cont-frac n d k)
  (define (helper x)
    (if (= x k)
        (/ (n x) (d x))
        (/ (n x) (+ (d x) (helper (inc x))))))
  (helper 1))

(define (cont-frac-iter n d k)
  (define (helper x result)
    (if (zero? x)
        result
        (helper (dec x) (/ (n x) (+ (d x) result)))))
  (helper k 0.0))

(cont-frac-iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           12)

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           12)

;; Exercise 1.38
(define (euler-approx x)
  (define (n i) 1)
  (define (d i)
    (if (= (remainder i 3) 2)
        (* 2 (inc (floor (/ i 3))))
        1))
  (exact->inexact (+ 2 (cont-frac n d x))))

(euler-approx 12)

;; Exercise 1.39
(define (tan-cf x k)
  (define (n i) (if (= i 1) x (- (square x))))
  (define (d i) (- (* 2 i) 1))
  (exact->inexact (cont-frac n d k)))

(tan-cf 0.69 5)

;; Exercise 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a (square x))
       (* b x)
       c)))

(newtons-method (cubic 3 2 1) 1)

;; Exercise 1.41
(define (double f)
  (lambda (x)
    (f (f x))))

(check (((double (double double)) inc) 5) => 21)

;; Exercise 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

(check ((compose square inc) 6) => 49)

;; Exercise 1.43
(define (repeated f n)
  (define (recur y result)
    (if (zero? y)
        result
        (recur (dec y) (f result))))
  (lambda (x)
    (recur (dec n) (f x))))

((repeated square 2) 5)

(define (repeated-compose f n)
  (if (zero? n)
      (lambda (x) x)
      (compose f (repeated-compose f (dec n)))))

((repeated-compose square 2) 5)

;; Exercise 1.44
(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
    (/ (+ (f x)
          (f (- x dx))
          (f (+ x dx)))
       3)))

(define (n-fold f n)
  (repeated (smooth f) n))

;; Exercise 1.46
(define (iterative-improve good-enough? improve)
  (define (recur x)
    (if (good-enough? x)
        x
        (recur (improve x))))
  (lambda (x)
    (recur x)))

(define (iter-impr-sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(iter-impr-sqrt 5)

