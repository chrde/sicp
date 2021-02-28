(use-modules 
  (srfi srfi-78) ;tests
  )

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance 
                       (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance 
                 (- balance amount))
               balance)
        "Insufficient funds")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                 (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: 
                 MAKE-ACCOUNT" m))))
  dispatch)

;; Exercise 3.1
(define (make-accumulator initial)
  (lambda (amount)
    (set! initial
      (+ initial amount))
    initial))

(define A (make-accumulator 5))
(A 10)
(A 10)

;; Exercise 3.2
(define (make-monitored fn)
  (let ((count 0))
    (define (dispatch m)
      (if (eq? m 'how-many-calls)
          count
          (begin
            (set! count (+ 1 count))
            (fn m))))
    dispatch))

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls)
