#lang racket

(require rackunit
         "lab2.rkt")

(check-equal? (permutation '()) '(()))
(check-equal? (permutation '(1)) '((1)))
(check-equal? (permutation '(1 2)) '((1 2) (2 1)))
(check-equal? (permutation '(1 2 3)) '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)))

(check-equal? (qsort '()) '())
(check-equal? (qsort '(2 1 3)) '(1 2 3))
(check-equal? (qsort '(6 3 2 5 6 3 3 5 7 3 1)) '(1 2 3 3 3 3 5 5 6 6 7))

(check-equal? (let ([f (lambda  (x) x)]) (f 'a)) 'a)
(check-equal? (let ([f (lambda  x x)]) (f 'a)) '(a))
(check-equal? (let ([f (lambda  (x . y) x)]) (f 'a)) 'a)
(check-equal? (let ([f (lambda (x . y) y)]) (f 'a)) '())

(check-equal? (fib 0) 1)
(check-equal? (fib 1) 1)
(check-equal? (fib 2) 2)
(check-equal? (fib 3) 3)
(check-equal? (fib 4) 5)
(check-equal? (fib 5) 8)
(check-equal? (fib 6) 13)
(check-equal? (fib 7) 21)
(check-equal? (fib 8) 34)
(check-equal? (fib 9) 55)
(check-equal? (fib 10) 89)

(check-equal? (fib/m 0) 1)
(check-equal? (fib/m 1) 1)
(check-equal? (fib/m 2) 2)
(check-equal? (fib/m 3) 3)
(check-equal? (fib/m 4) 5)
(check-equal? (fib/m 5) 8)
(check-equal? (fib/m 6) 13)
(check-equal? (fib/m 7) 21)
(check-equal? (fib/m 8) 34)
(check-equal? (fib/m 9) 55)
(check-equal? (fib/m 10) 89)

(let* ([t1 (current-milliseconds)]
       [t2 (begin
             (fib 30)
             (current-milliseconds))]
       [t3 (begin
             (fib/m 30)
             (current-milliseconds))])
  (check-true (> (- t2 t1) (* (- t3 t2) 100))))

(check-equal? (simp '(+ (+ y 0) (* y 1))) '(* y 2))
(check-equal? (simp '(+ (* y 0) x)) 'x)
