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

(check-equal? (fib 0) 0)
(check-equal? (fib 1) 1)
(check-equal? (fib 2) 1)
(check-equal? (fib 3) 2)
(check-equal? (fib 4) 3)
(check-equal? (fib 5) 5)
(check-equal? (fib 6) 8)
(check-equal? (fib 7) 13)
(check-equal? (fib 8) 21)
(check-equal? (fib 9) 34)
(check-equal? (fib 10) 55)
