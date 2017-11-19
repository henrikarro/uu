#lang racket

(provide permutation
         qsort
         fib)

(define (pyth)
  (for* ([a (in-range 1 20)]
         [b (in-range a 20)]
         [c (in-range b 20)])
    (when (= (+ (expt a 2) (expt b 2)) (expt c 2))
      (printf "~a^2 + ~a^2 = ~a^2~n" a b c))))

(define (permutation l)
  (if (null? l)
      '(())
      (for*/list ([x l]
                  [t (permutation (remove x l))])
        (cons x t))))

(define (qsort l)
  (if (null? l)
      '()
      (let* ([p (car l)]
             [xs (cdr l)]
             [smaller (filter (lambda (x) (<= x p)) xs)]
             [greater (filter (lambda (x) (> x p)) xs)])
        (append (qsort smaller) (list p) (qsort greater)))))

(define (fib n)
  (cond [(or (= n 0) (= n 1)) n]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))
