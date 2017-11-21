#lang racket

(provide permutation
         qsort
         fib
         fib/m
         simp)

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
  (cond [(or (= n 0) (= n 1)) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))

(define-syntax-rule (define-memo (f args ...) body ...)
  (define f
    (let ([memo (make-hash)])
      (lambda (args ...)
        (cond [(hash-has-key? memo (list args ...))
               (hash-ref memo (list args ...))]
              [else
               (let ([res (begin body ...)])
                 (hash-set! memo (list args ...) res)
                 res)])))))

(define-memo (fib/m x)
  (displayln x) ; side effect to see when we actually execute the body
  (case x
    [(0 1) 1]
    [else (+ (fib/m (- x 1)) (fib/m (- x 2)))]))

(define-syntax simplifier
  (syntax-rules (->)
    [(simplifier (x -> y) ...)
     (recur (lambda (form)
              (match form
                [`x `y]
                ...
                [else form])))]))

(define ((recur simplify) form)
  (let loop ([reduced-form
              (cond [(list? form)
                     (map (recur simplify) form)]
                    [else form])])
    (let ([final-form (simplify reduced-form)])
      (if (equal? final-form reduced-form)
          final-form
          (loop final-form)))))

(define simp (simplifier
              [(+ ,x 0) -> ,x]
              [(+ 0 ,x) -> ,x]
              [(* ,x 0) -> 0]
              [(* 0 ,x) -> 0]
              [(* ,x 1) -> ,x]
              [(* 1 ,x) -> ,x]
              [(+ ,x ,x) -> (* ,x 2)]))
