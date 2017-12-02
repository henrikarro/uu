#lang racket

(require racket/class)
(require (prefix-in g: graph))

(provide dice)

(define dice-state '())

(define (dice winning-node nodes dice)
  (set! dice-state (new dice-state% [dice dice]))
  (define graph (build-graph nodes))
  (my-dice graph winning-node '(1) 0))

(define (my-dice graph winning-node start-nodes move-number)
  (if (any (lambda (node) (eq? node winning-node)) start-nodes)
      move-number
      (if (has-seen-position-before (get-current-die-number) start-nodes)
          -1
          (begin
            (new-position-seen (get-current-die-number) start-nodes)
            (let ([die (next-die)])
              (if (= die -1)
                  -1
                  (let ([end-nodes (traverse-to-depth graph start-nodes die)])
                    (my-dice graph winning-node end-nodes (add1 move-number)))))))))

(define (build-graph nodes)
  (g:directed-graph nodes))

(define (any pred l)
  (if (null? l)
      #f
      (or (pred (car l))
          (any pred (cdr l)))))

(define (traverse-to-depth graph start-nodes depth)
  (cond
    [(null? start-nodes) '()]
    [(zero? depth) start-nodes]
    [else
     (let ([n1 (traverse-to-depth graph (g:get-neighbors graph (car start-nodes)) (sub1 depth))]
           [n2 (traverse-to-depth graph (cdr start-nodes) depth)])
       (remove-duplicates (append n1 n2)))]))

(define (has-seen-position-before die-number nodes)
  (send dice-state has-seen-position-before die-number nodes))

(define (new-position-seen die-number nodes)
  (send dice-state new-position-seen die-number nodes))

(define (get-current-die-number)
  (send dice-state get-current-die-number))

(define (next-die)
  (send dice-state next-die))

(define dice-state%
  (class object%
    (init dice)

    (define current-die-number 0)
    (define all-dice dice)
    (define positions-seen '())
    
    (super-new)

    (define/public (get-current-die-number)
      current-die-number)

    (define/private (current-die)
      (if (empty? all-dice)
          -1
          (list-ref all-dice current-die-number)))
    
    (define/public (next-die)
      (let ([die (current-die)])
        (if (< current-die-number (sub1 (length all-dice)))
            (set! current-die-number (add1 current-die-number))
            (set! current-die-number 0))
        die))

    (define/public (has-seen-position-before die-number nodes)
      (member (list die-number nodes) positions-seen))

    (define/public (new-position-seen die-number nodes)
      (set! positions-seen (cons (list die-number nodes) positions-seen)))
  )
)

(module+ test
  (require rackunit)
  (require (prefix-in g: graph))

  (check-equal? 0 (dice 1 '((1 2)) '()))
  (check-equal? -1 (dice 2 '((1 2)) '()))
  (check-equal? 1 (dice 2 '((1 2)) '(1)))
  (check-equal? -1 (dice 2 '((1 2)) '(2)))
  (check-equal? -1 (dice 2 '((1 2) (2 1)) '(2)))
  (check-equal? 1 (dice 2 '((1 2) (2 1)) '(3)))

  ; Test cases from assignment description
  (check-equal? 2 (dice 3 '((1 2) (2 1) (2 3) (3 2)) '(3 5)))
  (check-equal? 3 (dice 4 '((1 2) (2 3) (3 4)) '(1)))
  (check-equal? -1 (dice 3 '((1 2) (2 3)) '(4 2 6)))

  ; build-graph tests
  (let ([graph (build-graph '())])
    (check-equal? '() (g:get-vertices graph))
    (check-equal? '() (g:get-edges graph)))
  (let ([graph (build-graph '((1 2)))])
    (check-equal? '(1 2) (sort (g:get-vertices graph) <))
    (check-equal? '(2) (g:get-neighbors graph 1))
    (check-equal? '() (g:get-neighbors graph 2)))
  (let ([graph (build-graph '((1 2) (2 1)))])
    (check-equal? '(1 2) (sort (g:get-vertices graph) <))
    (check-equal? '(2) (g:get-neighbors graph 1))
    (check-equal? '(1) (g:get-neighbors graph 2)))
  (let ([graph (build-graph '((1 2) (2 1) (2 3) (3 2)))])
    (check-equal? '(1 2 3) (sort (g:get-vertices graph) <))
    (check-equal? '(2) (g:get-neighbors graph 1))
    (check-equal? '(1 3) (sort (g:get-neighbors graph 2) <))
    (check-equal? '(2) (g:get-neighbors graph 3)))

  ; traverse-to-depth tests
  (let ([graph (build-graph '((1 2) (2 1) (2 3) (3 2)))])
    ; Search starting from 1
    (check-equal? '(1) (traverse-to-depth graph '(1) 0))
    (check-equal? '(2) (traverse-to-depth graph '(1) 1))
    (check-equal? '(1 3) (traverse-to-depth graph '(1) 2))
    (check-equal? '(2) (traverse-to-depth graph '(1) 3))
    (check-equal? '(1 3) (traverse-to-depth graph '(1) 4))
    (check-equal? '(2) (traverse-to-depth graph '(1) 5))
    ; Search starting from 2
    (check-equal? '(2) (traverse-to-depth graph '(2) 0))
    (check-equal? '(1 3) (traverse-to-depth graph '(2) 1))
    (check-equal? '(2) (traverse-to-depth graph '(2) 2))
    (check-equal? '(1 3) (traverse-to-depth graph '(2) 3))
    (check-equal? '(2) (traverse-to-depth graph '(2) 4))
    (check-equal? '(1 3) (traverse-to-depth graph '(2) 5))
    ; Search starting from 3
    (check-equal? '(3) (traverse-to-depth graph '(3) 0))
    (check-equal? '(2) (traverse-to-depth graph '(3) 1))
    (check-equal? '(1 3) (traverse-to-depth graph '(3) 2))
    (check-equal? '(2) (traverse-to-depth graph '(3) 3))
    (check-equal? '(1 3) (traverse-to-depth graph '(3) 4))
    (check-equal? '(2) (traverse-to-depth graph '(3) 5)))
  (let ([graph (build-graph '((1 2) (2 3) (3 4)))])
    ; Search starting from 1
    (check-equal? '(1) (traverse-to-depth graph '(1) 0))
    (check-equal? '(2) (traverse-to-depth graph '(1) 1))
    (check-equal? '(3) (traverse-to-depth graph '(1) 2))
    (check-equal? '(4) (traverse-to-depth graph '(1) 3))
    (check-equal? '() (traverse-to-depth graph '(1) 4))
    ; Search starting from 2
    (check-equal? '(2) (traverse-to-depth graph '(2) 0))
    (check-equal? '(3) (traverse-to-depth graph '(2) 1))
    (check-equal? '(4) (traverse-to-depth graph '(2) 2))
    (check-equal? '() (traverse-to-depth graph '(2) 3))
    ; Search starting from 3
    (check-equal? '(3) (traverse-to-depth graph '(3) 0))
    (check-equal? '(4) (traverse-to-depth graph '(3) 1))
    (check-equal? '() (traverse-to-depth graph '(3) 2))
    ; Search starting from 4
    (check-equal? '(4) (traverse-to-depth graph '(4) 0))
    (check-equal? '() (traverse-to-depth graph '(4) 1)))
)  
