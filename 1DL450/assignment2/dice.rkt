#lang racket

(require racket/class)
(require (prefix-in g: graph))

(define dice-state '())

(define (dice winning-node nodes dice)
  (set! dice-state (new dice-state% [dice dice]))
  (define graph (g:directed-graph nodes))
  (my-dice graph winning-node '(1) 1))

(define (my-dice graph winning-node start-nodes move-number)
  (if (has-seen-position-before (get-current-die-number) start-nodes)
      -1
      (let* ([die (next-die)]
             [end-nodes (traverse-to-depth graph start-nodes die)])
        (new-position-seen (get-current-die-number) start-nodes)
        (if (any (lambda (node) (eq? node winning-node)) end-nodes)
            move-number
            (my-dice graph winning-node end-nodes (add1 move-number))))))
           
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
      (list-ref all-dice current-die-number))
    
    (define/public (next-die)
      (let ([die (current-die)])
        (if (< current-die-number (sub1 (length all-dice)))
            (set! current-die-number (add1 current-die-number))
            (set! current-die-number 0))
        die))

    (define/public (has-seen-position-before die-number nodes)
      (member (list die-number nodes) positions-seen))

    (define/public (new-position-seen die-number nodes)
      (set! positions-seen (cons (list die-number nodes) positions-seen)))))
