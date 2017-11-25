#lang racket

(require rackunit
         (submod "graph.rkt" graph))

(test-case
 "add_vertex"
 (check-false (has_vertex? (new) 1))
 (check-true (has_vertex? (add_vertex (new) 1) 1))
 (check-false (has_vertex? (add_vertex (new) 1) 2)))

(test-case
 "add_vertex non-integer name"
 (check-exn
  exn:fail:contract?
  (lambda ()
    (add_vertex (new) 'foo))))

(test-case
 "add_vertex same name"
 (check-exn
  exn:fail:contract?
  (lambda ()
    (add_vertex (add_vertex (new) 1) 1))))

(test-case
 "add_edge"
 (check-false (has_edge? (new) 1 2))
 (let ([g (add_edge (add_vertex (add_vertex (add_vertex (new) 1) 2) 3) 1 2 42)])
   (check-true (has_edge? g 1 2))
   (check-false (has_edge? g 2 1))
   (check-false (has_edge? g 1 3))
   (check-false (has_edge? g 3 1))
   (check-false (has_edge? g 2 3))
   (check-false (has_edge? g 3 2))))

(test-case
 "add_ege non-existing first vertex"
 (check-exn
  exn:fail:contract?
  (lambda ()
    (add_edge (add_vertex (new) 1) 2 1 42))))

(test-case
 "add_ege non-existing second vertex"
 (check-exn
  exn:fail:contract?
  (lambda ()
    (add_edge (add_vertex (new) 1) 1 2 42))))

(test-case
 "add_edge non-integer weight"
 (check-exn
  exn:fail:contract?
  (lambda ()
    (add_edge (add_vertex (add_vertex (new) 1) 2) 1 2 'foo))))

(test-case
 "add_edge existing edge"
 (check-exn
  exn:fail:contract?
  (lambda ()
    (add_edge (add_edge (add_vertex (add_vertex (new) 1) 2) 1 2 42) 1 2 4711))))

(test-case
 "out_neighbours"
  (let ([g (add_edge (add_edge (add_edge (add_vertex (add_vertex (add_vertex (new) 1) 2) 3) 1 2 42) 2 3 4711) 1 3 666)])
    (check-equal? (sort (out_neighbours g 1) <) '(2 3))
    (check-equal? (out_neighbours g 2) '(3))
    (check-equal? (out_neighbours g 3) '())))

(test-case
 "out-neighbours non-existing vertex"
 (check-exn
  exn:fail:contract?
  (lambda ()
    (out_neighbours (new) 1))))
