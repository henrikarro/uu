#lang racket

(require rackunit)
(require "relations.rkt")

(relation frotz (make-relation
                 (foo bar baz)
                 ((1 2 3) (4 5 6))))

(check-equal? '(((foo 1) (bar 2) (baz 3))) (restrict frotz (< foo 3)))
(check-equal? '(((foo 1) (bar 2) (baz 3)) ((foo 4) (bar 5) (baz 6)))
              (restrict frotz (or (< bar 5) (>= baz 6))))
(check-equal? '() (restrict frotz (and (< bar 5) (>= baz 6))))
(check-equal? '(((foo 1) (bar 2) (baz 3))) (restrict frotz (foo . != . 4)))

(check-equal? '(((foo 1) (baz 3)) ((foo 4) (baz 6)))
              (project frotz (foo baz)))

(check-true (equal? (project frotz (baz foo))
                     (make-relation (foo baz) ((4 6) (1 3)))))
(check-false (equal? (project frotz (baz foo))
                      (make-relation (foo baz) ((4 6) (1 2)))))
(check-false (equal? (project frotz (foo baz))
                     (make-relation (baz foo) ((6 4) (3 1)))))
                            