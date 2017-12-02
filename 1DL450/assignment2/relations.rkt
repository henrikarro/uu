#lang racket

(require racket/set)

(provide #%module-begin
         #%top-interaction
         #%app
         #%datum
         (rename-out [get-relation-definition #%top])
         
         > < <= >=
         (rename-out [equal? =]
                     [not-equal? !=])
         or and not
         
         make-relation
         project
         restrict
         relation
         (rename-out [relation-equal? equal?])
         show)

(define global-relations (make-hash))

;;; (make-relation colnames data)
;;; Creates a list of rows, where each row contains columns
;;; represented as (colname colvalue).
;;;
;;; Example: (make-relation (foo bar) ((1 2) (3 4))) gives
;;; '(((foo 1) (bar 2)) ((foo 3) (bar 4)))
(define-syntax-rule (make-relation colnames data)
  (map (lambda (row) (map list 'colnames row)) 'data))

(define (column-name col)
  (car col))

(define (column-value col)
  (car (cdr col)))

;;; (relation relname reldef)
;;; Saves the relation defined by reldef under the name relname.
;;;
;;; Example: (relation frotz (make-relation (foo bar) ((1 2) (3 4))))
(define-syntax-rule (relation relname reldef)
  (hash-set! global-relations 'relname reldef))

;;; (project rel colnames)
;;; Keeps only the columns with names in colnames from the
;;; relation rel, which can be either a name (created with
;;; the relation function) or a relation definition (created
;;; with the make-relation function).
;;;
;;; Example: (project frotz (foo)) gives '(((foo 1)) ((foo 3)))
(define-syntax-rule (project rel colnames)
  (remove-duplicates (filter-columns (lambda (col) (member (column-name col) 'colnames)) rel)))

(define (filter-columns pred rel)
  (map-rows (lambda (col) (filter pred col)) rel))

(define (map-rows f rel)
  (map (lambda (row) (f row)) rel))

;;; (restrict rel condition)
;;; Keeps only the rows in rel which satisfy condition.
;;; The condition can compare column names using =, !=, <, <=, > or >=,
;;; and conditions can be combined using and, or and not.
;;;
;;; Example: (restrict frotz (and (= foo 1) (< bar 4))) gives
;;; '(((foo 1) (bar 2)))
(define-syntax-rule (restrict rel condition)
  (filter (lambda (row) (eval-condition-on-row 'condition row)) rel))

(define (eval-condition-on-row condition row)
  (let ([ns (make-base-namespace)])
    (eval '(define = equal?) ns)
    (eval '(define (!= x y) (not (equal? x y))) ns)
    (map (lambda (col) (namespace-set-variable-value! (column-name col) (column-value col) #f ns)) row)
    (eval condition ns)))

;;; (equal? rel1 rel2) (Note that the exported name is equal?)
;;; Gives #t iff rel1 and rel2 represent the same relations, i.e.,
;;; they have the same column names and the same rows (in any order).
;;;
;;; Example (equal? frotz (make-relation (foo bar) ((3 4) (1 2))))
;;; gives #t, while (equal? frotz (make-relation (bar foo) ((2 1) (4 3))))
;;; gives #f
(define (relation-equal? rel1 rel2)
  (equal? (list->set rel1) (list->set rel2)))

;;; (show rel)
;;; Gives the internal representation of a relation.
;;;
;;; Example: (show frotz) gives '(((foo 1) (bar 2)) ((foo 3) (bar 4)))
(define-syntax-rule (show rel)
  rel)

;;; (get-relation-definition . rel)
;;; Gives the definition of the relation rel, which may either
;;; be a name saved by the relation function, or a relation
;;; definition, in which case it is returned unchanged.
;;;
;;; (get-relation-definition . frotz) gives
;;; '(((foo 1) (bar 2)) ((foo 3) (bar 4)))
(define-syntax-rule (get-relation-definition . rel)
  (if (hash-has-key? global-relations 'rel)
      (hash-ref global-relations 'rel)
      'rel))

;;; (!= x y) (Note that the exported name is !=)
;;; The negation of equal?
(define (not-equal? x y)
  (not (equal? x y)))
