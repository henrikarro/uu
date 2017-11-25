#lang racket

(module graph racket

  (provide new has_vertex? has_edge? weight)
  
  (include "contracts.rkt") ; <- This is the file that you should submit.

  (require (prefix-in g: graph))

  ; Returns a new, empty directed graph.
  (define (new)
    (g:weighted-graph/directed '()))

  ; Adds a vertex with name ’a’ to ’digraph’.
  (define (add_vertex digraph a)
    (g:add-vertex! digraph a)
    digraph)

  ; Adds an edge from ’a’ to ’b’ with weight ’w’.
  (define (add_edge digraph a b w)
    (g:add-directed-edge! digraph a b w)
    digraph)

  ; Checks whether ’digraph’ has a vertex named ’a’.
  (define (has_vertex? digraph a)
    (g:has-vertex? digraph a))

  ; Checks whether vertex ’a’ has an edge to ’b’.
  (define (has_edge? digraph a b)
    (g:has-edge? digraph a b))

  ; Returns all vertices that ’a’ has an edge to.
  (define (out_neighbours digraph a)
    (g:get-neighbors digraph a))

  ; Returns the weight of the edge from ’a’ to ’b’.
  (define (weight digraph a b)
    (g:edge-weight digraph a b))
  
  )

(require 'graph)
