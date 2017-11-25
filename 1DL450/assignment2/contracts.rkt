(provide
 (contract-out
  [add_vertex (->i ([g any/c]
                    [a (g) (and/c
                            integer?
                            (lambda (a) (not (has_vertex? g a))))])
                   [res any/c])]
  [add_edge (->i ([g any/c]
                  [a (g) (lambda (a) (has_vertex? g a))]
                  [b (g a) (and/c
                            (lambda (b) (has_vertex? g b))
                            (lambda (b) (not (has_edge? g a b))))]
                  [w integer?])
                 [res (a b) (lambda (res) (has_edge? res a b))])]
  [out_neighbours (->i ([g any/c]
                        [a (g) (lambda (a) (has_vertex? g a))])
                       [res (g a) (and/c
                                   (listof integer?)
                                   (lambda (res) (andmap (lambda (b) (has_edge? g a b)) res)))])]))
