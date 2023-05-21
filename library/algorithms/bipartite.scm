; pre: G is not cyclic!
(define (bipartite? G)
  (define (bipartite?iter G S1 S2 shift)
    ;; ^^ NOTE: bool == #t means S1; #f means S2
    ;; Shift tells us how many vertices have been skipped over already [aka shift->l1 for (l1->v l1 G)]
    (cond ((null? (getV G)) (disjoint? S1 S2))
          ((null? S1) (bipartite?iter (V-- G) (cons (car (getV G)) S1) (union (getAllEdgeAdj (car (getV G)) G) S2) 0))
          ;; Must have a check that (l1->v shift G) doesn't yield error!
          ((>= shift (first (undir->labeled G))) (bipartite?iter G S1 S2 0))
          ((inSet? (l1->v shift G) S2) (bipartite?iter (remVert (l1->v shift G) G) (union (getAllEdgeAdj (l1->v shift G) G) S1) S2 0))
          ;; ^^ if the vertex we are looking at is in S2, remove vertex from G and add all edges to S1.
          ((inSet? (l1->v shift G) S1) (bipartite?iter (remVert (l1->v shift G) G) S1 (union (getAllEdgeAdj (l1->v shift G) G) S2) 0))
          (else (bipartite?iter G S1 S2 (+ 1 shift)))))
  
  (if (acyclic? G) (bipartite?iter G '() '() 0) #f))