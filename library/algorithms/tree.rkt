;; pre: G is undirected graph
(define (tree? G)
  (and (acyclic? G) (connected? G)))
; G is an undirected graph that is both acyclic and connected <=> G must be a tree!

(define (treeify G v)
  (cond (((not (acyclic? G)) #f))
        ((not (vEdge? (car (getV G)) G)) (cons v '())) ; [remVert (all) v (not in) E]
        ((oneEdge? (car (getV G)) G) (cons v (treeify (remVert v (remEdge (getEdge v G) G))
                                                    (getEdgeAdj v G))))
        (else (altUnion (treeify (remVert v (remEdge (getEdge v G) G)) (getEdgeAdj v G))
                        (treeify (remEdge (getEdge v G) G) v)))))

