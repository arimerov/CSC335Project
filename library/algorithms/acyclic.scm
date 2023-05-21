; pre: G is a connected graph.
(define (acyclic? G)
  (cond ((null? (getV G)) #t)
        ((null? (getE G)) #t)
        ((not (vEdge? (car (getV G)) G)) (and #t (acyclic? (V-- G))))
        ((oneEdge? (car (getV G)) G) (and #t (acyclic? (V-- (remEdge (getEdge (car (getV G)) G) G)))))
        (else (and (not (path? (getEdgeAdj (car (getV G)) G) (car (getV G)) (remEdge (getEdge (car (getV G)) G) G)))
                   (acyclic? (remEdge (getEdge (car (getV G)) G) G))))))


; (acyclify G) := Takes in G and removes edges s.t. G is acyclic.
; pre: G is an undirected graph
(define (acyclify G)
  (cond ((null? (getV G)) G)
        ((null? (getE G)) G)
        ((not (vEdge? (car (getV G)) G)) (V++ (car (getV G)) (acyclify (V-- G))))
        ((oneEdge? (car (getV G)) G) (V++ (car (getV G)) (E++ (getEdge (car (getV G)) G) (acyclify (V-- (remEdge (getEdge (car (getV G)) G) G))))))
        ((path? (getEdgeAdj (car (getV G)) G) (car (getV G)) (remEdge (getEdge (car (getV G)) G) G)) ;Path between current node and edgeAdj
         (remEdge (getEdge (car (getV G)) G) (acyclify (remEdge (getEdge (car (getV G)) G) G))))
        (else (acyclify (remEdge (getEdge (car (getV G)) G) G)))))
