(define (depFirst vStart vTar G)
  (let ((V (getV G)) (E (getE G)))
    (cond ((null? V) '(())) ;If V is null, then all vertices have been searched
          ((null? E) '(())) ;If E is null, then all edges have been searched
          ((not (and (inSet? vStart V) (inSet? vTar V))) '(())) ;If either inputted vertex doesn't exist in V, then there's no solution.
          ((vEdge? vStart G) (let ((G1 (remEdge (getEdge vStart G) G)))
                                   (cond ((equal? vTar (getEdgeAdj vStart G)) (pair vStart vTar))
                                         ((inSet? (getEdgeAdj vStart G) V) ; If this is false, then the graph is cyclic!
                                          (let ((D1 (depFirst (getEdgeAdj vStart G) vTar (remVert vStart G1)))) ; D1 prioritizes traversing the edge first
                                            (if (lat? D1) (cons vStart D1) (depFirst vStart vTar G1)))) ; If traversing the edge does not yield a correct answer, then it calls itself with the same vertex and the edge removed.
                                         (else (depFirst vStart vTar G1))))) ; Else ensure the code doesn't traverse a loop during search.
          (else '(())))))

; Breadth-First should be similar, but instead calls (depFirst vStart vTar G1) since looking at all connections if first priority.

(define (breFirst vStart vTar G)
  (let ((V (getV G)) (E (getE G)))
    (cond ((null? V) '(())) ;If V is null, then all vertices have been searched
          ((null? E) '(())) ;If E is null, then all edges have been searched
          ((not (and (inSet? vStart V) (inSet? vTar V))) '(())) ;If either inputted vertex doesn't exist in V, then there's no solution.
          ((vEdge? vStart G) (let ((G1 (remEdge (getEdge vStart G) G)))
                                   (cond ((equal? vTar (getEdgeAdj vStart G)) (pair vStart vTar))
                                         ((inSet? (getEdgeAdj vStart G) V)
                                          (let ((B1 (breFirst vStart vTar G1)))
                                            (if (lat? B1) B1 (cons vStart (breFirst (getEdgeAdj vStart G) vTar (remVert vStart G1))))))
                                         (else (cons vStart (breFirst (getEdgeAdj vStart G) vTar (remVert vStart G1)))))))
          (else '(())))))

(define (path? vStart vTar G)
  (lat? (breFirst vStart vTar G)))

