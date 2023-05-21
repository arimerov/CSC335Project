(load "set.scm")

(define (getV G)
  (car G))
(define (getE G)
  (car (cdr G)))

(define (VSet? V)
  (and (set? V) (lat? V)))
(define (ESet? E)
  (and (set? E) (latPair? E)))

(define (EVDomain? E V)
  ;; Assume E and V are proper sets.
  (cond ((null? E) #t)
        ((and (inSet? (first (car E)) V) (inSet? (second (car E)) V)) (and #t (EVDomain? (cdr E) V)))
        (else #f)))

(define (undirGraph? G)
  (and (VSet? (getV G)) (ESet? (getE G)) (undirSet? (getE G)) (EVDomain? (getE G) (getV G))))
(define (undirGraphify G)
  (cond ((not (vSet? (getV G))) (undirGraphify (pair (setify (getV G)) (getE G))))
        ((not (eSet? (getE G))) (undirGraphify (pair (getV G) (undirSetify (getE G)))))
        ; Now that V and E are guaranteed to be sets, remove edges with reference to a v not in V,
        ; And remove v's not specified in an Edge.
        (else (remDisconnect G))))
(define (remDisconnect G)
  (remVertDis (remEdgeDis (remVertDis G))))
(define (remVertDis G)
  (cond ((null? (getV G)) (pair '() (getE G)))
        ((vEdge? (car (getV G)) G) (V++ (car (getV G)) (remVertDis (V-- G))))
        (else (remVertDis (V-- G)))))
(define (remEdgeDis G)
  (cond ((null? (getE G)) (pair (getV G) '()))
        ((and (inSet? (first (car (getE G))) (getV G)) (inSet? (second (car (getE G))) (getV G)))
         (E++ (car (getE G)) (remEdgeDis (E-- G))))
        (else (remEdgeDis (E-- G)))))



(define (vEdge? v G)
  (cond ((null? (getE G)) #f)
        ((equal? v (first (car (getE G)))) #t)
        ((equal? v (second (car (getE G)))) #t)
        (else (vEdge? v (E-- G)))))
(define (oneEdge? v G)
  (cond ((not (inSet? v (getV G))) #f) ; Not in graph, so can't have an edge
        ((not (vEdge? v G)) #f) ; Doesn't have an edge, so #f for 0
        ((moreEdge? v G) #f)
        (else #t)))
(define (moreEdge? v G)
  (cond ((not (inSet? v (getV G))) #f) ; Not in graph, so can't have an edge
        ((not (vEdge? v G)) #f) ; Doesn't have an edge, so #f for 0
        ((vEdge? v (remEdge (getEdge v G) G)) #t)
        (else #f)))
;; (getEdge v G) := Returns first found edge pair containing v.
(define (getEdge v G)
  (cond ((null? (getE G)) '()) ; Will return empty if not found
        ((equal? v (first (car (getE G)))) (car (getE G)))
        ((equal? v (second (car (getE G)))) (car (getE G)))
        (else (getEdge v (E-- G)))))
(define (nextEdge v G)
  (getEdge v (remEdge (getEdge v G) G)))
(define (getEdgeAdj v G)
  (cond ((null? (getE G)) #f) ;No adjacent found
        ((equal? v (first (car (getE G)))) (second (car (getE G))))
        ((equal? v (second (car (getE G)))) (first (car (getE G))))
        (else (getEdgeAdj v (E-- G)))))

;; (remVert v G) := Removes v from G
(define (remVert v G)
  (cond ((null? (getV G)) G)
        ((equal? v (car (getV G))) (V-- G))
        (else (V++ (car (getV G)) (remVert v (V-- G))))))
;; (remEdge e G) := Removes e from G
(define (remEdge e G)
  (cond ((null? (getE G)) G)
        ((equal? e (car (getE G))) (E-- G))
        (else (E++ (car (getE G)) (remEdge e (E-- G))))))
; (remVerts V G) := Removes all vertices detailed in set V from graph G.
(define (remVerts V G)
  (cond ((null? V) G)
        (else (remVerts (cdr V) (remVert (car V) G)))))
; (remEdges E G) := Removes all Edges detailed in set E from graph G.
(define (remEdges E G)
  (cond ((null? E) G)
        (else (remEdges (cdr E) (remEdge (car E) G)))))

; (remEdgesFromV V G) := Input set of vertices V, and this removes all Edges directed away from each v (in) V.
(define (remEdgesFromV V G)
  (remEdges (getAssoEdges V G) G))

; (getAssoEdges V G) := Input st of vertices V, and this returns a set of all edges directing away from each v (in) V. 
(define (getAssoEdges V G)
  (cond ((null? V) '())
        ((inSet? (car V) (firsts (getE G))) (cons (getEdge (car V) G) (getAssoEdges V (remEdge (getEdge (car V) G) G))))
        (else (getAssoEdges (cdr V) G))))



(define (graphUnion G1 G2)
  (pair (setify (union (getV G1) (getV G2))) (undirSetify (union (getE G1) (getE G2)))))
(define (dirGraphUnion G1 G2)
  (pair (setify (union (getV G1) (getV G2))) (setify (union (getE G1) (getE G2)))))


(define (getAllEdgeAdj v G)
  (cond ((vEdge? v G) (cons (getEdgeAdj v G) (getAllEdgeAdj v (remEdge (getEdge v G) G))))
        (else '())))



(define (V-- G)
  (pair (cdr (getV G)) (getE G)))
(define (E-- G)
  (pair (getV G) (cdr (getE G))))
(define (VE-- G) ;Equivalent to (V-- (E-- G))
  (pair (cdr (getV G)) (cdr (getE G))))

(define (V++ v G)
  (pair (cons v (getV G)) (getE G)))
(define (E++ e G)
  (pair (getV G) (cons e (getE G))))

;; (vShare? G1 G2) := #t if G1 and G2 share a vertex.
(define (vShare? G1 G2)
  (cond ((null? (getV G1)) #f)
        ((inSet? (car (getV G1)) (getV G2)) #t)
        (else (or #f (vShare? (V-- G1) G2)))))
;; (getVShare G1 G2) := Returns first v in G1 and G2 that is shared.
(define (getVShare G1 G2)
  (cond ((null? (getV G1)) #f)
        ((inSet? (car (getV G1)) (getV G2)) (car (getV G1)))
        (else (getVShare (V-- G1) G2))))












