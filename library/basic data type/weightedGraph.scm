; (getW wG) := Returns W of wG
(define (getW wG)
  (car (cdr (cdr wG))))

; (W-- wG) := Returns wG with weight in index 0 removed
(define (W-- wG)
  (triple (getV wG) (getE wG) (cdr (getW wG))))

; (W++ w wG) := Returns wG with weight w appended to index 0 of wG's W.
(define (W++ w wG)
  (triple (getV wG) (getE wG) (car w (getW wG))))

; (EW-- wG) := Returns wG with weighted edge in index 0 removed. [both the edge and its associated weight  
(define (EW-- wG)
  (triple (getV wG) (cdr (getE wG)) (cdr (getW wG))))

; (triple V E W) := Returns wG with set V, set E, and associated set W to set E.
(define (triple V E W)
  (cons V (cons E (cons W '()))))


; (wV++ v wG) := returns wG with vertice v added. Must incorporate new type to not remove W from weighted graph
(define (wV++ v wG)
  (triple (cons v (getV wG)) (getE wG) (getW wG)))

; (wV-- wG) := returns wG with vertice v removed.
(define (wV-- wG)
  (triple (cdr (getV wG)) (getE wG) (getW wG)))

; (EW++ e w wG) := adds e with weight w to wG
(define (EW++ e w wG)
  (triple (getV wG) (cons e (getE wG)) (cons w (getW wG))))

; (weMatch? wG) := Checks that the number of edges == number of weights. Only true for correctly formatted wG's.
(define (weMatch? wG)
  ; number of elements in E == number of elements in W.
  (cond ((null? (getE wG)) (null? (getW wG)))
        ((null? (getW wG)) (null? (getE wG)))
        (else (weMatch? (E-- (W-- wG))))))

; (getAssoDirWeight v G) := For vertex v, get the weight of the first dirEdge directing away from v.
(define (getAssoDirWeight v G)
  (getAssoDirEdgeWeight (getDirEdge v G) G))

(define (getAssoDirEdgeWeight e G)
  (getByIndex (e->l2 e G) (getW G)))

; (remAssoDirWeight v G) := Removes both the first dirEdge and its associated weight of vertex v in graph G.
; pre: (dirEdge? v G) -> #t.
(define (remAssoDirWeight v G)
  (remEdge (getDirEdge v G) (triple (getV G) (getE G) (remByIndex (e->l2 (getDirEdge v G) G) (getW G)))))


; returns the dirEdge with the lowest associated weight away from v.
(define (getLowestAssoDirWeight v G)
  (cond ((noDirEdge? v G) 0)
        ((oneDirEdge? v G) (getAssoDirWeight v G))
        (else (min (getAssoDirWeight v G) (getLowestAssoDirWeight v (remWGEdge (getDirEdge v G) G))))))

;; (remWGVert v G) := Removes v from wG. Make sure function doesn't remove third set W.
(define (remWGVert v wG)
  (cond ((null? (getV wG)) wG)
        ((equal? v (car (getV wG))) (wV-- wG))
        (else (wV++ (car (getV wG)) (remWGVert v (wV-- wG))))))
;; (remWGEdge e G) := Removes e from wG, along with its associated edge.
(define (remWGEdge e wG)
  (cond ((null? (getE wG)) wG)
        ((equal? e (car (getE wG))) (EW-- wG))
        (else (EW++ (car (getE wG)) (car (getW wG)) (remWGEdge e (EW-- wG))))))














