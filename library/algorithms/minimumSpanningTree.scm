;; Incorporate Kruskal's Algorithm for MST
;;; 1) Sort all the edges in non-decreasing order of their weight. 
;;; 2) Pick the smallest edge. Check if it forms a cycle with the spanning tree formed so far.
;;;;;   ^ If the cycle is not formed, include this edge. Else, discard it. 
;;; 3) Repeat step#2 until there are (V-1) edges in the spanning tree.

;; First sort all edges by order of weights.

;; To do this recursively, keep removing weights and their respective edges 
(define (orderWeights wG)
  (cond ((null? (getW wG)) (triple (getV wG) '() '()))
        (else (EW++ (getByIndex (getIndex (minInLat (getW wG)) (getW wG)) (getE wG))
                    (getByIndex (getIndex (minInLat (getW wG)) (getW wG)) (getW wG))
                    (orderWeights (remWGEdge (getByIndex (getIndex (minInLat (getW wG)) (getW wG)) (getE wG)) wG))))))


(define (MST wG)
  (define (MSTiter wGsort i rsf)
    (cond ((zero? i) rsf)
          ((acyclic? (EW++ (car (getE wGSort)) (car (getW wGsort)) rsf))
           (MSTiter (EW-- wGsort) (- i 1) (EW++ (car (getE wGSort)) (car (getW wGsort)) rsf)))
          (else (MSTiter (EW-- wGsort i rsf))))
    )
  (MSTiter (orderWeights wG) (- (lat-length (getV wG)) 1) (cons (getV wG) '(() ()))))

