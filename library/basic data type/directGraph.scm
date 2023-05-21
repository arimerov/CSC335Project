(define (dirEdge? v G)
  (inSet? v (firsts (getE G))))

(define (noDirEdge? v G)
  (not (inSet? v (firsts (getE G)))))


(define (oneDirEdge? v G)
  (and (dirEdge? v G) (noDirEdge? v (remDirEdge v G))))

(define (getDirEdge v G)
  (cond ((null? (getE G)) #f)
        ((equal? v (first (car (getE G)))) (car (getE G)))
        (else (getDirEdge v (E-- G)))))

(define (remDirEdge v G)
  (cond ((null? (getE G)) G)
        ((equal? v (first (car (getE G)))) (E-- G))
        (else (E++ (car (getE G)) (remDirEdge v (E-- G))))))

(define (getDirEdgeAdj v G)
  (cond ((null? (getE G)) #f)
        ((equal? v (first (car (getE G)))) (second (car (getE G))))
        (else (getDirEdgeAdj v (E-- G)))))