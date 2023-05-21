; Checks if G is a directed graph
(define (dirGraph? G)
  (and (VSet? (getV G)) (ESet? (getE G)) (EVDomain? (getE G) (getV G))))



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
(define (getAllDirEdgeAdj v G)
  (cond ((dirEdge? v G) (cons (getDirEdgeAdj v G) (getAllDirEdgeAdj v (remEdge (getEdge v G) G))))
        (else '())))

; Directed Graphs
(define (findHeads G)
  (cond ((null? (getV G)) '())
        ((inSet? (car (getV G)) (seconds (getE G))) (findHeads (V-- G)))
        (else (cons (car (getV G)) (findHeads (V-- G))))))
;; ^ can easily flip to find tails!
(define (findTails G)
  (cond ((null? (getV G)) '())
        ((inSet? (car (getV G)) (firsts (getE G))) (findTails (V-- G)))
        (else (cons (car (getV G)) (findTails (V-- G))))))


; Fancy stuff
; (forvInSUn S G funct Un)
(define (forvInSUn S G funct Un)
  (cond ((null? (cdr S)) (funct (car S) G))
        (else (Un (funct (car S) G) (forvInSUn (cdr S) G funct Un)))))

; (forvInSUn S G funct Un) := for all v listed in set S, perform (funct v G).
; ^ Depending on what the function returns details the 'Un' aka 'union'.
; ^^ In the case of topSort, I want to do a recursive call to itself for each head.
; Thus funct == (topSort G1), where G1 = (remVerts S (remEdgesFromV S G))


