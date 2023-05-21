; Fancy stuff
; (forvInSUn S G funct Un)
(define (forvInSUn S G funct Un)
  (cond ((null? (cdr S)) (funct (car S) G))
        (else (Un (funct (car S) G) (forvInSUn (cdr S) G funct Un)))))


(define (topSortIter v G)
  (cond ((noDirEdge? v G) (cons v '()))
        ((oneDirEdge? v G) (cons v (topSortIter (getDirEdgeAdj v G) G)))
        (else (altUnion (topSortIter v (remDirEdge v G)) (topSortIter (getDirEdgeAdj v G) G)))))
        ; ^ else, two or more path, aka branch

;; Assume Acyclic!
;(define (topSort G)
(define (topSort G)
  (setify (forvInSUn (findHeads G) G topSortIter altUnionCom)))

(define (cdrUntilMatch v L2)
  (cond ((null? L2) L2)
        ((equal? (car L2) v) L2)
        (else (cdrUntilMatch v (cdr L2)))))

(define (findVCommon L1 L2)
  (cond ((null? L1) #f)
        ((inSet? (car L1) L2) (car L1))
        (else (findVCommon (cdr L1) L2))))

(define (commonLabelIndexDiff v L1 L2)
  (- (getIndex v L1) (getIndex v L2)))

(define (popIter n L)
  (cond ((zero? n) '())
        ((null? L) '())
        (else (cons (car L) (popIter (- n 1) (cdr L))))))
(define (remIter n L)
  (cond ((zero? n) L)
        ((null? L) L)
        (else (popIter (- n 1) (cdr L)))))


(define (altUnionCom L1 L2)
  (let ((v (findVCommon (cdr L1) L2)) (i (commonLabelIndexDiff (findVCommon (cdr L1) L2) L1 L2)))
    (if (> i 0) (union (popIter i L1) (altUnion (remiter i L1) L2)) (union (popIter i L2) (altUnion (remiter i L2) L1)))))