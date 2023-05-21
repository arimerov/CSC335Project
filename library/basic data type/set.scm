
; Not really needed, but nice to have from The Little Schemer:
(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2)) (= a1 a2))
          ((or (number? a1) (number? a2)) #f)
          (else (eq? a1 a2)))))
(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
          ((or (null? l1) (null? l2)) #f)
          (else (and (equal? (car1) (car2)) (eqlist? (cdr l1) (cdr l2)))))))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
(define first
  (lambda (p)
    (cond (else (car p)))))
(define second
  (lambda (p)
    (cond (else (car (cdr p))))))
(define pair
  (lambda (s1 s2)
    (cond (else (cons s1 (cons s2 (quote ())))))))

(define (firsts E)
  (cond ((null? E) '())
        (else (cons (first (car E)) (firsts (cdr E))))))
(define (seconds E)
  (cond ((null? E) '())
        (else (cons (second (car E)) (seconds (cdr E))))))

; pre: p is a pair
(define (first++ p)
  (pair (+ 1 (first p)) (second p)))
(define (second++ p)
  (pair (first p) (+ 1 (second p))))
(define (first-- p)
  (pair (- 1 (first p)) (second p)))
(define (second-- p)
  (pair (first p) (- 1 (second p))))


(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
(define (latPair? E)
  (cond ((null? E) #t)
        ((l-pair? (car E)) (latPair? (cdr E)))
        (else #f)))
(define (revPair p)
  (if (l-pair? p) (pair (second p) (first p))))
(define (lat-length VSet)
  (cond ((null? VSet) 0)
        (else (+ 1 (lat-length (cdr VSet))))))

(define (set? S)
  (cond ((null? S) #t)
        ((inSet? (car S) (cdr S)) #f)
        (else (and #t (set? (cdr S))))))
(define (inSet? a S)
  (cond ((null? S) #f)
        ((equal? a (car S)) #t)
        (else (or #f (inSet? a (cdr S))))))

(define (setify S)
  (cond ((null? S) S)
        ((inSet? (car S) (cdr S)) (setify (cdr S)))
        (else (cons (car S) (setify (cdr S))))))
(define (undirSet? S)
  (cond ((null? S) #t)
        ((number? (car S)) #f)
        ((not (l-pair? (car S))) #f)
        ((or (inSet? (car S) (cdr S)) (inSet? (revPair (car S)) (cdr S))) #f)
        (else (and #t (undirSet? (cdr S))))))
(define (undirSetify S)
  (cond ((null? S) S)
        ((or (inSet? (car S) (cdr S)) (inSet? (revPair (car S)) (cdr S))) (undirSetify (cdr S)))
        (else (cons (car S) (undirSetify (cdr S))))))


(define (union S1 S2)
  (cond ((null? S1) S2)
        (else (setify (cons (car S1) (union (cdr S1) S2))))))
(define (altUnion L1 L2)
  (cond ((null? L1) L2)
        ((null? L2) L1)
        (else (cons (car L1)
                    (cons (car L2)
                          (altUnion (cdr L1) (cdr L2)))))))

(define (disjoint? S1 S2)
  (cond ((null? S1) #t)
        ((inSet? (car S1) S2) #f)
        (else (and #t (disjoint? (cdr S1) S2)))))

; (aka our version of 'Stack'
(define (consEnd a list)
  (cond ((null? list) (cons a '()))
        (else (cons (car list) (consEnd a (cdr list))))))


(define (altUnionCom L1 L2)
  (let ((v (findVCommon (cdr L1) L2)) (i (commonLabelIndexDiff (findVCommon (cdr L1) L2) L1 L2)))
    (if (> i 0) (union (popIter i L1) (altUnion (remiter i L1) L2)) (union (popIter i L2) (altUnion (remiter i L2) L1)))))

; (getIndex v S) := given element v, return index in set S.
(define (getIndex v S)
  (cond ((null? S) 0) ; So we get a value == cap if value not found
        ((not (atom? v)) #f)
        ((= v (car S)) 0)
        (else (+ 1 (getIndex v (cdr S))))))
; (getByIndex i S) := Given index i, return element in index i of set S.
(define (getByIndex i S)
  (cond ((null? S) #f)
        ((zero? i) (car S))
        (else (getByIndex (- i 1) (cdr S)))))
; (remByIndex i S) := Removes element at index i in S.
(define (remByIndex i S)
  (cond ((null? S) S)
        ((zero? i) (cdr S))
        (else (cons (car S) (remByIndex (- i 1) (cdr S))))))








