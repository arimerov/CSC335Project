;; Labeled Undirected Graph, need to have a relation! G <- (V, E) <- Label
;;;; where Label =: (L1 L2) | (and [#(L1) == #(V)] [#(L2) == #(E)] [(lat? L1)] [(lat? L2)])
;;; For practicality, the user must be able to assign their own labeling to the graph G, and call values using it.
;;; ^^ Think of this like indexing both V and E, and being able to specify a vertix by the first label, and the edge by L2
;;; ^ Start indexing at 0?
;;; Also, think of V and E are data and Label gives the data a name/pointer to that data.

;;; To make our current setup a Labeled Undirected Graph, we just need to establish helper functions to call values by index.

; Assume G is an Undirected Graph, thus (undirGraph? G) -> #t

; Labeling a graph is as easy as referring to a vertex or edge by a number. To make it easier, start counting from 0

; pre: G is an undirected graph!
(define (undir->labeled G)
  (cond ((and (null? (getV G)) (null? (getE G))) (pair 0 0))
        ((null? (getV G)) (second++ (undir->labeled (pair (getV G) (cdr (getE G))))))
        ((null? (getE G)) (first++ (undir->labeled (pair (cdr (getV G)) (getE G)))))
        (else (first++ (second++ (undir->labeled (pair (cdr (getV G)) (cdr (getE G)))))))))

; Now that we have a function that returns the number of elements in V and E, can consider the graph labeled.
; Like an array, the index the vertices in V is the vertex's label, and likewise for the l-pair's index in E.

; Now must develop functions to fetch the value at the specified label:
 
(define (l1->v l G)
  (cond ((= l 0) (car (getV G)))
        (else (l1->v (- l 1) (V-- G)))))
(define (l2->e l G)
  (cond ((= l 0) (car (getE G)))
        (else (l2->e (- l 1) (E-- G)))))

; Likewise, should have functions to get the index of an inputted value:
; ^ No need to specify 'first' since it's implied G is made of sets.

; Assume V in G is non-empty
(define (v->l1 v G)
  (if (atom? v)
      (cond ((null? (getV G)) 0) ; So we get a value == cap if value not found
            ((equal? v (car (getV G))) 0)
            (else (+ 1 (v->l1 v (V-- G)))))
      #f)) ;return #f if invalid v

(define (e->l2 e G)
  (if (l-pair? e)
      (cond ((null? (getE G)) 0)
            ((equal? e (car (getE G))) 0)
            (else (+ 1 (e->l2 e (E-- G)))))
      #f)) ;return #f if invalid e