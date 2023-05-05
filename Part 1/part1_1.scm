; Define your own data structure for Graphs.

;; First define for undirected graphs.
;; Use pair (V E) -> G
;;;; Where V is a ""set"" of vertices.
;;;; ----- E is a ""set"" of edges.

;; Recommends defining data struct for:
;;;; Sets
;;;; Vertices
;;;; Edges
;; Design based on computations needed with this library

;; A set is: A lat of sexp s.t. no sexp repeat

;; First, we want to be able to use equal?
;; Thus, using 'The Little Schemer' as reference:

;(define equal?
;  (lambda (s1 s2)
;    (cond ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
;          ((atom? s1) #f)
;          ((atom? 2) #f)
;          (else (eqlist? s1 s2)))))
;
;^ On compilation, it was found that equal? is already included in Scheme

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
;; Yes, eqlist? and equal? both call each other, but the cases in which they call each other will end in termination.

; Also need atom? (from The Little Schemer):
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; More borrowing from The Little Schemer for reference:
(define a-pair?
  (lambda (x)
    (cond ((atom? x) #f)
          ((null? x) #f)
          ((null? (cdr x)) #f)
          ((null? (cdr (cdr x))) #t)
          (else #f))))
(define first
  (lambda (p)
    (cond (else (car p)))))
(define second
  (lambda (p)
    (cond (else (car (cdr p))))))
(define pair
  (lambda (s1 s2)
    (cond (else (cons s1 (cons s2 (quote ())))))))
;^ Was initially called 'build' but I find 'pair' more convenient for understandability.

;; First need to understand what's a primitive and non-primitive in Scheme:
;(define primitive?
;  (lambda (l)
;    (eq? (first l) (quote primitive))))
;(define non-primitive
;  (lambda (l)
;    (eq? (first l) (quote non-primitive))))



  
;; Now define what a set is:
(define (set? S)
  (cond ((null? S) #t)
        ((inSet? (car S) (cdr S)) #f)
        (else (and #t (set? (cdr S))))))

;(define (dup? a S)
;  (cond ((null? S) #f)
;        ((equal? a (car S)) #t)
;        (else (or #f (dup? a (cdr S))))))
;
;; set? specifically checks if there's duplicates at the lat level.
;; That's because we only check if elements duplicate, and not atoms.

;; Just compiled and tested. It works! At least for testing for sets of vertices and edges


; Let's define datatypes for sets of vertices (V) and sets of edges (E):
;;; V ::= () | (cons atom V) iff (dup? atom V) -> #f
;;; E ::= () | (cons edge E) iff (dup? edge E) -> #f
;;; edge ::= (vertex1 vertex2) iff (and (in? vertex1 V) (in? vertex2 V)) -> #t

;;; in? is the same as dup? so I shall rename dup? -> inSet?

(define (inSet? a S)
  (cond ((null? S) #f)
        ((equal? a (car S)) #t)
        (else (or #f (inSet? a (cdr S))))))

; Adjust BNF's:
;;; V ::= () | (cons atom V) iff (inSet? atom V) -> #f
;;; E ::= () | (cons edge E) iff (inSet? edge E) -> #f
;;; edge ::= (vertex1 vertex2) iff (and (inSet? vertex1 V) (inSet? vertex2 V)) -> #t

; Define above BNF's
;(define (vertex? a V)
;  (cond ((not (set? V)) #f)
;        ((null? V) #f)
;        ((inSet? a V) #t)
;        (else #f)))
;
;^ If (set? V) -> #f, should we setify(V)?
; Also, clearly unnecessary since same as (inSet? vertex V)

; VSet? should test if (and (set? V) (lat? V))
(define (VSet? V)
  (and (set? V) (lat? V)))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
;^ From The Little Schemer

; ESet? should test if (and (set? E) (latPair? E))
(define (ESet? E)
  (and (set? E) (latPair? E)))
;^ latPair? checks if E is a lat of l-pairs.
; An l-pair is a lat with 2 elements.
(define (latPair? E)
  (cond ((null? E) #t)
        ((l-pair? (car E)) (latPair? (cdr E)))
        (else #f)))
(define (l-pair? e)
  (and (number? (car e)) (number? (car (cdr e))) (null? (cdr (cdr e)))))


(define (graph? G)
  (and (VSet? (getV G)) (ESet? (getE G)) (EVDomain? (getE G) (getV G))))
(define (getV G)
  (car G))
(define (getE G)
  (car (cdr G)))

(define (EVDomain? E V)
  ;; Assume E and V are proper sets.
  (cond ((null? E) #t)
        ((and (inSet? (first (car E)) V) (inSet? (second (car E)) V)) (and #t (EVDomain? (cdr E) V)))
        (else #f)))

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
(define (revPair p)
  (if (l-pair? p) (pair (second p) (first p))))

(define (undirSetify S)
  (cond ((null? S) S)
        ((or (inSet? (car S) (cdr S)) (inSet? (revPair (car S)) (cdr S))) (undirSetify (cdr S)))
        (else (cons (car S) (undirSetify (cdr S))))))

(define (undirGraph? G)
  (and (VSet? (getV G)) (ESet? (getE G)) (undirSet? (getE G)) (EVDomain? (getE G) (getV G))))


; Documentation so far:

;; (eqan? a b) := Checks if a and b are the same atom. #t if yes. #f if either is not atom or not equal.
;; (eqlist? a b) := Checks if a and b are the same list.
;; (atom? a) := #t if a is an atom.
;; (first a) := Short for (car a)
;; (second a) := Short for (car (cdr a))
;; (pair a b) := Short for (cons a (cons b '())); Thus building (a b) pair. Will be handy for making edges.

;; (set? S) := #t if S is a set.
;; (inSet? a S) := #t if a is an element in set S. Works for finding if an l-pair is in set S as well!
;; (VSet? V) := #t if V is a set of Vertices (which are atoms).
;; (ESet? E) := #t if E is a set of Edges (which are l-pairs). NOTE: REDUNDANT FOR UNDIRECTED GRAPHS! (A B) == (B A)
;; (latPair? E) := #t if E is a lat of l-pairs.
;; (l-pair? e) := #t if e is a lat with two elements.

;; (graph? G) := Checks that G is a viable graph. Need to ensure elements in E's l-pairs exist in V!
;; (getV G) := Returns set V of graph G. Same as (car G)
;; (getE G) := Returns set E of graph G. Same as (car (cdr G))
;; (EVDomain? E V) := #t if all edges connect only vertices specified in domain V
;; (setify S) := Removes duplicates in lat to make S a set.

;; (undirSet? S) := Checks for occurrences of (and (A B) (B A)). In undirected graph, this makes edges not a set!
;; (revPair p) := Takes in pair p := (A B) and return (B A).
;; (undirSetify S) := Removes duplicates in lat where (A B) and (B A) are considered duplicates.
;; (undirGraph? G) := #t if G is an undirected graph.


; Next Steps:

;? (buildV v1 v2 ... vN) := Returns set V with vertices v1, v2, ..., vN.
;? (buildE v1 v2 ... vN) := Returns set E with edges (v1 v2), (v3 v4), ..., (v(N-1) vN).
;? (buildG V E) := Returns graph G in formate (V E).
;? (printG G) := Print (list of) graph G to terminal.











;; Suggestions of possible computations:
;;;; path?(v1, v2) -> BOOL
;;;; shortPath(v1, v2) -> INT
;;;; acyclic?(G) -> BOOL
;;;; connected?(G) -> BOOL
;;;; spanningTree(G) -> TREE (requires defining TREE)
;;;; diameter(G) -> INT
;;;; bipartite?(G) -> BOOL
;;;; largeClique(G) -> CLIQUE (requires defining CLIQUE)

;; Thus, the new data types are TREE and CLIQUE to be defined.






;(Make each a separate scm?! That could emphasize what changes are needed!!) 
; What changes needed to support:
;;; a) Labeled Undirected Graphs
;;; b) Directed Graphs
;;; c) Weighted Graphs

; Algorithms needed to apply for:
;;; a -> Depth-First
;;; a -> Breadth-First
;;; b -> Topological
;;; c -> Minimum Spanning Tree


; Part 2: Make new data struct for one or both:
;;; 1) Adjacency List Rep.
;;; 2) Adjacent Matrix Rep.
;; Do the algorithms adapted to Part 1's structure apply to the rep(s) chosen? Show how..