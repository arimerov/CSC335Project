; Part 1.1 Documentation:

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

(load "part1_1.scm")

;; Part 1.2!

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

; Define new data types:
;;; TREE: Undirected Graph in which any two vertices are connected by exactly one path!
;;; ^^ equivalently: A connected acyclic undirected graph!
;;;;; ACYCLIC: a graph havining no graph cycles. Bipartite! Connected == Tree. Unconnect == Forest.
;;;;; BIPARTITE: Exists set of vertices decomposed into 2 disjoint sets
;;;;; ^^ s.t. no two graph vertices within the same set are adjacent.
;;;;; ADJACENT (Vertecies): Two vertices are adjacent if there is an edge between two vertices.

;;; CLIQUE: Subset of vertices of an undirected graph s.t. (for all) v1, v2 (in) C (subset) V -> v1 (adjacent) v2





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


; Define new Graph Types:
;; Labeled Undirected Graphs:
;;;; Graph Labeling: The assignment of labels, traditionally represented by integers, to edges and/or vertices of a graph.


; Thus, to create:

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


; pre: p is a pair
(define (first++ p)
  (pair (+ 1 (first p)) (second p)))
(define (second++ p)
  (pair (first p) (+ 1 (second p))))
(define (first-- p)
  (pair (- 1 (first p)) (second p)))
(define (second-- p)
  (pair (first p) (- 1 (second p))))

(define (V-- G)
  (pair (cdr (getV G)) (getE G)))
(define (E-- G)
  (pair (getV G) (cdr (getE G))))
(define (VE-- G)
  (pair (cdr (getV G)) (cdr (getE G))))

(define (V++ v G)
  (pair (cons v (getV G)) (getE G)))
(define (E++ e G)
  (pair (getV G) (cons e (getE G))))


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


; Develop Depth-First and Breadth-First Search:
;;; In both cases, assume v0 as root. Returns path from root to target.
;;; Later abstract so that root is a parameter.

; Thus, consequential for definition of data type 'path'.

; DFS requires goes down a branch as far as possible before backtracking.

; pre: G is an Undirected Graph

(define (depFirst vStart vTar G)
  (let ((V (getV G)) (E (getE G)))
    (cond ((null? V) '(())) ;If V is null, then all vertices have been searched
          ((null? E) '(())) ;If E is null, then all edges have been searched
          ((not (and (inSet? vStart V) (inSet? vTar V))) '(())) ;If either inputted vertex doesn't exist in V, then there's no solution.
          ((vEdge? vStart G) (let ((G1 (remEdge (getEdge vStart G) G)))
                                   (cond ((equal? vTar (getEdgeAdj vStart G)) (pair vStart vTar))
                                         ((inSet? (getEdgeAdj vStart G) V)
                                          (let ((B1 (depFirst (getEdgeAdj vStart G) vTar (remVert vStart G1))))
                                            (if (lat? B1) (cons vStart B1) (depFirst vStart vTar G1))))
                                         (else (depFirst vStart vTar G1)))))
          (else '()))))
; ^Imma be honest, I winged this one. I can't believe it actually worked. Probably has a bug tho no cap.



;;(let (
;  (path (cons vStart (depFirst (getEdgeAdj vStart G) vTar (remEdge (getEdge vStart G) (remVert vStart G)))))
;  (G1 (remEdge (getEdge vStart G) G))
;  )
;(cond ((lat? path) path)
;      ((vEdge? vstart G1) (depFirst vStart vTar G1))
;      (else '(())))))

; Using '(()) for termination resulting in #f because the proper result should be a lat, allowing lat? to test if result is true.

;; (vEdge? v G) := #t if (exists) edge in G containing v in the pair.
(define (vEdge? v G)
  (cond ((null? (getE G)) #f)
        ((equal? v (first (car (getE G)))) #t)
        ((equal? v (second (car (getE G)))) #t)
        (else (vEdge? v (E-- G)))))
;; (getEdge v G) := Returns first found edge pair containing v.
(define (getEdge v G)
  (cond ((null? (getE G)) '()) ; Will return empty if not found
        ((equal? v (first (car (getE G)))) (car (getE G)))
        ((equal? v (second (car (getE G)))) (car (getE G)))
        (else (getEdge v (E-- G)))))
;; (getEdgeInd v G) := Returns index of first found edge pair containing v.
(define (getEdgeInd v G)
  (cond ((null? (getE G)) 0) ; Will return index == cap
        ((equal? v (first (car (getE G)))) 0)
        ((equal? v (second (car (getE G)))) 0)
        (else (+ 1 (getEdgeInd v (E-- G))))))
;; (getEdgeAdj v G) := Returns adjacent vertex in first found edge containing v.
(define (getEdgeAdj v G)
  (cond ((null? (getE G)) #f) ;No adjacent found
        ((equal? v (first (car (getE G)))) (second (car (getE G))))
        ((equal? v (second (car (getE G)))) (first (car (getE G))))
        (else (getEdgeAdj v (E-- G)))))

;; (remVert v G) := Removes v from G
(define (remVert v G)
  (cond ((null? (getV G)) '())
        ((equal? v (car (getV G))) (V-- G))
        (else (V++ (car (getV G)) (remVert v (V-- G))))))
;; (remEdge e G) := Removes e from G
(define (remEdge e G)
  (cond ((null? (getE G)) '())
        ((equal? e (car (getE G))) (E-- G))
        (else (E++ (car (getE G)) (remEdge e (E-- G))))))

; ^No need to specify first instance since V and E are sets 




; Algorithms to develop:
;; Labeled Undirected Graph -- Depth-First and Breadth-First
;;;; Let the first vertex (v0) alway be the root,
;;;;;; Code must be able to find all the vertices connected to v0.
;;;; Depth-first: From root, 



; Documentation so Far (Part 1.2):

;; (undir->labeled G) := Converts undirected graph G to 'labeled undirected graph'.
;; ^ Values returned are the number of vertices in V and edges in E, respectively
;; (first++ p) := Given pair p, assuming the first atom is numeric, returns p with first element incremented by 1.
;; (second++ p) := Given pair p, assuming the first atom is numeric, returns p with second element incremented by 1.
;; (first-- p) := Given pair p, assuming the first atom is numeric, returns p with first element decremented by 1.
;; (second-- p) := Given pair p, assuming the first atom is numeric, returns p with second element decrementedby 1.

;; (V-- G) := Takes cdr of V, hence removing the vertex labeled 0 in graph G.
;; (E-- G) := Removes the edges labeled 0 in graph G.
;? (VE-- G) := Removes both a vertex and edge labeled 0 in graph G. [Actually seems unnecessary; though less strain maybe?]

;; (V++ v G) := Adds v into the 0th index of G's set V.
;; (E++ e G) := Adds e into the 0th index of G's set E.
;; ^ After these operations, G should be checked to ensure set definition hasn't been violated due to insertion.

;; (l1->v l G) := Returns vertex in G labeled as the lth index/label (counting from 0)
;; (l2->e l G) := Returns edge in G labeled as the lth index/label (counting from 0)
;; (v->l1 v G) := Returns index/label of vertex v in G. Returns # of vertices in G if not found. #f if invalid v
;; (e->l2 e G) := Returns index/label of edge e in G. Returns # of edges in G if not found. #f if invalid e.

;; (vEdge? v G) := #t if (exists) edge in G containing v in the pair.
;; (getEdgeInd v G) := Returns index of first found edge pair containing v.
;; (getEdgeAdj v G) := Returns adjacent vertex in first found edge containing v.
;; (remVert v G) := Removes v from G
;; (remEdge e G) := Removes e from G


