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

; Most definitely should have a undirGraphify:
; (undirGraphify G) := Given an undirGraph G, ensure G passes EVDomain?, vSet?, and eSet?

(define (undirGraphify G)
  (cond ((not (vSet? (getV G))) (undirGraphify (pair (setify (getV G)) (getE G))))
        ((not (eSet? (getE G))) (undirGraphify (pair (getV G) (undirSetify (getE G)))))
        ; Now that V and E are guaranteed to be sets, remove edges with reference to a v not in V,
        ; And remove v's not specified in an Edge.
        (else (remDisconnect G))))

; (remDisconnect G) := With V and E guaranteed to be sets, remove edges with reference to a v not in V,
        ; And remove v's not specified in an Edge.

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

;; Suggestions of possible computations:
;;;; path?(v1, v2) -> BOOL
;;;; shortPath(v1, v2) -> [INT or LIST DETAILING SHORTEST ROUTE aka breFirst]
;;;; acyclic?(G) -> BOOL
;;;; connected?(G) -> BOOL [tech EVDomain?]
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

;; (tree? G) -> (and (connected? G) (acyclic? G) (undir? G))

;; (connected? G) <=> (EVDomain? (getE G) (getV G))



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
(define (VE-- G) ;Equivalent to (V-- (E-- G))
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
; RULES: As we traverse down an edge, the edge is removed from set E in G.
;;; As we leave a vertex, the vertex is removed from set V in G.


; pre: G is an Undirected Graph

(define (depFirst vStart vTar G)
  (let ((V (getV G)) (E (getE G)))
    (cond ((null? V) '(())) ;If V is null, then all vertices have been searched
          ((null? E) '(())) ;If E is null, then all edges have been searched
          ((not (and (inSet? vStart V) (inSet? vTar V))) '(())) ;If either inputted vertex doesn't exist in V, then there's no solution.
          ((vEdge? vStart G) (let ((G1 (remEdge (getEdge vStart G) G)))
                                   (cond ((equal? vTar (getEdgeAdj vStart G)) (pair vStart vTar))
                                         ((inSet? (getEdgeAdj vStart G) V) ; If this is false, then the graph is cyclic!
                                          (let ((D1 (depFirst (getEdgeAdj vStart G) vTar (remVert vStart G1)))) ; D1 prioritizes traversing the edge first
                                            (if (lat? D1) (cons vStart D1) (depFirst vStart vTar G1)))) ; If traversing the edge does not yield a correct answer, then it calls itself with the same vertex and the edge removed.
                                         (else (depFirst vStart vTar G1))))) ; Else ensure the code doesn't traverse a loop during search.
          (else '(())))))

; Breadth-First should be similar, but instead calls (depFirst vStart vTar G1) since looking at all connections if first priority.

(define (breFirst vStart vTar G)
  (let ((V (getV G)) (E (getE G)))
    (cond ((null? V) '(())) ;If V is null, then all vertices have been searched
          ((null? E) '(())) ;If E is null, then all edges have been searched
          ((not (and (inSet? vStart V) (inSet? vTar V))) '(())) ;If either inputted vertex doesn't exist in V, then there's no solution.
          ((vEdge? vStart G) (let ((G1 (remEdge (getEdge vStart G) G)))
                                   (cond ((equal? vTar (getEdgeAdj vStart G)) (pair vStart vTar))
                                         ((inSet? (getEdgeAdj vStart G) V)
                                          (let ((B1 (breFirst vStart vTar G1)))
                                            (if (lat? B1) B1 (cons vStart (breFirst (getEdgeAdj vStart G) vTar (remVert vStart G1))))))
                                         (else (cons vStart (breFirst (getEdgeAdj vStart G) vTar (remVert vStart G1)))))))
          (else '(())))))

; Now having Depth-First and Breadth-First, we can easily define path and acyclic.


;(define (acyclic? G)
;  (cond ((null? (getV G)) #t)
;        ((null? (getE G)) #t)
        ; If (vEdge? (car (getV G))) returns #f, then there's nowhere to go, and at the end of the graph with no loops found:
;        ((not (vEdge? (car (getV G)) G)) #t)
        ; If (getEdgeAdj (car (getV G)) G) is in Set V, then call this function with both vertex and edge removed.
;        ((inSet? (getEdgeAdj (car (getV G)) G) (getV G)) (and (acyclic? (remEdge (getEdge (car (getV G)) G) G)) (acyclic? (V-- (remEdge (getEdge (car (getV G)) G) G)))))
        ; ^ Must check for cycles down the first edge found, and any other possible edges containing (car (getV G)).
        ; If (getEdgeAdj (car (getV G)) G) is not in Set V, then there must be a cycle:
;        (else #f)))

; After some testing, it was found that the initial code improperly returns #f since (acyclic? (V-- (remEdge (getEdge (car (getV G)) G) G))) doesn't remove all occurrences of v in edges.
;; The code gets confused since the other edge that can possibly be traversed, yet not directly connected to the branch looked at currently contains the vertex just removed, and hence returns #f.
;; Thus, there needs to be a check to see if there's a connection between the two branches. If there is, then there's a cycle!

; Procedure for acyclic? should be be as follows:
; 1) Is V null? -> #t
; 2) Is E null? -> #t
; 3) If v has no branches -> #t
; 4) If v has 1 branch -> remove v and respective e from G, and see if that graph is acyclic
; 5) 2? -> Is there a connected between the two branches? (Yes -> #f; No -> #t)
; 6) 3, 4, 5? -> (exists) connection b/w any of the brancheds -> #f; Else -> #t
; Thus, 5 and 6 are 'else' where (and (not (connected? G G1)) (acyclic? (remEdge (getEdge (car (getV G)))))
;; ^ where G is current 'first' edge, and G1 is graph with 'first' edge removed and thus checking second edge.
;; ^^ Guaranteed second edge because case 3 was found false.

; Else: return: (not (connected? Branch 1 and Branch2)) AND (acyclic? (G with edge removed)) <- To cover 3 or more edges
; ^ Where B1 == G following first found edge and with prior vertex, traversed edge, and all disconnected edges removed.
; --------B2 == G following second found edge and with prior vertex, traversed edge, and all disconnected edges removed.
; ^^ 

(define (acyclic? G)
  (let ((v (car (getV G))))
    (cond ((null? (getV G)) #t)
          ((null? (getE G)) #t)
          ; If (vEdge? (car (getV G))) returns #f, then there's nowhere to go, and at the end of the graph with no loops found:
          ((not (vEdge? v G)) #t)
          ((oneEdge? v G) (acyclic? (V-- (remEdge (getEdge v G) G))))
          (else (let ((B1 (undirGraphify (V-- (remEdge (getEdge v G) G)))) (B2 (undirGraphify (V-- (remEdge (nextEdge v G) G)))))
                  (and (not (connection? B1 B2)) (acyclic? (remEdge (getEdge v G) G))))))))
; ^need to fill in B1 and B2 at some point.

; Make helper:
; (moreEdge? v G) := #t if more than 1 edge in G containing v; #f if only 1 or none.
(define (moreEdge? v G)
  (cond ((not (inSet? v (getV G))) #f) ; Not in graph, so can't have an edge
        ((not (vEdge? v G)) #f) ; Doesn't have an edge, so #f for 0
        ((vEdge? v (remEdge (getEdge v G) G)) #t)
        (else #f)))

; (oneEdge? v G) := #t if only 1 edge in G containing v; #f is 0 or more than 1 edge with v.
(define (oneEdge? v G)
  (cond ((not (inSet? v (getV G))) #f) ; Not in graph, so can't have an edge
        ((not (vEdge? v G)) #f) ; Doesn't have an edge, so #f for 0
        ((moreEdge? v G) #f)
        (else #t)))

(define (nextEdge v G)
  (getEdge v (remEdge (getEdge v G) G)))

; (connection? G1 G2) := G1 and G2 have a connection if they have an edge connecting from a vertex from G1 to a vertex in G2.
; if: (exists) v s.t. v (in) G1 and v (in) G2, then #t if (and (vEdge? v G1) (vEdge? v G2))
; if: no shared v, then there's no way to be connected unless I messed up. Can check for mess up if (EVDomain? G) => #f
; Just inserted (connected? G1) AND (connected? G2) to check for mess-up incase the tree itself is not connected.

(define (vShare? G1 G2)
  (cond ((null? (getV G1)) #f)
        ((inSet? (car (getV G1)) (getV G2)) #t)
        (else (or #f (vShare? (V-- G1) G2)))))

(define (getVShare G1 G2)
  (cond ((null? (getV G1)) #f)
        ((inSet? (car (getV G1)) (getV G2)) (car (getV G1)))
        (else (getVShare (V-- G1) G2))))

(define (connection? G1 G2)
  (cond ((VShare? G1 G2) (let ((v (getVShare G1 G2))) (and (vEdge? v G1) (vEdge? v G2) (connected? G1) (connected? G2))))
        (else #f)))

; Thus, a singular tree is connected if all the listed vertices appear in at least a specified edge
(define (connected? G)
  (cond ((null? (getV G)) #t)
        ((vEdge? (car (getV G)) G) (and #t (connected? (V-- G))))
        (else #f)))

; Should I make a function that takes a disconnected graph and returns a set of vertices which are disconnected?


;;(let (
;  (path (cons vStart (depFirst (getEdgeAdj vStart G) vTar (remEdge (getEdge vStart G) (remVert vStart G)))))
;  (G1 (remEdge (getEdge vStart G) G))
;  )
;(cond ((lat? path) path)
;      ((vEdge? vstart G1) (depFirst vStart vTar G1))
;      (else '(())))))

; Using '(()) for termination resulting in #f because the proper result should be a lat, allowing lat? to test if result is true.

;; (vEdge? v G) := #t if (exists) edge in G containing v in the pair. (look at a vertice in a graph. do you see an edge?)
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


