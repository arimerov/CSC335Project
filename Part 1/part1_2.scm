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

; Checks I want to develop:

;; acyclic?(G)
;; ^ path?(v1, v2) <- equivalent to connected?
;; tree?(G)
;; bipartite?(G) << what is this?

;; undirGraphifyBetter(G)

;; path?(G): Use DFS or BFS to find if a path exists and return bool
;; acyclic?(G): If I want to check if a cycle exists at v1. first check if there are at least two edges.
;; ^^ If only 1 edge, no way for cycle.
;; ^^ If only 2 edges, assume first edge connects v1 to v2, then check path?(v1, v2, G) where G is the initial graph with edge (v1 v2) removed.
;; ^^ If there are 3 or more edges (let e represent # of edges connected to v1), then we need to check path? for (e-1) of the connected edges.


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
  (cond ((null? (getV G)) G)
        ((equal? v (car (getV G))) (V-- G))
        (else (V++ (car (getV G)) (remVert v (V-- G))))))
;; (remEdge e G) := Removes e from G
(define (remEdge e G)
  (cond ((null? (getE G)) G)
        ((equal? e (car (getE G))) (E-- G))
        (else (E++ (car (getE G)) (remEdge e (E-- G))))))




;; path?(vStart, vTar, G) :== #t if path exists b/w vStart and vTar. #f otherwise

(define (path? vStart vTar G)
  (lat? (breFirst vStart vTar G)))

;; acyclic?(G): If I want to check if a cycle exists at v1. first check if there are at least two edges.
;; ^^ If only 1 edge, no way for cycle. -> Remove vertex from G, remove edge from graph, check acyclic? of new graph.
;; ^^ If only 2 edges, assume first edge connects v1 to v2, then check path?(v1, v2, G) where G is the initial graph with edge (v1 v2) removed.
;; ^^ If there are 3 or more edges (let e represent # of edges connected to v1), then we need to check path? for (e-1) of the connected edges.


; pre: G is a connected graph.
(define (acyclic? G)
  (cond ((null? (getV G)) #t)
        ((null? (getE G)) #t)
        ((not (vEdge? (car (getV G)) G)) (and #t (acyclic? (V-- G))))
        ((oneEdge? (car (getV G)) G) (and #t (acyclic? (V-- (remEdge (getEdge (car (getV G)) G) G)))))
        (else (and (not (path? (getEdgeAdj (car (getV G)) G) (car (getV G)) (remEdge (getEdge (car (getV G)) G) G)))
                   (acyclic? (remEdge (getEdge (car (getV G)) G) G))))))


; (acyclify G) := Takes in G and removes edges s.t. G is acyclic.
; pre: G is an undirected graph
(define (acyclify G)
  (cond ((null? (getV G)) G)
        ((null? (getE G)) G)
        ((not (vEdge? (car (getV G)) G)) (V++ (car (getV G)) (acyclify (V-- G))))
        ((oneEdge? (car (getV G)) G) (V++ (car (getV G)) (E++ (getEdge (car (getV G)) G) (acyclify (V-- (remEdge (getEdge (car (getV G)) G) G))))))
        ((path? (getEdgeAdj (car (getV G)) G) (car (getV G)) (remEdge (getEdge (car (getV G)) G) G)) ;Path between current node and edgeAdj
         (remEdge (getEdge (car (getV G)) G) (acyclify (remEdge (getEdge (car (getV G)) G) G))))
        (else (acyclify (remEdge (getEdge (car (getV G)) G) G)))))

; (graphUnion G1 G2) <= Takes data from two undirected graphs with same labeling and merges them into one, ensuring sets of both V and E too!
(define (graphUnion G1 G2)
  (pair (setify (union (getV G1) (getV G2))) (undirSetify (union (getE G1) (getE G2)))))

(define (dirGraphUnion G1 G2)
  (pair (setify (union (getV G1) (getV G2))) (setify (union (getE G1) (getE G2)))))
                      

; connected?(G) := returns #t if G is a connected graph; #f if G is disconnected
; my idea: iterate through set V (aka check every vertex).
;; for any v1, v2 (in) V

;; (V E) -> G
;; if G is connected then every vertex in G has an edge to another vertex in G
;; since G is defined, every vertex in G is listed in the set (getV G).
;; (car (getV G) -> (null? (getV G)) terminating argument.
;; Assume (car (getV G)) is the root.

;; 1 loop! Assume v0 <- (car (getV G)) as root. Test if there is path?(v0, (all v in G)).
;; ^ if yes! then there is a connection/path from v0 to every vertex in G, thus G must be connected!

;; Init function with l1 = 0; EVDomain?(G.V, G.E) -> #t
(define (connected? G)
  (define (connected?iter G l1)
    (cond ((>= l1 (first (undir->labeled G))) #t)
          ((null? (getV G)) #t)
          (else (and (path? (car (getV G)) (l1->v l1 G) G) (connected?iter G (+ 1 l1))))))
  (connected?iter G 1))

        ; ((not (vEdge? (l1->v l1 G) G)) #f) <- Covered by else statement. B/c if vEdge? doesn't return #t, there are no edges.
        ; ((vEdge? (l1->v l1 G) G) (and (inSet? (getEdgeAdj (l1->v l1 G) G) (getV G)) (connected? G (+ 1 l1))))
        ;; ^If EdgeAdj (in) G, then both v_l1 and edgeadj are 'clear for connection'
        ;; ^If EdgeAdj (not) (in) G, then wtf is edge adj doing as an edge?
        ;; ^^This just means that G is not a proper undirected graph


;; '((1 2 3 4 5 6) ((1 2) (3 4) (4 5) (5 6) (6 3)))


;; pre: G is undirected graph
(define (tree? G)
  (and (acyclic? G) (connected? G)))
; G is an undirected graph that is both acyclic and connected <=> G must be a tree!

(define (adjacent? v1 v2 G)
  (or (inSet? (pair v1 v2) (getE G)) (inSet? (pair v2 v1) (getE G))))

; Documentation so far:

;; (vEdge? v G) := #t if (exists) edge in G containing v in the pair. (look at a vertice in a graph. do you see an edge?)
;; (getEdge v G) := Returns first found edge pair containing v.
;; (getEdgeInd v G) := Returns index of first found edge pair containing v.
;; (getEdgeAdj v G) := Returns adjacent vertex in first found edge containing v.
;; (remVert v G) := Removes v from G
;; (remEdge e G) := Removes e from G

;; (path? vStart VTar G) := #t if path exists vStart -> vTar; #f otherwise
;; (depFirst vStart vTar G) := Returns the path found (if any) by doing dfs ; if no path then returns '(())
;; (breFirst vStart vTar G) := Returns the path found (if any) by doing bfs ; if no path then returns '(())

;; (tree? G) := Returns #t if G is a tree; #f otherwise
;; (adjacent? v1 v2 G) := Returns #t if 2 verticies are adjacent (i.e. there exists an edge connecting v1 and v2 in G), #f otherwise
;; (acyclic? G) := #t if is G is accrylic; #f if G has a cycle
;; (connected? G) := returns #t if G is a connected graph; #f if G is disconnected

;--------------------------------------------------------------------------------------------------------------------------



;;;;; BIPARTITE: Exists set of vertices decomposed into 2 disjoint sets
;;;;; ^^ s.t. no two graph vertices within the same set are adjacent.

;; DISJOINT SET: S1 (intersect) S2 == (empty set) [Share no elements]

; Start with helper function to decompose sets:
;; What is 'Decomposing'?
; If v0 is connected by an edge to v1, put v0 is S1 and v1 in S2.
; Then, if v1 is connected to vN, put vN in S1,
; Continue until no more vertices.
; Bipartite if (all) v (in) S1 are non-adjacent to each other
;; ^^ AND if (all) v (in) S2 are non-adjacent to each other 



;(define (bipartite? G)
;  (define (bipartite?iter G S1 S2 l1)
;    (cond ((null? (getV G)) (disjoint? S1 S2))
;          ((null? S1) (bipartite?iter (V-- G) (cons (car (getV G)) S1) (union (getAllEdgeAdj (car (getV G)) G) S2) (+ 1 l1)))
;          ;; ^^if S1 is null, then no vertices have been allocated yet and we are looking at the root. thus put root in S1.
;          ((inSet? (l1->v l1 G) S1) (bipartite?iter (V-- G) S1 (union (getAllEdgeAdj (car (getV G)) G) S2) (+ 1 l1)))
;          ((inSet? (l1->v l1 G) S2) (bipartite?iter (V-- G) (union (getAllEdgeAdj (car (getV G)) G) S1) S2 (+ 1 l1)))
;          (else #f)))
;  (bipartite?iter G '() '() 0))


; pre: G is not cyclic!
(define (bipartite? G)
  (define (bipartite?iter G S1 S2 shift)
    ;; ^^ NOTE: bool == #t means S1; #f means S2
    ;; Shift tells us how many vertices have been skipped over already [aka shift->l1 for (l1->v l1 G)]
    (cond ((null? (getV G)) (disjoint? S1 S2))
          ((null? S1) (bipartite?iter (V-- G) (cons (car (getV G)) S1) (union (getAllEdgeAdj (car (getV G)) G) S2) 0))
          ;; Must have a check that (l1->v shift G) doesn't yield error!
          ((>= shift (first (undir->labeled G))) (bipartite?iter G S1 S2 0))
          ((inSet? (l1->v shift G) S2) (bipartite?iter (remVert (l1->v shift G) G) (union (getAllEdgeAdj (l1->v shift G) G) S1) S2 0))
          ;; ^^ if the vertex we are looking at is in S2, remove vertex from G and add all edges to S1.
          ((inSet? (l1->v shift G) S1) (bipartite?iter (remVert (l1->v shift G) G) S1 (union (getAllEdgeAdj (l1->v shift G) G) S2) 0))
          (else (bipartite?iter G S1 S2 (+ 1 shift)))))
  
  (if (acyclic? G) (bipartite?iter G '() '() 0) #f))

; I'm trying to make a disjoint set of vertices.
; Start at root. put in S1. Then make two recursive calls!
; First call is going to traverse the first edge found. Going down the first edge, it will add v0 to S1, and vAdj to S2,
;; ^ This call will know that it's now looking at vAdj, and must be able to:
;;   Know that v0 is already in S1, so it doesn't need to mark/traverse that edge again
;; ^



;; Cases:
;; 1) If no edges, return S1 and S2.
;; 2) If 1 edge, traverse edge and remove edge from G
;; 3) If 2 edges, traverse edge 1


; Returns VSet.
(define (getAllEdgeAdj v G)
  (cond ((vEdge? v G) (cons (getEdgeAdj v G) (getAllEdgeAdj v (remEdge (getEdge v G) G))))
        (else '())))

; (union S1 S2)
(define (union S1 S2)
  (cond ((null? S1) S2)
        (else (setify (cons (car S1) (union (cdr S1) S2))))))

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

         

(define (disjoint? S1 S2)
  (cond ((null? S1) #t)
        ((inSet? (car S1) S2) #f)
        (else (and #t (disjoint? (cdr S1) S2)))))


; (acyclic? '((1 2 3 4 5 6) ((1 2) (3 4) (4 5) (5 6) (6 3))))



; Left of Part 1:
;; What changes needed for Directed Graphs? Then do Topological Sort!

;;;; ANSWER: Edges represent direction by... (v1 v2) := v1->v2; Thus edges (v1 v2) =/ (v2 v1).

; We were making directed graphs the entire time.
; Detailing an undirected graph in Scheme requires the implification of a dual direction between the nodes.
; No longer needing to check for revPair edge duplicates with eSet? and using (undirSetify E).
; Revert away from treating edges like dual directional, but instead one way--like arrows.

; tl;dr Restrict our current data structure to a more basic form. Increase restrictions.

; Helper:
(define (consEnd a list)
  (cond ((null? list) (cons a '()))
        (else (cons (car list) (consEnd a (cdr list))))))

(define (altUnion L1 L2)
  (cond ((null? L1) L2)
        ((null? L2) L1)
        (else (cons (car L1)
                    (cons (car L2)
                          (altUnion (cdr L1) (cdr L2)))))))

; Make tree...

;; connected acyclic undirected graph

; Given node/root,
; last? -> (cons (car V) '())
; (not (vEdge? (car V) G)) -> [remVert (all) v (not in) E]
; (oneEdge? (car V) G)) -> (cons (car V) (treeify (remEdge (getEdge (car V))) G) (getEdgeAdj v))
; (else "Take the


(define (treeify G v)
  (cond (((not (acyclic? G)) #f))
        ((not (vEdge? (car (getV G)) G)) (cons v '())) ; [remVert (all) v (not in) E]
        ((oneEdge? (car (getV G)) G) (cons v (treeify (remVert v (remEdge (getEdge v G) G))
                                                    (getEdgeAdj v G))))
        (else (altUnion (treeify (remVert v (remEdge (getEdge v G) G)) (getEdgeAdj v G))
                        (treeify (remEdge (getEdge v G) G) v)))))




; (cons (cons (treeify (remEdge (getEdge (car VG)) G)
;                   (treeify (re))))


; Topological sort
;(define (topSort G)
;  (topSortIter G (car (getV G))))


;;DON'T FORGET REMDISCONNECT EXISTS!




(define (firsts E)
  (cond ((null? E) '())
        (else (cons (first (car E)) (firsts (cdr E))))))
(define (seconds E)
  (cond ((null? E) '())
        (else (cons (second (car E)) (seconds (cdr E))))))

; Find all vertices in G where no edges point to them. <- Imma call them 'heads'
; aka iterate v, (inSet? (car V) (seconds E))
; Returns V of heads in G.

(define (findHeads G)
  (cond ((null? (getV G)) '())
        ((inSet? (car (getV G)) (seconds (getE G))) (findHeads (V-- G)))
        (else (cons (car (getV G)) (findHeads (V-- G))))))
;; ^ can easily flip to find tails!
(define (findTails G)
  (cond ((null? (getV G)) '())
        ((inSet? (car (getV G)) (firsts (getE G))) (findTails (V-- G)))
        (else (cons (car (getV G)) (findTails (V-- G))))))



; already have getAllEdgeAdj

; (remEdges E G) := Removes all Edges detailed in set E from graph G.

(define (remEdges E G)
  (cond ((null? E) G)
        (else (remEdges (cdr E) (remEdge (car E) G)))))

; (remVerts V G) := Removes all vertices detailed in set V from graph G.
(define (remVerts V G)
  (cond ((null? V) G)
        (else (remVerts (cdr V) (remVert (car V) G)))))

; (remEdgesFromV V G) := Input set of vertices V, and this removes all Edges directed away from each v (in) V.
(define (remEdgesFromV V G)
  (remEdges (getAssoEdges V G) G))

; (getAssoEdges V G) := Input st of vertices V, and this returns a set of all edges directing away from each v (in) V. 
(define (getAssoEdges V G)
  (cond ((null? V) '())
        ((inSet? (car V) (firsts (getE G))) (cons (getEdge (car V) G) (getAssoEdges V (remEdge (getEdge (car V) G) G))))
        (else (getAssoEdges (cdr V) G))))




; Fancy stuff
; (forvInSUn S G funct Un)
(define (forvInSUn S G funct Un)
  (cond ((null? (cdr S)) (funct (car S) G))
        (else (Un (funct (car S) G) (forvInSUn (cdr S) G funct Un)))))

; (forvInSUn S G funct Un) := for all v listed in set S, perform (funct v G).
; ^ Depending on what the function returns details the 'Un' aka 'union'.
; ^^ In the case of topSort, I want to do a recursive call to itself for each head.
; Thus funct == (topSort G1), where G1 = (remVerts S (remEdgesFromV S G))



; Ok so we can now identify what vertices are heads and make a topography graph for each of them.

; topSort := Returns V set in order of topography

;; Assume Acyclic!
;(define (topSort G)
;  (cond ((null? (getV G)) '())
;        (else (cons (findHeads G)
;                    (topSort (remVerts (findHeads G) (remEdgesFromV (findHeads G) G)))))))



; (dirEdge? v G) := #t if a dirEdge extends from v
; (noDirEdge? v G) := #t if no dirEdge exists extending from v.
; (oneDirEdge? v G) := #t if only one dirEdge exists extending from v.
; (getDirEdge v G) := Returns dirEdge pair extending from v.
; (remDirEdge v G) := Returns graph with first dirEdge pair extending from v removed.
; (getDirEdgeAdj v G) := Returns adjacent vertex from first dirEdge pair extending from v.


(define (dirEdge? v G)
  (inSet? v (firsts (getE G))))

(define (noDirEdge? v G)
  (not (inSet? v (firsts (getE G)))))


(define (oneDirEdge? v G)
  (and (dirEdge? v G) (noDirEdge? v (remDirEdge v G))))

(define (getDirEdge v G)
  (cond ((null? (getE G)) #f)
        ((equal? v (first (car (getE G)))) (car (getE G)))
        (else (getDirEdge (E-- G)))))

(define (remDirEdge v G)
  (cond ((null? (getE G)) G)
        ((equal? v (first (car (getE G)))) (E-- G))
        (else (E++ (car (getE G)) (remDirEdge v (E-- G))))))

(define (getDirEdgeAdj v G)
  (cond ((null? (getE G)) #f)
        ((equal? v (first (car (getE G)))) (second (car (getE G))))
        (else (getDirEdgeAdj v (E-- G)))))

;; (topSort '((1 2 3 4 5) ((4 3) (3 2) (2 1) (5 1))))

;; ^ I think I'm being too hopeful. I should probably have a function to chart every path from the head onwards.

;; v is at the head. 
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
;  (cond ((null? L1) L2)
;        ((equal? (car L1) (car L2)) (altUnion L1 L2))
;        ((inSet? (car L1) L2) (altUnionCom L1 (cdrUntilMatch (car L1) L2)))


; (getIndex v S) := given index v, return element in specified index of S.

(define (getIndex v S)
  (if (atom? v)
      (cond ((null? S) 0) ; So we get a value == cap if value not found
            ((equal? v (car S)) 0)
            (else (+ 1 (getIndex v (cdr S)))))
      #f)) ;return #f if invalid v

; (remIndex i S) := Removes element at index i in S.
(define (remIndex i S)
  (cond ((null? S) S)
        ((zero? i) (cdr S))
        (else (cons (car S) (remIndex (- i 1) (cdr S))))))



        
; I feel like there's a wayyyy easier way to do topography sort. It's in the name!
; Build from the ground up like levels.
; Use 'union' not 'altunion' for this one!
; ^Idea was to find tails and label them 'ground' but runs to same problem. I now know what's the issue!

; Basically just keep finding heads. Then for each head, make the tree with them as a root.
; Like rivers, they will point directions of constant decline.
; Heads are not connected in any form, so they can't possible connect to each other!
; However, we do not know their relative height to each other. It's a connected graph they all flow to puddles.
; Aka tails. Thus each river must flow to a tail, or share a tail with another vertex.
; That's why waiting for the recursion to finish helps. The last to be removed will be the most bottom tail.
;

; Ok, onePath?















; treeView := Vertices are sorted with '() grouping branch deviations from the main tree line/branch.

;(define (treeView G)
;  (cond ((null? (getV G)) '())
;        ((





; Documentation So Far:

; (bipartite? G) := #t if G is bipartite, or #f if G is bipartite.
;

; (getAllEdgeAdj v G) := Returns all vertices connected to v assumming 

; (union S1 S2) := appends elements of S1 into S2.
; (moreEdge? v G) := #t if vertex v has more than 1 undirect edge connected to it. #f if only 1 or none.
; (oneEdge? v G) := #t if vertex v has only 1 undirect edge.
; (nextEdge v G) := Returns the second undirect edge connected to v.
; (disjoint? S1 S2) := Returns #t if S1 and S2 are disjoint sets. #f otherwise.

; (consEnd a list) := Appends element a to the end of list.
; (altUnion L1 L2) := alternates unioning elements into 1 set.

; (treeify G v) := Given v as the root, return G s.t. G is a tree with v as the root.

; (firsts E) := Return set of vertices in G with dirEdges directing away from them.
; (seconds E) := Return set of vertices in G with dirEdges directing towards them.
; (findHeads G) := Returns set of vertices in G with no edges directing towards them. aka set of 'head' vertices in G.
; (findTails G) := Returns set of vertices in G with only edges directing towards them. aka set of 'tail' vertices in G.

; (remEdges E G) := Removes all Edges detailed in set E from graph G.
; (remVerts V G) := Removes all Vertices detailed in set V from graph G.
; (remEdgesFromV V G) := Input set of vertices V, and removes all edges directed away from each v (in) V.
; (getAssoEdges V G) := Input set of vertices V, and returs a set of all edges directing away from each v (in) V.

; (forvInSUn S G funct Un) := for all v listed in set S, perform (funct v G), and union the results using the 'Un' procedure.
; (topSort G) := Finds the topographic vertex sorting of graph G.
; (topSortIter v G) := Certain same as (treeify G v); Returns topographic from root v.
; (cdrUntilMatch v L2) := Removes first elements from L2 until the first element in L2 is the same as v.
; (findVCommon L1 L2) := Returns first v that are in both L1 and L2.
; (commonLabelIndexDiff v L1 L2) := Returns integer detailing how many more elements are before common v in L1 compared to L2.

; (popIter n L) := Returns lat of n first elements in lat L.
; (remIter n L) := Removes n elements from beginning of lat L.
; (altUnionCom L1 L2) := This performs an alternating union around the first common vertex in both L1 and L2. This preserves topography.
; (getIndex v S) := Returns index value of v in S for reference later.
; (remIndex i S) := Removes element at index i in S.

; (dirEdge? v G) := #t if a dirEdge extends from v
; (noDirEdge? v G) := #t if no dirEdge exists extending from v.
; (oneDirEdge? v G) := #t if only one dirEdge exists extending from v.
; (getDirEdge v G) := Returns dirEdge pair extending from v.
; (remDirEdge v G) := Returns graph with first dirEdge pair extending from v removed.
; (getDirEdgeAdj v G) := Returns adjacent vertex from first dirEdge pair extending from v.

; ---------------------------------------------------------------------------------------------------------------------


;; What changes needed for Weighted Graphs? Then do Minimum Spanning Tree!

;;;; ANSWER: Instead of (V E) -> G, (V E W) -> WG, where W is a lat directly correlating to repective edges' weight.

; WG ::= (V E W)
;; where W is constrained as follows:
;;;; a) elements are nonnegative numbers
;;;; b) Support up to Q (rational numbers) [a/b | a, b (in) Z]
;;;; c) W(i) (mapsto) E(i)

(define (getW G)
  (car (cdr (cdr G))))
(define (W-- G)
  (triple (getV G) (getE G) (cdr (getW G))))
(define (W++ w G)
  (triple (getV G) (getE G) (car w (getW G))))
(define (triple V E W)
  (cons V (cons E (cons W '()))))

(define (weMatch? G)
  ; number of elements in E == number of elements in W.
  (cond ((null? (getE G)) (null? (getW G)))
        ((null? (getW G)) (null? (getE G)))
        (else (weMatch? (E-- (W-- G))))))

; (getAssoWeight v G) := For vertex v, get the weight of the first dirEdge directing away from v.

(define (getAssoWeight v G)
  (getIndex (e->l2 (getDirEdge v G) G) (getW G)))

; (remAssoWeight v G) := Removes both the first dirEdge and its associated weight of vertex v in graph G.
; pre: (dirEdge? v G) -> #t.
(define (remAssoWeight v G)
  (remEdge (getDirEdge v G) (triple (getV G) (getE G) (remIndex (e->l2 (getDirEdge v G) G) (getW G)))))

; (getAllAssoWeights v G) := Returns a list of all the edgeWeights of dirEdges away from v.
(define (getAllAssoWeights v G)
  (cond ((noDirEdge? v G) '())
        (else (cons (getAssoWeight v G) (remDirEdge v G)))))

; returns the dirEdge with the lowest associated weight away from v.
(define (getLowestAssoWeight v G)
  (cond (noDirEdge?)))



; (getLowestAssoWeight := For vertex v, get the weight of the dirEdge directing away from v with the lowest weight. [MUST BE NONNEGATIVE]


; Want to implement Minimum-Spanning Tree for Weighted Graphs.
; My idea: Look at every node and delete all but the lowest weights to each node.
; Thus: Take the topologic sort of the Graph and the first node should be the head of the tree.
; Then find the shortest path between the head and every vertex in V of G.
; Compile the tree of all shortest paths and that's the MST.

; pre: WG is acyclic
(define (MST WG)
  (let ((acyclicWG (acyclify WG))
        (let ((root (car (topSort acyclicWG))))












;          ; If (vEdge? (car (getV G))) returns #f, then there's nowhere to go, and at the end of the graph with no loops found:
;          ((not (vEdge? v G)) (and #t (acyclic? (V-- G))))
;          ((oneEdge? v G) (and #t (acyclic? (V-- (remEdge (getEdge v G) G)))))
;          (else (let ((B1 (undirGraphify (remEdge (getEdge v G) G))) (B2 (undirGraphify (remEdge (nextEdge v G) G))))
;                  (and (not (connection? B1 B2)) (acyclic? (remEdge (getEdge v G) G))))))))
; ^need to fill in B1 and B2 at some point.

;; This returned #f <- (acyclic? '((1 2 3 4 5) ((4 1) (1 2) (2 3))))



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
;1;; path?(v1, v2) -> BOOL
;;;; shortPath(v1, v2) -> [INT or LIST DETAILING SHORTEST ROUTE aka breFirst]
;1;; acyclic?(G) -> BOOL
;1;; connected?(G) -> BOOL [tech EVDomain?]
;;;; spanningTree(G) -> TREE (requires defining TREE)
;;;; diameter(G) -> INT
;1;; bipartite?(G) -> BOOL
;;;; largeClique(G) -> CLIQUE (requires defining CLIQUE)
;1;; tree?(G)
;1;; adajacent(v1, v2) <- Doesn't seem to be helpful though

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

;(define (acyclic? G)
;  (let ((v (car (getV G))))
;    (cond ((null? (getV G)) #t)
;          ((null? (getE G)) #t)
;          ; If (vEdge? (car (getV G))) returns #f, then there's nowhere to go, and at the end of the graph with no loops found:
;          ((not (vEdge? v G)) (and #t (acyclic? (V-- G))))
;          ((oneEdge? v G) (and #t (acyclic? (V-- (remEdge (getEdge v G) G)))))
;          (else (let ((B1 (undirGraphify (remEdge (getEdge v G) G))) (B2 (undirGraphify (remEdge (nextEdge v G) G))))
;                  (and (not (connection? B1 B2)) (acyclic? (remEdge (getEdge v G) G))))))))
; ^need to fill in B1 and B2 at some point.

;; This returned #f <- (acyclic? '((1 2 3 4 5) ((4 1) (1 2) (2 3))))





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

;(define (connection? G1 G2)
;  (cond ((VShare? G1 G2) (let ((v (getVShare G1 G2))) (and (vEdge? v G1) (vEdge? v G2) (connected? G1) (connected? G2))))
;        (else #f)))

;; ^^ Just use path? instead.

; Thus, a singular tree is connected if all the listed vertices appear in at least a specified edge
;(define (connected? G)
;  (cond ((null? (getV G)) #t)
;        ((vEdge? (car (getV G)) G) (and #t (connected? (V-- G))))
;        (else #f)))

; Should I make a function that takes a disconnected graph and returns a set of vertices which are disconnected?


;;(let (
;  (path (cons vStart (depFirst (getEdgeAdj vStart G) vTar (remEdge (getEdge vStart G) (remVert vStart G)))))
;  (G1 (remEdge (getEdge vStart G) G))
;  )
;(cond ((lat? path) path)
;      ((vEdge? vstart G1) (depFirst vStart vTar G1))
;      (else '(())))))

; Using '(()) for termination resulting in #f because the proper result should be a lat, allowing lat? to test if result is true.



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

;; (vShare? G1 G2) := #t if G1 and G2 share a vertex.
;; (getVShare G1 G2) := Returns first v in G1 and G2 that is shared.


;; (undirGraphify G) := Given an undirGraph G, ensure G passes EVDomain?, vSet?, and eSet?
;; (remDisconnect G) := With V and E guaranteed to be sets, remove edges with reference to a v not in V, and remove v's not specified in an Edge.


 