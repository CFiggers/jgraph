(use judge)
(import spork/schema)

(def graph-schema :tested :is-private
  (schema/predicate
    (props :metadata :table
           :nodeset :table
           :adj :table
           :in :table
           :attrs :table)))

(def unweighted-edge-schema :tested :is-private
  (schema/predicate
    (and (pred indexed?)
         (length 0 3))))

(def attr-edge-schema :is-private
  (schema/predicate
    (and (pred indexed?)
         (length 0 3))))

(def weighted-edge-schema :tested :is-private
  (schema/predicate
    (and (pred indexed?)
         (or (length 0)
             (and (length 3)
                  (pred |(= (type (last $)) :number)))))))

(def edge-schema :tested :is-private
  (schema/predicate
    (or (pred unweighted-edge-schema)
        (pred weighted-edge-schema)
        (pred attr-edge-schema))))

(def node-schema :tested :is-private
  (schema/predicate
    (pred truthy?)))

(def attr-node-schema :tested :is-private
  (schema/predicate
    (and (pred indexed?)
         (length 2)
         (pred |(dictionary? (last $))))))

(def Graph :is-private
  {:metadata @{:graph true}
   :nodeset @{}
   :adj @{}
   :in @{}
   :attrs @{}})

(defn graph? :tested
  ``Check if `g` is a graph.``
  [g]
  (and (graph-schema g)
       ((g :metadata) :graph)))

(defn metadata :tested
  ``Returns the `:metadata` of `g`. Equivalent to 
  (g :metadata), except returns an empty table
  instead of `nil` if `g` has no `:metadata` key.``
  [g]
  (if-let [metadata (g :metadata)]
    metadata @{}))

(def deep-clone :tested
  ``Fully copies an object or data structure by marshalling
  it into memory and then unmarshalling it into a new data structure.
  Useful for deeply copying tables, like graphs.``
  (comp unmarshal marshal))

(defn defgraph :tested :is-private [&opt ingraph]
  (when ingraph (assert (graph? ingraph) (string/format "optional argument to `defgraph` must be a valid graph Got: %q" ingraph)))
  (default ingraph @{})
  (merge (deep-clone (struct/to-table Graph)) ingraph))

(defn node? :tested
  ``Check if `n` is a valid node.``
  [n]
  (node-schema n))

(defn digraph? :tested
  ``Check if `g` is a digraph.``
  [g]
  (and (dictionary? g)
       ((metadata g) :graph)
       ((metadata g) :digraph)
       (graph-schema g)))

(defn weighted? :tested
  ``Check if `g` is a weighted graph.``
  [g]
  (and ((metadata g) :graph)
       ((metadata g) :weighted)
       (graph-schema g)))

(defn attr?
  ``Check if `g` is an attr graph.``
  [g]
  (and ((metadata g) :graph)
       ((metadata g) :attr)
       (graph-schema g)))

(defn nodes :tested
  ``Get the nodes of a valid graph. Equivalent to calling
  (keys (`g` :nodeset)).``
  [g]
  (keys (g :nodeset)))

(defn add-nodes :tested
  ``Given a valid graph, `g`, and any number of values, add 
  an arbitrary number of nodes to `g`.``
  [g & nodes]
  (each node nodes
    (put (g :nodeset) node true))
  g)

(defn has-node? :tested
  ``Given a valid graph, `g`, and any value, `node`, check 
  whether `node` is in the `:nodeset` of `g`.``
  [g node]
  (truthy? (index-of node (nodes g))))

(defn has-edge? :tested
  ``Given a valid graph, `g`, and a valid edge, returns 
  `true` or `false` indicating whether `g` has that edge.``
  [g [n1 n2 n3]]
  (truthy? (index-of n2 (keys (get-in g [:adj n1] @{})))))

(defn successors :tested
  ``Given a valid graph, `g`, and a node, `node`, returns an array
  of nodes pointed to by all edges leaving `node`.``
  [g node]
  (keys (get-in g [:adj node] @{})))

(defn predecessors :tested
  ``Given a valid digraph, `dg`, and a node, `node`, returns an array
  of nodes possessing edges that point to `node`. Errors if `dg` is 
  not a digraph.``
  [dg node]
  (assert (digraph? dg) (string/format "Input graph `dg` must be a digraph. Got: %q" dg))
  (keys (get-in dg [:in node] @{})))

(defn remove-nodes-prim :tested :is-private [g nodes neighbors]
  (each neighbor neighbors
    (each node nodes
      (put-in (g :adj) [neighbor node] nil)
      (put-in (g :in) [neighbor node] nil)))
  (each [node val] (pairs (g :in))
    (when (deep= val @{})
      (put (g :in) node nil)))
  g)

(defn remove-nodes :tested
  ``Given a valid graph, `g`, and any number of values, removes
  those values from `g`'s :nodeset and eliminates all edges that include
  any of those values.``
  [g & nodes]
  (assert (graph? g) (string/format "First argument to `remove-nodes` must be a valid graph. Got: %q" g))
  (let [nodes-flat (flatten nodes)
        nbrs (distinct (filter |(not (index-of $ nodes-flat))
                               (array/concat (mapcat |(successors g $) nodes-flat)
                                             (if (digraph? g)
                                               (mapcat |(predecessors g $) nodes-flat) @[]))))]
    (each n nodes-flat (put (g :nodeset) n nil))
    (each n nodes-flat (put (g :adj) n nil))
    (each n nodes-flat (put (g :in) n nil))
    (each n nodes-flat (put (g :attrs) n nil))
    (remove-nodes-prim g nodes-flat nbrs))
  g)

(defmacro remove-nodes-succ
  ``Given a valid graph, `g`, and a node, `node`, removes all nodes
  from the graph that are successors to the provided node.``
  [g node]
  (with-syms [$g $node]
    ~(let [,$g ,g ,$node ,node]
       (,remove-nodes ,$g (,successors ,$g ,$node)))))

(defmacro remove-nodes-pred
  ``Given a valid digraph, `dg`, and a node, `node`, removes all nodes
  from the graph that are predecessors to the provided node. Errors if
  `g` is not a digraph.``
  [dg node]
  (with-syms [$dg $node]
    ~(let [,$dg ,dg ,$node ,node]
       (,remove-nodes ,$dg (,predecessors ,$dg ,$node)))))

(defmacro remove-nodes-adj
  ``Given a valid graph, `g`, and a node, `node`, removes all nodes
  from the graph that have any adjacency to that node.``
  [g node]
  (with-syms [$g $node]
    ~(let [,$g ,g ,$node ,node]
       (,remove-nodes-pred ,$g ,$node)
       (,remove-nodes-succ ,$g ,$node))))

(defmacro out-edges :tested
  ``Returns a tuple of all edges that go out from the provided `node`. 
  Errors if `g` is not a graph or if `node` is not a node in `g`.``
  [g node]
  (with-syms [$g $node]
    ~(let [,$g ,g ,$node ,node]
       (assert (,graph? ,$g) (string/format "First argument to `out-edges` must be a valid graph. Got: %q" ,$g))
       (assert (,has-node? ,$g ,$node) "Provided node is not a member of provided graph.")
       (seq [to-node :in (,successors ,$g ,$node)]
         [,$node to-node]))))

(defmacro in-edges :tested
  ``Given a valid graph, `g`, and any value, `node`, returns an array
  of edges in `g` that point to `node`.``
  [g node]
  (with-syms [$g $node]
    ~(let [,$g ,g ,$node ,node]
       (assert (,graph? ,$g) (string/format "First argument to `in-edges` must be a valid graph. Got: %q" ,$g))
       (assert (,has-node? ,$g ,$node) "Provided node is not a member of provided graph.")
       (seq [n2 :in (,predecessors ,$g ,$node)]
         [n2 ,$node]))))

(defmacro out-degree :tested
  ``Given a valid graph, `g`, and any value, `node`, returns an integer
  indicating the number of edges in `g` that point to `node`. Equivalent to
  `(length (out-edges g node))`.``
  [g node]
  (with-syms [$g $node]
    ~(let [,$g ,g ,$node ,node]
       (length (eval (,out-edges ,$g ,$node))))))

(defmacro in-degree :tested
  ``Takes a digraph, `dg` and a node, `node` and returns an integer
  representing the number of directional edges pointing "in" to that node.
  Errors if `dg` is not a digraph. `node` must be a member of the provided
  graph `dg`.``
  [dg node]
  (with-syms [$dg $node]
    ~(let [,$dg ,dg ,$node ,node]
       (assert (,digraph? ,$dg) (string/format "Input graph `dg` must be a valid digraph. Got: %q" ,$dg))
       (length (get-in ,$dg [:in ,$node] @{})))))

(defn edges :tested
  ``Iterates all nodes in a graph `g` and returns the
  full set of all edges from each node to each of that
  node's successor nodes. ``
  [g]
  (assert (graph? g) (string/format "Argument to `edges` must be a valid graph. Got: %q" g))
  (seq [node :in (nodes g)
        edge :in (out-edges g node)]
    edge))

(defn update-edge :tested :is-private [g [n1 n2 w] kind]
  (let [weighted (weighted? g)
        old-content (get-in g [:adj n1 n2])
        content (cond
                  (= kind :remove) nil
                  weighted w
                  (array? old-content) @[;old-content w]
                  (and w (not= w true)) @[w]
                  true)]
    (when (and weighted (= kind :add))
      (assert w "You must provide a weight for edges in a weighted graph."))
    (add-nodes g n1 n2)
    (put-in g [:adj n1 n2] content)
    (if (digraph? g)
      (put-in g [:in n2 n1] content)
      (put-in g [:adj n2 n1] content))))

(defn add-edges :tested
  ``Given a valid graph, `g`, and any number of edges, add all edges
  to `g`, including creating new nodes if not already in `g`.``
  [g & edges]
  (assert (graph? g) (string/format "First argument to `add-edges` must be a valid graph. Got: %q" g))
  (each edge edges
    (if (weighted? g)
      (assert (weighted-edge-schema edge) (string/format "All edges passed to `add-edges` with a weighted graph must be valid weighted edges. Got: %q" edge))
      (assert (edge-schema edge) (string/format "All edges passed to `add-edges` must be valid edges. Got: %q" edge))))
  (reduce |(update-edge $0 $1 :add) g edges)
  g)

(defn remove-edges :tested
  ``Given a valid graph, `g`, and any number of edges, remove all edges
  from `g`.``
  [g & edges]
  (assert (graph? g) (string/format "First argument to `remove-edges` must be a valid graph. Got: %q" g))
  (reduce |(update-edge $0 $1 :remove) g edges)
  g)

(defn remove-edges-all :tested
  ``Remove all edges from graph `g`.``
  [g]
  (remove-edges g ;(edges g)) g)

(defn remove-nodes-all :tested
  ``Remove all nodes from graph `g`.``
  [g]
  (set (g :nodeset) @{})
  (set (g :adj) @{})
  (set (g :attrs) @{})
  (when (digraph? g)
    (set (g :in) @{}))
  g)

(defn src :is-private [edge] (get edge 0))
(defn dest :is-private [edge] (get edge 1))
(defn weight :is-private [edge] (get edge 2))

(defn get-weight :tested
  ``Given a valid graph, `g`, and a tuple of at least two elements,
  checks for a weight on the edge in `g` between the two elements.
  Will return `true` if `g` is not a weighted graph and `nil` if there
  is no edge between `n1` and `n2`.``
  [g [n1 n2]]
  (get-in g [:adj n1 n2] nil))

(defn transpose-digraph :tested
  ``Takes a digraph and inverts the direction of all edges. 
  Will error if provided graph is not a digraph.``
  [dg]
  (assert (digraph? dg) (string/format "First argument to `transpose-digraph` must be a valid digraph. Got: %q" dg))
  (let [temp-g (-> (deep-clone dg)
                   (put :adj @{})
                   (put :in @{}))]
    (reduce (fn [tg [n1 n2]]
              (add-edges tg (if (weighted? tg)
                              [n2 n1 (get-weight dg [n1 n2])]
                              [n2 n1])))
            temp-g
            (edges dg))))

(defn subgraph :tested
  "Returns a graph with only the given nodes"
  [g ns]
  (let [new-g (deep-clone g)]
    (remove-nodes new-g (filter |(not (index-of $ ns)) (nodes g)))))

(defn add-weights :tested
  ``Given an indexed ds of edges and any value, `weight`, adds `weight` 
  to each edge.``
  [edges &opt weight]
  (seq [edge :in edges]
    (array/push edge (or weight 1))))

(defn add-path :tested
  "Adds a path of edges connecting the given nodes in order"
  [g nodes]
  (let [pnodes (seq [i :range [0 (dec (length nodes))]]
                 (array/slice nodes i (+ 2 i)))
        edges (if (weighted? g)
                (add-weights pnodes)
                pnodes)]
    (add-edges g ;pnodes)))

(defn add-cycle :tested
  "Adds a cycle of edges connecting the given nodes in order"
  [g nodes]
  (add-path g (array/push (array/slice nodes) (first nodes))))

(defn make-digraph! :tested :is-private [g]
  (let [dflt (if (weighted? g) 1 true)
        edg (map |(array/push (array/slice $) dflt) (edges g))]
    (put (g :metadata) :digraph true)
    (add-edges g ;edg)
    g))

(defn make-weighted! :tested :is-private [g &opt weight]
  (default weight 1)
  (put (g :metadata) :weighted true)
  (each key [:adj :in]
    (each edge (g key)
      (each key (keys edge)
        (update edge key
                |(if (= (type $) :number) $ weight)))))
  g)

(defn build-graph :tested :is-private [g & inits]
  (defn build [g init]
    (cond
      # graph
      (graph? init) (do (add-nodes g ;(nodes init))
                      (add-edges g ;(map |(array/push (array/slice $) (if (weighted? g) 1 true)) (edges init)))
                      (put g :attrs (merge (g :attrs) (init :attrs))))
      # adjacency map
      (dictionary? init) (let [init-s (seq [[fst snd] :in (pairs init)]
                                        (if (indexed? snd)
                                          [fst (from-pairs (map |[$ true] snd))] [fst snd]))]
                           (add-nodes g ;(keys init))
                           (add-edges g ;(seq [[node neighbors] :in init-s
                                               [n1 n2] :in (pairs neighbors)]
                                           [node n1 n2])))
      # edge
      (indexed? init) (add-edges g init)
      # node
      (add-nodes g init)))

  (reduce build g inits))

(defn graph :tested
  ``Initialize a graph using any combination of nodes, edges, 
  adjacency maps, or existing graphs.``
  [& inits]
  (build-graph (defgraph) ;inits))

(defn digraph :tested
  ``Initialize a digraph using any combination of nodes, edges, 
  adjacency maps, or existing graphs.``
  [& inits]
  (build-graph (make-digraph! (defgraph)) ;inits))

(defn weighted-graph :tested
  ``Initialize a weighted graph using any combination of nodes, edges, 
  adjacency maps, or existing graphs.``
  [& inits]
  (build-graph (make-weighted! (defgraph)) ;inits))

(defn weighted-digraph :tested
  ``Initialize a weighted digraph using any combination of nodes, edges, 
  adjacency maps, or existing graphs.``
  [& inits]
  (build-graph (make-weighted! (make-digraph! (defgraph))) ;inits))
