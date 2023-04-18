(use judge)
(import spork/schema)
(use /src/utils)

(def graph-schema :tested
  (schema/predicate 
   (props :metadata :table
          :nodeset  :table
          :adj      :table
          :in       :table
          :attrs    :table)))

(def unweighted-edge-schema :tested
  (schema/predicate
   (and (pred indexed?)
        (length 2))))

(def weighted-edge-schema :tested
  (schema/predicate
   (and (pred indexed?)
        (length 3)
        (pred |(= (type (last $)) :number)))))

(def edge-schema :tested
  (schema/predicate
   (or (pred unweighted-edge-schema)
       (pred weighted-edge-schema))))

(def node-schema :tested
  (schema/predicate
   (pred truthy?)))

(def Graph 
  {:metadata @{:graph true}
   :nodeset  @{}
   :adj      @{}
   :in       @{}
   :attrs    @{}})

(defn graph? :tested [g]
  (and (graph-schema g)
       ((g :metadata) :graph)))

(defn metadata :tested [g]
  (if-let [metadata (g :metadata)]
    metadata @{}))

(defn defgraph :tested [&opt ingraph] 
  (when ingraph (assert (graph? ingraph) "optional argument to `defgraph` must be a valid graph"))
  (default ingraph @{})
  (merge (deep-clone (struct/to-table Graph)) ingraph))

(defn make-digraph! :tested [g]
  (put (g :metadata) :digraph true) g)

(defn make-weighted! :tested [g &opt weight]
  (default weight 1)
  (put (g :metadata) :weighted true)
  (each key [:adj :in] 
    (each edge (g key) 
      (each key (keys edge) 
        (update edge key 
                |(if (= (type $) :number) $ weight)))))
  g)

(def node? :tested
  node-schema)

(defn digraph? :tested [g]
  (and (dictionary? g)
       ((metadata g) :graph)
       ((metadata g) :digraph)
       (graph-schema g)))

(defn weighted? :tested [g]
  (and ((metadata g) :graph)
       ((metadata g) :weighted)
       (graph-schema g)))

(defn nodes :tested [g] 
  (keys (g :nodeset)))

(defn add-nodes :tested [g & nodes]
  (each node nodes 
    (put (g :nodeset) node true))
  g)

(defn has-node? :tested [g node] 
  (truthy? (index-of node (nodes g))))

(defn has-edge? :tested [g [n1 n2 n3]]
  (truthy? (index-of n2 (keys (get-in g [:adj n1] @{})))))

(defn successors :tested [g node]
  (keys (get-in g [:adj node] @{})))

(defn predecessors :tested [g node]
  (assert (digraph? g) "Input graph `g` must be a digraph.")
  (keys (get-in g [:in node] @{})))

(defn remove-adj-nodes :tested [g nodes neighbors] 
  (each neighbor neighbors
    (each node nodes
      (put-in (g :adj) [neighbor node] nil)))
  g)

(defn remove-nodes :tested [g & nodes]
  (assert (graph? g) "First argument to `remove-nodes` must be a valid graph.") 
  (let [nodes-flat (flatten nodes)
        nbrs (distinct (filter |(not (index-of $ nodes-flat)) 
                     (mapcat |(successors g $) nodes-flat)))]
    (each n nodes-flat (put (g :nodeset) n nil))
    (each n nodes-flat (put (g :adj) n nil))
    (remove-adj-nodes g nodes-flat nbrs))
  g)

(defn out-edges :tested
  ``Returns a tuple of all edges that go out from the
  provided `node`. Node must be a member of the provided
  graph `g`.``
  [g node]
  (assert (graph? g) "First argument to `edges` must be a valid graph.") 
  (assert (has-node? g node) "Provided node is not a member of provided graph.")
  (seq [to-node :in (successors g node)]
       [node to-node]))

(defn in-edges :tested [g node]
  (seq [n2 :in (predecessors g node)]
    [n2 node]))

(def out-degree :tested
  (comp length out-edges))

(defn in-degree :tested [g node]
  (assert (digraph? g) "Input graph `g` must be a valid digraph.")
  (length (get-in g [:in node] @{})))

(defn edges :tested
  ``Iterates all nodes in a graph `g `and returns the
  full set of all edges from each node to each of that
  node's successor nodes. ``
  [g]
  (assert (graph? g) "Argument to `edges` must be a valid graph.")
  (seq [node :in (nodes g)] 
    ;(seq [edge :in (out-edges g node)] 
       edge)))

(defn update-edge :tested [g [n1 n2 n3] kind]
  (let [weighted (weighted? g)
        content (case kind
                  :add (if weighted n3 true)
                  :remove nil)]
    (when (and weighted (= kind :add)) 
      (assert n3) "You must provide a weight for edges in a weighted graph.")
    (add-nodes g n1 n2)
    (put-in g [:adj n1 n2] content)
    (if (digraph? g)
      (put-in g [:in n2 n1] content)
      (put-in g [:adj n2 n1] content))))

(defn add-edges :tested [g & edges] 
  (assert (graph? g) "First argument to `add-edges` must be a valid graph.")
  (each edge edges
    (if (weighted? g)
      (assert (weighted-edge-schema edge) "All edges passed to `add-edges` with a weighted graph must be valid weighted edges.")
      (assert (edge-schema edge) "All edges passed to `add-edges` must be valid edges."))) 
  (reduce |(update-edge $0 $1 :add) g edges)
  g)

(defn remove-edges :tested [g & edges]
  (assert (graph? g) "First argument to `remove-edges` must be a valid graph.") 
  (reduce |(update-edge $0 $1 :remove) g edges)
  g) 

(defn remove-all-edges :tested [g]
  (remove-edges g ;(edges g)) g)

(defn remove-all-nodes :tested [g]
  (set (g :nodeset) @{})
  (set (g :adj) @{})
  (set (g :attrs) @{})
  (when (digraph? g)
    (set (g :in) @{}))
  g)

(defn src    [edge] (get edge 0))
(defn dest   [edge] (get edge 1))
(defn weight [edge] (get edge 2))

(defn get-weight :tested [g [n1 n2]] 
  (get-in g [:adj n1 n2] nil))

(defn transpose :tested [g]
  (assert (digraph? g) "First argument to `transpose` must be a valid digraph.")
  (let [temp-g (-> (deep-clone g)
                   (put :adj @{})
                   (put :in @{}))]
    (reduce (fn [tg [n1 n2]]
              (add-edges tg (if (weighted? tg) 
                              [n2 n1 (get-weight g [n1 n2])]
                              [n2 n1])))
            temp-g
            (edges g))))

(defn subgraph :tested
  "Returns a graph with only the given nodes"
  [g ns]
  (let [new-g (deep-clone g)] 
    (remove-nodes new-g (filter |(not (index-of $ ns)) (nodes g)))))

(defn add-weights :tested [edges &opt weight]
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

(defn build-graph :tested [g & inits] 
  (defn build [g init] 
    (cond
      # graph
      (graph? init) (if (and (weighted? g) (weighted? init)) 
                      (put (add-edges  
                            (add-nodes g ;(nodes init)) 
                            ;(seq [[n1 n2] :in (edges init)]  
                               [n1 n2 (get-weight init [n1 n2])])) 
                           :attrs (merge (g :attrs) (init :attrs))) 
                      (let [build-edges (if (weighted? g) 
                                          (seq [[n1 n2] :in (edges init)] 
                                            [n1 n2 1]) 
                                          (edges init))] 
                        (add-nodes g ;(nodes init)) 
                        (add-edges g ;build-edges) 
                        (put g :attrs (merge (g :attrs) (init :attrs)))))
      # adjacency map
      (dictionary? init) (let [es (if (number? (get-in (map values (values init))
                                                       [0 0] nil))
                                    ;(seq [[n nbrs] :in (pairs init)]
                                      ;(seq [[nbr wt] :in (pairs nbrs)]
                                        [n nbr wt]))
                                    ;(seq [[n nbrs] :in (pairs init)]
                                      ;(seq [[nbr bool] :in (pairs nbrs)]
                                         [n nbr])))] 
                           (add-nodes g ;(keys init))
                           (add-edges g ;es))
      # edge
      (indexed? init) (add-edges g init)
      # node
      (add-nodes g init)))

  (reduce build g inits))

(defn graph [& inits]
  (build-graph (defgraph) ;inits))

(defn digraph [& inits]
  (build-graph (make-digraph! (defgraph)) ;inits))

(defn weighted-graph [& inits] 
  (build-graph (make-weighted! (defgraph)) ;inits))

(defn weighted-digraph [& inits]
  (build-graph (make-digraph! (make-weighted! (defgraph))) ;inits))