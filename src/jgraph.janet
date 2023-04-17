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
  (put (g :metadata) :digraph true))

(defn make-weighted! :tested [g &opt weight]
  (default weight 1)
  (put (g :metadata) :weighted true)
  (each key [:adj :in] 
    (each edge (g key) 
      (each key (keys edge) 
        (update edge key 
                |(if (= (type $) :number) $ weight))))))

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

# (let [nbrs (mapcat #(successors g %) nodes)]
#        (-> g
#            (update-in [:nodeset] #(apply disj % nodes))
#            (assoc :adj (remove-adj-nodes (:adj g) nodes nbrs disj))))

(defn has-node? :tested [g node] 
  (truthy? (index-of node (nodes g))))

(defn has-edge? :tested [g [n1 n2 n3]]
  (truthy? (index-of n2 (keys (get-in g [:adj n1] @{})))))

(defn successors :tested [g node]
  (keys (get-in g [:adj node] @{})))

(defn predecessors [g node]
  (assert (digraph? g) "Input graph `g` must be a digraph.")
  (keys (get-in g [:in node] @{})))

(defn remove-adj-nodes :tested [g nodes neighbors] 
  (each neighbor neighbors
    (each node nodes
      (put-in (g :adj) [neighbor node] nil)))
  g)

(defn remove-nodes :tested [g & nodes]
  (assert (graph? g) "Input graph `g` must be a valid graph.") 
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

(defn add-edges :tested [g & edges]
  (assert (graph? g) "First argument to `add-edges` must be a valid graph.")
  (each edge edges
    (if (weighted? g)
      (assert (weighted-edge-schema edge) "All edges passed to `add-edges` with a weighted graph must be valid weighted edges.")
      (assert (edge-schema edge) "All edges passed to `add-edges` must be valid edges.")))
  (reduce (fn [g [n1 n2 n3]]
            (let [weighted (weighted? g)
                  content (if weighted n3 true)]
              (when weighted (assert n3) "You must provide a weight for edges in a weighted graph.")
              (add-nodes g n1 n2)
              (put-in g [:adj n1 n2] content)
              (if (digraph? g)
                (put-in g [:in n2 n1] content)      
                (put-in g [:adj n2 n1] content))))
          g edges))

(defn remove-edges [])

(defn remove-all [])

(defn transpose []
  # (reduce (fn [tg [n1 n2]]
  #           (add-edges* tg [[n2 n1 (weight g n1 n2)]]))
  #         (assoc g :adj {} :in {})
  #         (edges g))
  )

(defn weight [])

(defn src [edge])

(defn dest [edge])

(defn subgraph
  "Returns a graph with only the given nodes"
  [g ns]
  # (remove-nodes* g (remove (set ns) (nodes g)))
  )

(defn add-path
  "Adds a path of edges connecting the given nodes in order"
  [g & nodes]
  # (add-edges* g (partition 2 1 nodes))
  )

(defn add-cycle
  "Adds a cycle of edges connecting the given nodes in order"
  [g & nodes]
  # (add-edges* g (partition 2 1 (concat nodes [(first nodes)])))
  )

# (defn build-graph [g & inits]
#   (assert (= (type name) :string)) 

#   (defn build [g init]
#     (cond
#       # graph
#       (graph? init) (if (and (weighted? g) (weighted? init))
#                       (put (reduce add-edges
#                                    (add-nodes g (nodes init))
#                                    (seq [[n1 n2] :in (edges init)]
#                                         [n1 n2 (weight init [n1 n2])]))
#                            :attrs (merge (:attrs g) (:attrs init)))
#                       (-> g
#                           (add-nodes (nodes init))
#                           (add-edges (edges init))
#                           (put :attrs (merge (:attrs g) (:attrs init)))))
#       # adjacency map
#       (dictionary? init) (let [es (if (dictionary? (get init 0))
#                                     (seq [[n nbrs] :in init]
#                                          (seq [[nbr wt] :in nbrs]
#                                               [n nbr wt]))
#                                     (seq [[n nbrs] :in init]
#                                          (seq [nbr :in nbrs]
#                                               [n nbr])))]
#                            (-> g
#                                (add-nodes (keys init))
#                                (add-edges es)))
#       # edge
#       (indexed? init) (add-edges g init)
#       # node
#       (add-nodes g init)))

#   (reduce build g inits))