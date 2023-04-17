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

(def edge-schema :tested
  (schema/predicate
   (and (pred indexed?)
        (length 2))))

(def weighted-edge-schema :tested
  (schema/predicate
   (and (pred indexed?)
        (length 3)
        (pred |(= (type (last $)) :number)))))

(def node-schema :tested
  (schema/predicate
   (and (pred indexed?)
        (or (pred edge-schema)
            (pred weighted-edge-schema)))))

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
  (g :metadata))

(defn defgraph :tested [&opt ingraph] 
  (when ingraph (assert (graph? ingraph) "optional argument to `defgraph` must be a valid graph"))
  (default ingraph @{})
  (merge (deep-clone (struct/to-table Graph)) ingraph))

(defn make-digraph! :tested [g]
  (put (g :metadata) :digraph true))

(defn make-weighted! :tested [g &opt weight]
  (default weight 0)
  (put (g :metadata) :weighted true)
  (each key [:adj :in] 
        (each edge (g key) 
              (each key (keys edge) 
                    (update edge key |(if (= (type $) :number) $ weight))))))

(update @[:a :b] 2 |(or $ "this"))

(defn node? :tested [node]
  (node-schema node))

(defn digraph? :tested [g]
  (and ((metadata g) :graph)
       ((metadata g) :digraph)
       (graph-schema g)))

(defn weighted? :tested [g]
  (and ((metadata g) :graph)
       ((metadata g) :weighted)
       (graph-schema g)))

(defn nodes :tested [g] 
  (g :nodeset))

(defn add-nodes :tested [g & nodes]
  (each node nodes
        (put (g :nodeset) node true))
  g)

(defn member-node? :tested [g node]
  (truthy? ((nodes g) node)))

(defn successors [g node]
  (keys (get-in g [:adj node])))

(defn out-edges 
  ``Returns a tuple of all edges that go out from the
  provided `node`. Node must be a member of the provided
  graph `g`.``
  [g node]
  (assert (graph? g) "First argument to `edges` must be a valid graph.") 
  (assert (node? node) "Second argument to `edges` must be a valid node.")
  (assert (member-node? g node) "Provided node is not a member of provided graph.")
  (seq [to-node :in (successors g node)]
       [node to-node]))

(defn edges 
  ``Iterates all nodes in a graph `g` and returns the
  full set of all edges from each node to each of that 
  node's successor nodes.``
  [g]
  (assert (graph? g) "Argument to `edges` must be a valid graph.")
  (seq [node :in (nodes g)]
       (seq [edge :in (out-edges g node)]
            edge)))

(defn add-edges [g & edges]
  (assert (graph? g) "First argument to `add-edges` must be a valid graph.")
  (each edge edges
        (if (weighted? g)
          (assert (weighted-edge-schema edge) "All edges passed to `add-edges` with a weighted graph must be valid weighted edges.")
          (assert (edge-schema edge) "All edges passed to `add-edges` must be valid edges.")))
  (reduce (fn [g [n1 n2 n3]]
            (let [weighted (weighted? g)
                  content (if weighted n3 true)]
              (when weighted (assert n3) "Provide a weight for edges in a weighted graph.")
              (add-nodes g n1 n2)
              (put-in g [:adj n1 n2] content)
              (if (digraph? g)
                (put-in g [:in n2 n1] content)      
                (put-in g [:adj n2 n1] content))))
          g edges))


# (defn add-edges [g & edges]
#   (each edge edges
#         (assert (edge-schema edge)))
#   (reduce (fn [g [n1 n2]]
#             (-> g 
#                 (add-nodes n1 n2)
#                 ()))))

(comment
  (defgraph "mygraph-2")


  (graph? mygraph-2)


  (metadata a-graph)
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