(use judge)
(import spork/schema)

(def graph-schema :tested
  (schema/predicate 
   (props :nodeset :array
          :adj     :table
          :in      :table
          :attrs   :table)))

(def Graph
  :graph
  @{:nodeset @[]
    :adj     @{}
    :in      @{}
    :attrs   @{}})

(defmacro metadata :tested [x]
  (assert (not (nil? x)) "graph was nil")
  ~(-> (table/clone (dyn ',x))
       (put :source-map nil)
       (put :value nil)))

(defmacro defgraph :tested [s]
  (assert (index-of (type s) [:string :buffer])
          "defgraph requires a string")
  ~(do (def ,(symbol s) :graph 
         (table/setproto @{} Graph)) 
       (setdyn ',(symbol s) (table/setproto 
                             @{:value ,(symbol s)
                               :graph true}
                             Graph))))

(defmacro make-digraph! :tested [g]
  ~(put (dyn ',g) :digraph true))

(defmacro make-weighted! :tested [g]
  ~(put (dyn ',g) :weighted true))

(defmacro graph? :tested [g]
  ~(and ((metadata ,g) :graph)
        (graph-schema ((dyn ',g) :value))))

(defmacro digraph? :tested [g]
  ~(and ((metadata ,g) :graph)
        ((metadata ,g) :digraph)
        (graph-schema (table/proto-flatten ((dyn ',g) :value)))))

(defmacro weighted? :tested [g]
  ~(and ((metadata ,g) :graph)
        ((metadata ,g) :weighted)
        (graph-schema (table/proto-flatten ((dyn ',g) :value)))))

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