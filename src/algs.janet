(use judge)
(use ./graphs)

(defn remove :is-private [pred col]
  (case (type pred)
    :function (filter |(not (pred $)) col)
    :array (filter |(not (index-of $ pred)) col)
    :tuple (filter |(not (index-of $ pred)) col)
    (filter |(not (= pred $)) col)))

(defn iterate :is-private [fun init]
  (coro (var x init)
        (forever (yield x)
                 (set x (fun x)))))

(defn trace-path
  "Using a map of nodes-to-preds, traces a node's family tree back to the
  source. Cycles are not accounted for."
  [preds node]
  (take-while identity (iterate |(get $ node) preds)))

(defn paths
  "Returns a lazy seq of all non-looping path vectors starting with
  [<start-node>]"
  [preds path]
  (let [path (array/slice path)
        this-node (array/peek path)
        partish |(seq [i :range [0 (dec (length $))]] 
                  (array/slice $ i (+ 2 i)))] 
    @[path ;(->> (preds this-node) 
                 (filter |(all (fn [edge] (not= edge [this-node $]))  
                               (partish path))) 
                 (mapcat |(paths preds [;path $])))]))

(defn trace-paths
  "Given a function and a starting node, returns all possible paths
  back to source. Cycles are not accounted for."
  [preds start]
  (filter |(empty? (preds (array/peek $))) (paths preds [start])))

(defn preds->span
  "Converts a map of the form {node predecessor} to a spanning tree of the
  form {node [successors]}"
  [preds]
  (reduce
   (fn [span [n p]]
     (if p
       (put span p [;(or (span p) []) n])
       span)) 
   @{} (pairs preds)))

###
### Depth-first traversal
###

# (defn pre-traverse
#   "Traverses a graph depth-first preorder from start, successors being
#   a function that returns direct successors for the node. Returns a
#   lazy seq of nodes."
#   [successors start & seen] # {:keys [seen] :or {seen {}}}
#   (letfn [(step [stack seen]
#             (when-let [node (peek stack)]
#               (if (contains? seen node)
#                 (step (pop stack) seen)
#                 (let [seen (conj seen node)
#                       nbrs (remove seen (successors node))]
#                   (lazy-seq
#                     (cons node
#                           (step (into (pop stack) nbrs)
#                                 seen)))))))]
#     (step [start] seen)))