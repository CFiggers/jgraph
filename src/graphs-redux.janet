(use judge)
(use ./dataclasses)
(use ./deep-clone)
(use ./traverse)

# Nodes

(dataclass Node # :tested
           :val :array
           {:get |(($ :val) 0)
            :v |(($ :val) 0)
            :set (fn [self v]
                   (put self :val @[])
                   (put-in self [:val 0] v))})

# Edges

#    Edge─────────────►Attr-Edge    
#      │                   │
#      ▼                   ▼
#  Weighted-Edge──►Weighted-Attr-Edge 
#       

(dataclass Edge
           :from Node
           :to Node
           {:f |($ :from)
            :t |($ :to)})

(dataclass Weighted-Edge
           :from Node
           :to Node
           :weight [:number 1]
           {:f |($ :from)
            :t |($ :to)
            :w |($ :weight)})
(table/setproto Weighted-Edge Edge)

(dataclass Attr-Edge
           :from Node
           :to Node
           :attrs :table
           {:f |($ :from)
            :t |($ :to)
            :as |($ :attrs)})
(table/setproto Attr-Edge Edge)

(dataclass Weighted-Attr-Edge
           :from Node
           :to Node
           :weight [:number 1]
           :attrs :table
           {:f |($ :from)
            :t |($ :to)
            :as |($ :attrs)})
(table/setproto Weighted-Attr-Edge Weighted-Edge)

# Graphs

#    Graph─────────────────►Weighted-Graph
#      │ ╲                   │ ╲
#      │  ╲                  │  ╲
#      │   Attr-Graph─────────►Weighted-Attr-Graph
#      │    │                │   │               
#      ▼    │                ▼   │
# Digraph───│──►Weighted-Digraph │
#       ╲   │                 ╲  │
#        ╲  ▼                  ╲ ▼
#        Attr-Digraph────────►Weighted-Attr-Digraph
#

(dataclass Graph
           :metadata [:struct {:graph true}]
           :nodeset :table:k:Node:v:boolean
           :adj :table)

(test (graph :nodeset @{(node :val @[:a]) true}) @{:nodeset @{@{:val :a} true}})
(test (graph? (graph :nodeset @{(node :val @[:a]) true})) false)
(test (graph? (graph :nodeset @{@{:val @[:a]} true})))
(test-macro (assert-graph (graph :nodeset @{@{:val @[:a]} true})))

(dataclass Digraph
           :metadata [:struct {:digraph true :graph true}]
           :nodeset :table:k:Node:v:boolean
           :adj :table
           :in :table)
(table/setproto Digraph Graph)

(dataclass Attr-Graph
           :metadata [:struct {:attr true :graph true}]
           :nodeset :table:k:Node:v:boolean
           :adj :table
           :attrs :table)
(table/setproto Attr-Graph Graph)

(dataclass Weighted-Graph
           :metadata [:struct {:weighted true :graph true}]
           :nodeset :table:k:Node:v:boolean
           :adj :table)
(table/setproto Weighted-Graph Graph)

(dataclass Attr-Digraph
           :metadata [:struct {:attr true :digraph true :graph true}]
           :nodeset :table:k:Node:v:boolean
           :adj :table
           :attrs :table)
(table/setproto Attr-Digraph Digraph)

(dataclass Weighted-Digraph
           :metadata [:struct {:weighted true :digraph true :graph true}]
           :nodeset :table:k:Node:v:boolean
           :adj :table)
(table/setproto Weighted-Digraph Digraph)

(dataclass Weighted-Attr-Graph
           :metadata [:struct {:weighted true :attr true :graph true}]
           :nodeset :table:k:Node:v:boolean
           :adj :table
           :attrs :table)
(table/setproto Weighted-Attr-Graph Attr-Graph)

(dataclass Weighted-Attr-Digraph
           :metadata [:struct {:weighted true :attr true :digraph true :graph true}]
           :nodeset :table:k:Node:v:boolean
           :adj :table
           :attrs :table)
(table/setproto Weighted-Attr-Digraph Attr-Digraph)

## Add Graph/Edge Archetypes 

(defn add-digraph
  ``
  Add Digraph features to an existing graph, `g`. If `g` is already a digraph, returns `g` unmodified. 

  Existing edges are treated as bi-directional pairs of directional edges between two points. In other words, each edge is
    a. Converted to a one-directional edge 
    b. Matched with a duplicate pointing in the other direction.
  ``
  [g]
  (assert-graph g)
  (put g :in (g :adj))
  (cond
    (digraph? g) g
    (weighted-attr-graph? g)
    (do (put g :metadata {:digraph true :attr true :weighted true :graph true})
      (assert-digraph (table/setproto g Weighted-Attr-Digraph)))

    (attr-graph? g)
    (do (put g :metadata {:digraph true :attr true :graph true})
      (assert-digraph (table/setproto g Weighted-Attr-Graph)))

    (weighted-graph? g)
    (do (put g :metadata {:digraph true :weighted true :graph true})
      (assert-digraph (table/setproto g Weighted-Digraph)))

    (do (put g :metadata {:digraph true :graph true})
      (assert-digraph (table/setproto g Digraph)))))

(defn add-attr-to-edge
  ``
  Add Attr Edge features to an existing edge, `e`. If `e` is already an Attr Edge, returns `e` unmodified.
  ``
  [e]
  (assert-edge e)
  (put e :attrs @{})
  (cond
    (attr-edge? e) e

    (weighted-edge? e)
    (assert-attr-edge (table/setproto e Weighted-Attr-Edge))

    (assert-attr-edge (table/setproto e Weighted-Edge))))

(defn add-attr
  ``
  Add Attr Graph features to an existing graph, `g`. If `g` is already an attr graph, returns `g` unmodified. 

  Existing edges will have Attr Edge features added, if they did not have them already.
  ``
  [g]
  (assert-graph g)
  (put g :attrs @{})
  (each e (keys (g :adj)) (add-attr-to-edge e))
  (cond
    (attr-graph? g) g
    (weighted-digraph? g)
    (do (put g :metadata {:attr true :digraph true :weighted true :graph true})
      (assert-attr-graph (table/setproto g Weighted-Attr-Digraph)))

    (digraph? g)
    (do (put g :metadata {:attr true :digraph true :graph true})
      (assert-attr-graph (table/setproto g Attr-Digraph)))

    (weighted-graph? g)
    (do (put g :metadata {:attr true :weighted true :graph true})
      (assert-attr-graph (table/setproto g Weighted-Attr-Graph)))

    (do (put g :metadata {:attr true :graph true})
      (assert-attr-graph (table/setproto g Attr-Graph)))))

(defn add-weighted-to-edge
  ``
  Add Weighted Edge features to an existing edge, `e`. If `e` is already a Weighted Edge, returns `e` unmodified.
  ``
  [e]
  (assert-edge e)
  (put e :weight 1)
  (cond
    (weighted-edge? e) e

    (attr-edge? e)
    (assert-weighted-edge (table/setproto e Weighted-Attr-Edge))

    (assert-weighted-edge (table/setproto e Weighted-Edge))))

(defn add-weighted
  ``
  Add Weighted Graph features to an existing graph, `g`. If `g` is already a weighted graph, returns `g` unmodified. 

  Existing edges will have Weighted Edge features added, if they did not have them already. Newly weighted edges will be given the default Weighted Edge weight of 1.
  ``
  [g]
  (assert-graph g)
  (each e (keys (g :adj)) (add-weighted-to-edge e))
  (cond
    (weighted-graph? g) g

    (attr-digraph? g)
    (do (put g :metadata {:weighted true :attr true :digraph true :graph true})
      (assert-weighted-graph (table/setproto g Weighted-Attr-Digraph)))

    (attr-graph? g)
    (do (put g :metadata {:weighted true :attr true :graph true})
      (assert-weighted-graph (table/setproto g Weighted-Attr-Graph)))

    (digraph? g)
    (do (put g :metadata {:weighted true :digraph true :graph true})
      (assert-weighted-graph (table/setproto g Weighted-Digraph)))

    (do (put g :metadata {:weighted true :graph true})
      (assert-weighted-graph (table/setproto g Weighted-Graph)))))

# Functions

(defn nodes
  ``
  Get the nodes of a valid graph. Equivalent to calling 
  (keys (`g` :nodeset)).
  ``
  [g]
  (assert-graph g)
  (keys (g :nodeset)))

(defn add-nodes
  ``
  Given a valid graph, `g`, and any number of values, add 
  an arbitrary number of nodes to `g`.
  ``
  [g & nodes]
  (assert-graph g)
  (each n nodes
    (if (node? n)
      (put (g :nodeset) n true)
      (put (g :nodeset) (node :val n) true)))
  g)

(defn has-node?
  `` 
  Given a valid graph, `g`, and any value, `node`, check 
  whether `node` is in the `:nodeset` of `g`.
  ``
  [g node]
  (assert-graph g)
  (truthy? (has-value? (g :nodeset) node)))

(defn has-edge?
  ``
  Given a valid graph, `g`, and a valid edge, returns `true`
  or `false` indicating whether `g` has that edge.
  ``
  [g edge]
  (assert-graph g)
  (assert-edge edge)
  (let [f (:f edge)
        t (:t edge)]
    (truthy? (has-value? t (keys (get-in g [:adj f] @{}))))))

(defn successors
  ``
  Given a valid graph, `g`, and any node, `node`, returns an array 
  of nodes pointed to by all edges leaving `node`. 
  ``
  [g node]
  (assert-graph g)
  (keys (get-in g [:adj node] @{})))

(defn predecessors
  ``
  Given a valid digraph, `g`, and a node, `node`, returns an array 
  of nodes possessing edges that point to `node`.

  If `g` is not a digraph, returns `(successors g node)`.
  ``
  [g node]
  (assert-graph g)
  (if (digraph? g)
    (keys (get-in g [:in node] @{}))
    (successors g node)))

(defn remove-nodes-prim
  [g nodes neighbors]
  (each neighbor neighbors
    (each node nodes
      (put-in (g :adj) [neighbor node] nil)))
  (when (digraph? g)
    (each neighbor neighbors
      (each node nodes
        (put-in (g :in) [neighbor node] nil)))))

(defn remove-nodes
  ``
  Given a valid graph, `g`, and any number of values, removes 
  those values from `g`'s `:nodeset` and eliminates all edges 
  that include any of those values.
  ``
  [g & nodes]
  (assert (graph? g) (string/format "First argument to `remove-nodes` must be a valid graph. Got: %q" g))
  (let [nodes-flat (flatten nodes)
        nbrs (distinct (filter |(not (has-value? nodes-flat $))
                               (array/concat (mapcat |(successors g $) nodes-flat)
                                             (if (digraph? g)
                                               (mapcat |(predecessors g $) nodes-flat) @[]))))]
    (each n nodes-flat (put (g :nodeset) n nil))
    (each n nodes-flat (put (g :adj) n nil))
    (when (digraph? g)
      (each n nodes-flat (put (g :in) n nil)))
    (remove-nodes-prim g nodes-flat nbrs)
    g))

(defn remove-nodes-succ
  ``
  Given a valid graph, `g`, and any node, `node`, removes all nodes 
  from the graph that are successors to the provided node.
  ``
  [g node]
  (remove-nodes g (successors g node)))

(defn remove-nodes-pred
  ``
  Given a valid digraph, `g`, and any node, `node`, removes all nodes 
  from the graph that are predecssors to the provided node.

  If `g` is not a digraph, performs `(remove-nodes-succ g node)`.
  ``
  [g node]
  (if (digraph? g)
    (remove-nodes g (predecessors g node))
    (remove-nodes g (successors g node))))

(defn out-edges
  ``
  Returns a tuple of all edges that go out from the provided `node`.
  Errors if `g` is not a graph or if `node` is not a node in `g`.

  Does not preserve weights or edge attrs in `g`.
  ``
  [g node]
  (assert (graph? g) (string/format "First argument to `edges` must be a valid graph. Got: %q" g))
  (assert (has-node? g node) "Provided node is not a member of provided graph.")
  (seq [to-node :in (successors g node)]
    (edge :from node :to to-node)))

(defn in-edges
  ``
  Given a valid graph, `g`, and any value, `node`, returns an array 
  of edges in `g` that point to `node`.

  Does not preserve weights or edge attrs in `g`.
  ``
  [g node]
  (seq [n2 :in (predecessors g node)]
    (edge :from n2 :to node)))

(defn out-degree
  ``
  Given a valid graph, `g`, and any value, `node`, returns an integer 
  indicating the number of edges in `g` that point to `node`. 

  Equivalent to `(length (out-edges g node))`.
  ``
  [g node]
  (length (out-edges g node)))

(defn in-degree
  ``
  Takes a digraph, `g`, and a node, `node`, and returns an integer 
  representing the number of directional edges pointing "in" to that node.
  
  If `g` is not a digraph, performs `(out-degree g node)`.
  ``
  [g node]
  (if (digraph? g)
    (length (get-in g [:in node] @{}))
    (length (out-edges g node))))

(defn edges
  ``
  Iterates all nodes in a graph `g` and returns the full set of 
  all edges from each node to each of that node's successor nodes.
  ``
  [g]
  (assert (graph? g) (string/format "Argument to `edges` must be a valid graph. Got: %q" (table/proto-flatten g)))
  (seq [node :in (nodes g)
        edge :in (out-edges g node)]
    edge))

(defn update-edge
  ``
  Update an edge.
  ``
  [g edge operation]
  (assert-graph graph)
  (assert-edge edge)
  (let [{:from n1 :to n2 :weight w} edge
        old-content (get-in g [:adj n1 n2])
        content
        (cond
          (= operation :remove) nil)]))
