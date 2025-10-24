(use judge)
(use ./dataclasses)
(use ./deep-clone)
(use ./traverse)

(dataclass Node
           :val :array)

(test node @node)

(def Nowhere (node @[]))

(dataclass Edge
           :from [Node Nowhere]
           :to [Node Nowhere]
           {:f |($ :from)
            :t |($ :to)})

(def To-Nowhere (edge))

(test To-Nowhere @{})
(test (table/proto-flatten To-Nowhere)
      @{:_name :Edge
        :f @short-fn
        :from @{}
        :schema @{:f :method
                  :from [pred node?]
                  :t :method
                  :to [pred node?]}
        :t @short-fn
        :to @{}
        :type :Edge})

(dataclass Weighted-edge
           :weight :number)
(table/setproto Weighted-edge Edge)

(test (weighted-edge :weight 5) @{:weight 5})
(test (table/proto-flatten (weighted-edge :weight 5))
      @{:_name :Weighted-edge
        :f @short-fn
        :from @{}
        :schema @{:weight :number}
        :t @short-fn
        :to @{}
        :type :Weighted-edge
        :weight 5})

(def Weightless (weighted-edge))

(dataclass Attr-Edge
           :attrs :table)
(table/setproto Attr-edge Edge)

(dataclass Weighted-Attr-Edge
           :attrs :table)
(table/setproto Weighted-attr-edge Weighted-edge)

(test (table/proto-flatten (weighted-attr-edge))
      @{:_name :Weighted-attr-edge
        :attrs @{}
        :f @short-fn
        :from @{}
        :schema @{:attrs :table}
        :t @short-fn
        :to @{}
        :type :Weighted-attr-edge
        :weight 0})

(test (edge? (weighted-attr-edge)) true)
(test (weighted-edge? (weighted-attr-edge)) true)
(test (weighted-attr-edge? (weighted-attr-edge)) true)

(dataclass Graph
           :metadata [:struct {:graph true}]
           :nodeset :table
           :adj :table
           :in :table
           :attrs :table)

(test Graph
      @{:_name :Graph
        :adj @{}
        :attrs @{}
        :metadata {:graph true}
        :nodeset @{}
        :schema @{:adj :table
                  :attrs :table
                  :in :table
                  :metadata :struct
                  :nodeset :table}
        :type :Graph})

(test graph @graph)
(test (graph? (graph)) true)

(dataclass Digraph
           :metadata [:struct {:digraph true :graph true}]
           :in :table)
(table/setproto Digraph Graph)

(test (table/proto-flatten (digraph))
      @{:_name :Digraph
        :adj @{}
        :attrs @{}
        :in @{}
        :metadata {:digraph true :graph true}
        :nodeset @{}
        :schema @{:metadata :struct}
        :type :Digraph})
(test (get-in (digraph) [:metadata :graph]) true)
(test (graph? (digraph)) true)

(test (graph? (deep-clone (digraph))) true)
(test (digraph? (deep-clone (digraph))) true)

(dataclass Attr-Graph
           :metadata [:struct {:attr true :graph true}]
           :attrs :table)
(table/setproto Attr-graph Graph)

(test (graph? (attr-graph)) true)
(test (attr-graph? (attr-graph)) true)
(test (attr-graph? (graph)) false)

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
  (each node nodes
    (put (g :nodeset) node true))
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

(defn update-edge [g edge])
