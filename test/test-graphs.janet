(use judge)
(use /src/graphs)
(use /test/test-utils)

(assert-docs "/src/graphs")

(def start (os/clock))

(defmacro* deftest-g [name & args]
  ~(deftest ,name
     (test Graph {:adj @{} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{}})
     (def before-graph (defgraph))
     (test before-graph @{:adj @{} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{}})
     (setdyn 'before-graph nil)

     ,;args

     (test Graph {:adj @{} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{}})
     (def after-graph (defgraph))
     (test after-graph @{:adj @{} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{}})
     (setdyn 'after-graph nil)))

(deftest "graph-schema"
  (test (graph-schema Graph) true)
  (def test-graph (defgraph))
  (put test-graph :in nil)
  (test (graph-schema test-graph) nil))

(deftest "unweighted-edge-schema"
  (test (unweighted-edge-schema @[:a :b]) true)
  (test (unweighted-edge-schema @[:a :b :c]) true)
  (test (unweighted-edge-schema {:x 1 :y 2}) nil)
  (test (unweighted-edge-schema @["here" "there"]) true)
  (test (unweighted-edge-schema @[:a @[:c :d]]) true)
  (test (unweighted-edge-schema @[]) true))

(deftest "weighted-edge-schema"
  (test (weighted-edge-schema @[:a :b]) nil)
  (test (weighted-edge-schema @[:a :b :c]) false)
  (test (weighted-edge-schema @[:a :b 1]) true)
  (test (weighted-edge-schema {:x 1 :y 2}) nil)
  (test (weighted-edge-schema @["here" "there"]) nil)
  (test (weighted-edge-schema @["here" "there" 50]) true)
  (test (weighted-edge-schema @[:a @[:c :d]]) nil)
  (test (weighted-edge-schema @[]) true))

(deftest "edge-schema"
  (test (edge-schema :a) false)
  (test (edge-schema @[:a :b :c]) true)
  (test (edge-schema @[:a :b 15]) true)
  (test (edge-schema {:x 1 :y 2}) false)
  (test (edge-schema @["here" "there"]) true)
  (test (edge-schema @[:a @[:c :d]]) true)
  (test (edge-schema false) false)
  (test (edge-schema nil) false)
  (test (edge-schema @[]) true))

(deftest "node-schema"
  (test (node-schema :a) true)
  (test (node-schema @[:a :b :c]) true)
  (test (node-schema {:x 1 :y 2}) true)
  (test (node-schema @["here" "there"]) true)
  (test (node-schema @[:a @[:c :d]]) true)
  (test (node-schema false) false)
  (test (node-schema nil) false))

(deftest "graph?"
  (test (graph? Graph) true)
  (def test-graph (defgraph))
  (test (graph? test-graph) true))

(deftest "graph?, not a graph"
  (test (graph? @{}) nil)
  (test (graph? "not a graph") nil)
  (test (graph? @[:a :b :c]) nil))

(deftest "metadata"
  (def test-graph (defgraph))
  (test (metadata test-graph) @{:graph true})
  (setdyn 'agraph nil))

(deftest "defgraph"
  (def test-graph (defgraph))
  (test (= test-graph Graph) false)
  (test test-graph @{:adj @{} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{}})
  (test-error (defgraph :fails) "optional argument to `defgraph` must be a valid graph Got: :fails"))

(deftest "make-digraph!"
  (def test-graph (defgraph))
  (test ((metadata test-graph) :digraph) nil)
  (make-digraph! test-graph)
  (test ((metadata test-graph) :digraph) true))

(deftest "make-digraph!, with existing edges"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test ((metadata test-graph) :digraph) nil)
  (make-digraph! test-graph)
  (test ((metadata test-graph) :digraph) true)
  (test test-graph
        @{:adj @{:a @{:b true :c true}
                 :b @{:a true :c true}
                 :c @{:a true :b true}}
          :attrs @{}
          :in @{:a @{:b true :c true}
                :b @{:a true :c true}
                :c @{:a true :b true}}
          :metadata @{:digraph true :graph true}
          :nodeset @{:a true :b true :c true}}))

(deftest "make-weighted!, simple"
  (def test-graph-0 (defgraph))
  (test ((metadata test-graph-0) :weighted) nil)
  (make-weighted! test-graph-0)
  (test ((metadata test-graph-0) :weighted) true))

(deftest "make-weighted!, with existing unweighted"
  (def test-graph-1 (defgraph))
  (add-edges test-graph-1 [:a :b] [:b :c] [:b :d])
  (make-weighted! test-graph-1)
  (test test-graph-1 @{:adj @{:a @{:b 1} :b @{:a 1 :c 1 :d 1} :c @{:b 1} :d @{:b 1}} :attrs @{} :in @{} :metadata @{:graph true :weighted true} :nodeset @{:a true :b true :c true :d true}}))

(deftest "make-weighted!, existing unweighted, weight supplied"
  (def test-graph-2 (defgraph))
  (add-edges test-graph-2 [:a :b] [:b :c] [:b :d])
  (make-weighted! test-graph-2 4)
  (test test-graph-2 @{:adj @{ :a @{:b 4} :b @{:a 4 :c 4 :d 4} :c @{:b 4} :d @{:b 4}} :attrs @{} :in @{} :metadata @{:graph true :weighted true} :nodeset @{ :a true :b true :c true :d true}}))

(deftest "make-weighted!, existing unweighted digraph"
  (def test-graph-1 (defgraph))
  (make-digraph! test-graph-1)
  (add-edges test-graph-1 [:a :b] [:b :c] [:b :d])
  (make-weighted! test-graph-1)
  (test test-graph-1 @{:adj @{:a @{:b 1} :b @{:c 1 :d 1}} :attrs @{} :in @{:b @{:a 1} :c @{:b 1} :d @{:b 1}} :metadata @{:digraph true :graph true :weighted true} :nodeset @{:a true :b true :c true :d true}}))

(deftest "make-weighted!, existing unweighted digraph, weight supplied"
  (def test-graph-1 (defgraph))
  (make-digraph! test-graph-1)
  (add-edges test-graph-1 [:a :b] [:b :c] [:b :d])
  (make-weighted! test-graph-1 15)
  (test test-graph-1 @{:adj @{:a @{:b 15} :b @{:c 15 :d 15}} :attrs @{} :in @{:b @{:a 15} :c @{:b 15} :d @{:b 15}} :metadata @{:digraph true :graph true :weighted true} :nodeset @{:a true :b true :c true :d true}}))

(deftest "node?"
  (test (node? @[:a :b]) true)
  (test (node? @[:a :b :c]) true)
  (test (node? {:x 1 :y 2}) true)
  (test (node? @["here" "there"]) true)
  (test (node? @[:a @[:c :d]]) true)
  (test (node? @[:a :b 50]) true)
  (test (node? @[:a :b :c]) true))

(deftest "digraph?"
  (def test-graph (defgraph))
  (test ((metadata test-graph) :digraph) nil)
  (make-digraph! test-graph)
  (test ((metadata test-graph) :digraph) true)
  (test (digraph? test-graph) true))

(deftest "digraph?, not a graph"
  (test (digraph? "not a graph, should fail") false)
  (test (digraph? @{}) nil)
  (test (digraph? @[:a :b :c]) false))

(deftest "weighted?"
  (def test-graph (defgraph))
  (test ((metadata test-graph) :weighted) nil)
  (make-weighted! test-graph)
  (test ((metadata test-graph) :weighted) true)
  (test (weighted? test-graph) true))

(deftest "nodes"
  (test (nodes Graph) @[])
  (def test-graph (defgraph))
  (test (nodes test-graph) @[]))

(deftest "add-nodes"
  (def test-graph (defgraph))
  (add-nodes test-graph :a :b :c 1 2 3 ["a" "b"] "c")
  (test test-graph @{:adj @{} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{ 1 true 2 true 3 true "c" true :a true :b true :c true ["a" "b"] true}}))

(deftest "has-node?"
  (def test-graph (defgraph))
  (add-nodes test-graph :a :b :c 1 2 3 ["a" "b"] "c")
  (test test-graph @{:adj @{} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{ 1 true 2 true 3 true "c" true :a true :b true :c true ["a" "b"] true}})
  (test (has-node? test-graph ["a" "b"]) true)
  (test (has-node? test-graph :d) false))

(deftest "has-edge?"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:a :c])
  (test (has-edge? test-graph [:a :c]) true)
  (test (has-edge? test-graph [:b :c]) false))

(deftest "has-edge?, digraph"
  ((def test-graph (defgraph))
    (make-digraph! test-graph))
  (add-edges test-graph [:a :b] [:a :c])
  (test (has-edge? test-graph [:a :c]) true)
  (test (has-edge? test-graph [:b :c]) false))

(deftest "has-edge?, weighted graph"
  (def test-graph (defgraph))
  (make-weighted! test-graph)
  (add-edges test-graph [:a :b 10] [:a :c 20])
  (test (has-edge? test-graph [:a :c]) true)
  (test (has-edge? test-graph [:b :c]) false))

(deftest "has-edge?, weighted digraph"
  ((def test-graph (defgraph))
    (make-digraph! test-graph))
  (make-weighted! test-graph)
  (add-edges test-graph [:a :b 100] [:a :c 200])
  (test (has-edge? test-graph [:a :c]) true)
  (test (has-edge? test-graph [:b :c]) false))

(deftest "successors"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:a :c])
  (test (successors test-graph :a) @[:c :b])
  (test (successors test-graph :b) @[:a])
  (test (successors test-graph :c) @[:a]))

(deftest "successors, none to find"
  (def test-graph (defgraph))
  (add-nodes test-graph :a :b)
  (test (successors test-graph :a) @[]))

(deftest "predecessors, fails if not a digraph"
  (def test-graph (defgraph))
  (test-error (predecessors test-graph :fails) "Input graph `dg` must be a digraph. Got: @{:adj @{} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{}}"))

(deftest "predecessors"
  ((def test-graph (defgraph))
    (make-digraph! test-graph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test (predecessors test-graph :c) @[:a :b]))

(deftest "remove-nodes-succ"
  (def test-graph (defgraph))
  (make-digraph! test-graph)
  (add-edges test-graph [:a :b] [:a :c] [:c :d])
  (remove-nodes-succ test-graph :c)
  (test test-graph
    @{:adj @{:a @{:b true :c true} :c @{}}
      :attrs @{}
      :in @{:b @{:a true}
            :c @{:a true}}
      :metadata @{:digraph true :graph true}
      :nodeset @{:a true :b true :c true}}))

(deftest "remove-nodes-pred"
  (def test-graph (defgraph))
  (make-digraph! test-graph)
  (add-edges test-graph [:a :b] [:a :c] [:c :d])
  (remove-nodes-pred test-graph :c)
  (test test-graph
    @{:adj @{:b @{} :c @{:d true}}
      :attrs @{}
      :in @{:d @{:c true}}
      :metadata @{:digraph true :graph true}
      :nodeset @{:b true :c true :d true}}))

(deftest "remove-nodes-prim"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (remove-nodes-prim test-graph [:a] [:b :c])
  (test test-graph 
        @{:adj @{:a @{:b true :c true}
                 :b @{:c true}
                 :c @{:b true}}
          :attrs @{}
          :in @{}
          :metadata @{:graph true}
          :nodeset @{:a true :b true :c true}}))

(deftest "remove-nodes, not passed a graph"
  (test-error (remove-nodes @{} :fails) "First argument to `remove-nodes` must be a valid graph. Got: @{}"))

(deftest "remove-nodes, not in list, 1"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (remove-nodes test-graph :a)
  (test test-graph @{:adj @{:b @{:c true} :c @{:b true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:b true :c true}}))

(deftest "remove-nodes, not in list, 2"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (remove-nodes test-graph :c)
  (test test-graph
    @{:adj @{:a @{:b true} :b @{:a true}}
      :attrs @{}
      :in @{}
      :metadata @{:graph true}
      :nodeset @{:a true :b true}}))

(deftest "remove-nodes, in a list"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (remove-nodes test-graph [:a])
  (test test-graph @{:adj @{:b @{:c true} :c @{:b true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:b true :c true}}))

(deftest "remove-nodes, not in list"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (remove-nodes test-graph :a :c)
  (test test-graph @{:adj @{:b @{}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:b true}}))

(deftest "remove-nodes, in a list"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (remove-nodes test-graph [:a :c])
  (test test-graph @{:adj @{:b @{}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:b true}}))

(deftest "remove-nodes, digraph, in a list, 1"
  (def test-graph (defgraph))
  (make-digraph! test-graph)
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (remove-nodes test-graph [:a])
  (test test-graph
    @{:adj @{:b @{:c true} :c @{}}
      :attrs @{}
      :in @{:c @{:b true}}
      :metadata @{:digraph true :graph true}
      :nodeset @{:b true :c true}}))

(deftest "remove-nodes, digraph, in a list, 2"
  (def test-graph (defgraph))
  (make-digraph! test-graph)
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (remove-nodes test-graph [:c])
  (test test-graph
    @{:adj @{:a @{:b true} :b @{}}
      :attrs @{}
      :in @{:b @{:a true}}
      :metadata @{:digraph true :graph true}
      :nodeset @{:a true :b true}}))

(deftest "out-edges, fails if `g` is not a valid graph"
  (test-error (out-edges :fails :a) "First argument to `edges` must be a valid graph. Got: :fails"))

(deftest "out-edges, fails if `node` not member of `g`"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test-error (out-edges test-graph :d) "Provided node is not a member of provided graph."))

(deftest "out-edges"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test (out-edges test-graph :a) @[[:a :c] [:a :b]])
  (test (out-edges test-graph :b) @[[:b :c] [:b :a]])
  (test (out-edges test-graph :c) @[[:c :a] [:c :b]]))

(deftest "out-edges, none"
  (def test-graph (defgraph))
  (add-nodes test-graph :a)
  (test (out-edges test-graph :a) @[]))

(deftest "out-edges, digraph"
  ((def test-graph (defgraph))
    (make-digraph! test-graph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test (out-edges test-graph :a) @[[:a :c] [:a :b]])
  (test (out-edges test-graph :b) @[[:b :c]])
  (test (out-edges test-graph :c) @[]))

(deftest "in-edges, not a digraph"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test-error (in-edges test-graph :b) "Input graph `dg` must be a digraph. Got: @{:adj @{:a @{:b true :c true} :b @{:a true :c true} :c @{:a true :b true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:a true :b true :c true}}"))

(deftest "in-edges"
  ((def test-graph (defgraph))
    (make-digraph! test-graph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test (in-edges test-graph :a) @[])
  (test (in-edges test-graph :b) @[[:a :b]])
  (test (in-edges test-graph :c) @[[:a :c] [:b :c]]))

(deftest "out-degree"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test (out-degree test-graph :a) 2)
  (test (out-degree test-graph :b) 2)
  (test (out-degree test-graph :c) 2))

(deftest "out-degree, none"
  (def test-graph (defgraph))
  (add-nodes test-graph :a)
  (test (out-degree test-graph :a) 0))

(deftest "out-degree, digraph"
  ((def test-graph (defgraph))
    (make-digraph! test-graph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test (out-degree test-graph :a) 2)
  (test (out-degree test-graph :c) 0))

(deftest "in-degree, not digraph"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test-error (in-degree test-graph :a) "Input graph `dg` must be a valid digraph. Got: @{:adj @{:a @{:b true :c true} :b @{:a true :c true} :c @{:a true :b true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:a true :b true :c true}}")
  (test-error (in-degree test-graph :b) "Input graph `dg` must be a valid digraph. Got: @{:adj @{:a @{:b true :c true} :b @{:a true :c true} :c @{:a true :b true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:a true :b true :c true}}")
  (test-error (in-degree test-graph :c) "Input graph `dg` must be a valid digraph. Got: @{:adj @{:a @{:b true :c true} :b @{:a true :c true} :c @{:a true :b true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:a true :b true :c true}}"))

(deftest "in-degree, none"
  ((def test-graph (defgraph))
    (make-digraph! test-graph))
  (add-nodes test-graph :a)
  (test (in-degree test-graph :a) 0))

(deftest "in-degree"
  ((def test-graph (defgraph))
    (make-digraph! test-graph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test (in-degree test-graph :a) 0)
  (test (in-degree test-graph :b) 1)
  (test (in-degree test-graph :c) 2))

(deftest "edges, fails if graph `g` is not a valid graph"
  (test-error (edges :fails) "Argument to `edges` must be a valid graph. Got: :fails"))

(deftest "edges, none"
  (def test-graph (defgraph))
  (add-nodes test-graph :a :b)
  (test (edges test-graph) @[]))

(deftest "edges"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test (edges test-graph) @[[:c :a] [:c :b] [:a :c] [:a :b] [:b :c] [:b :a]]))

(deftest "edges, digraph"
  ((def test-graph (defgraph))
    (make-digraph! test-graph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test (edges test-graph) @[[:a :c] [:a :b] [:b :c]]))

(deftest "edges, weighted graph"
  (def test-graph (defgraph))
  (make-weighted! test-graph)
  (add-edges test-graph [:a :b 5] [:b :c 6] [:a :c 7])
  (test (edges test-graph) @[[:c :a] [:c :b] [:a :c] [:a :b] [:b :c] [:b :a]]))

(deftest "edges, weighted digraph"
  ((def test-graph (defgraph))
    (make-digraph! test-graph))
  (make-weighted! test-graph)
  (add-edges test-graph [:a :b 7] [:b :c 8] [:a :c 9])
  (test (edges test-graph) @[[:a :c] [:a :b] [:b :c]]))
(deftest "add-edges, fails if bad graph"
  (test-error (add-edges :fails [:a :b]) "First argument to `add-edges` must be a valid graph. Got: :fails"))

(deftest "add-edges, fails if bad edges"
  (def test-graph (defgraph))
  (test-error (add-edges test-graph :fails) "All edges passed to `add-edges` must be valid edges. Got: :fails"))

(deftest "add-edges, simple"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:a :c])
  (test (metadata test-graph) @{:graph true})
  (test test-graph @{:adj @{:a @{:b true :c true} :b @{:a true} :c @{:a true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:a true :b true :c true}}))

(deftest "add-edges, digraph"
  (def test-digraph (defgraph))
  (make-digraph! test-digraph)
  (add-edges test-digraph [:a :b] [:a :c])
  (test (metadata test-digraph) @{:digraph true :graph true})
  (test test-digraph @{:adj @{:a @{:b true :c true}} :attrs @{} :in @{:b @{:a true} :c @{:a true}} :metadata @{:digraph true :graph true} :nodeset @{:a true :b true :c true}}))

(deftest "add-edges, weighted, fails if bad edges"
  (def test-weighted-graph (defgraph))
  (make-weighted! test-weighted-graph)
  (test (metadata test-weighted-graph) @{:graph true :weighted true})
  (test-error (add-edges test-weighted-graph [:a :b] [:a :c]) "All edges passed to `add-edges` with a weighted graph must be valid weighted edges. Got: (:a :b)"))

(deftest "add-edges, weighted"
  (def test-weighted-graph (defgraph))
  (make-weighted! test-weighted-graph)
  (test (metadata test-weighted-graph) @{:graph true :weighted true})
  (add-edges test-weighted-graph [:a :b 50] [:a :c 100])
  (test test-weighted-graph @{:adj @{:a @{:b 50 :c 100} :b @{:a 50} :c @{:a 100}} :attrs @{} :in @{} :metadata @{:graph true :weighted true} :nodeset @{:a true :b true :c true}}))

(deftest "add-edges, weighted and directional, fails if bad edges"
  (def test-weighted-digraph (defgraph))
  (make-digraph! test-weighted-digraph)
  (make-weighted! test-weighted-digraph)
  (test (metadata test-weighted-digraph) @{:digraph true :graph true :weighted true})
  (test-error (add-edges test-weighted-digraph [:a :b] [:a :c]) "All edges passed to `add-edges` with a weighted graph must be valid weighted edges. Got: (:a :b)"))

(deftest "add-edges, weighted and directional"
  (def test-weighted-digraph (defgraph))
  (make-digraph! test-weighted-digraph)
  (make-weighted! test-weighted-digraph)
  (test (metadata test-weighted-digraph) @{:digraph true :graph true :weighted true})
  (test (add-edges test-weighted-digraph [:a :b 50] [:a :c 100]) @{:adj @{:a @{:b 50 :c 100}} :attrs @{} :in @{:b @{:a 50} :c @{:a 100}} :metadata @{:digraph true :graph true :weighted true} :nodeset @{:a true :b true :c true}}))

(deftest "remove-edges, fails if bad graph"
  (test-error (remove-edges :fails [:a :b]) "First argument to `remove-edges` must be a valid graph. Got: :fails"))

(deftest "remove-edges, fails if bad edges"
  (def test-graph (defgraph))
  (test-error (add-edges test-graph :fails) "All edges passed to `add-edges` must be valid edges. Got: :fails"))

(deftest "remove-edges, simple"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:a :c])
  (remove-edges test-graph [:a :b])
  (test test-graph @{:adj @{:a @{:c true} :b @{} :c @{:a true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:a true :b true :c true}}))

(deftest "remove-edges, digraph"
  (def test-digraph (defgraph))
  (make-digraph! test-digraph)
  (add-edges test-digraph [:a :b] [:a :c])
  (remove-edges test-digraph [:a :b])
  (test test-digraph @{:adj @{:a @{:c true}} :attrs @{} :in @{:b @{} :c @{:a true}} :metadata @{:digraph true :graph true} :nodeset @{:a true :b true :c true}}))

(deftest "remove-edges, weighted"
  (def test-weighted-graph (defgraph))
  (make-weighted! test-weighted-graph)
  (add-edges test-weighted-graph [:a :b 15] [:a :c 25])
  (remove-edges test-weighted-graph [:a :b])
  (test test-weighted-graph @{:adj @{:a @{:c 25} :b @{} :c @{:a 25}} :attrs @{} :in @{} :metadata @{:graph true :weighted true} :nodeset @{:a true :b true :c true}}))

(deftest "remove-edges, weighted and directional"
  (def test-weighted-digraph (defgraph))
  (make-digraph! test-weighted-digraph)
  (make-weighted! test-weighted-digraph)
  (add-edges test-weighted-digraph [:a :b 15] [:a :c 25])
  (remove-edges test-weighted-digraph [:a :b])
  (test test-weighted-digraph @{:adj @{:a @{:c 25}} :attrs @{} :in @{:b @{} :c @{:a 25}} :metadata @{:digraph true :graph true :weighted true} :nodeset @{:a true :b true :c true}}))

(deftest "remove-edges-all, simple"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:a :c])
  (remove-edges-all test-graph)
  (test test-graph @{:adj @{:a @{} :b @{} :c @{}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:a true :b true :c true}}))

(deftest "remove-edges-all, digraph"
  (def test-digraph (defgraph))
  (make-digraph! test-digraph)
  (add-edges test-digraph [:a :b] [:a :c])
  (remove-edges-all test-digraph)
  (test test-digraph @{:adj @{:a @{}} :attrs @{} :in @{:b @{} :c @{}} :metadata @{:digraph true :graph true} :nodeset @{:a true :b true :c true}}))

(deftest "remove-edges-all, weighted"
  (def test-weighted-graph (defgraph))
  (make-weighted! test-weighted-graph)
  (add-edges test-weighted-graph [:a :b 15] [:a :c 25])
  (remove-edges-all test-weighted-graph)
  (test test-weighted-graph @{:adj @{:a @{} :b @{} :c @{}} :attrs @{} :in @{} :metadata @{:graph true :weighted true} :nodeset @{:a true :b true :c true}}))

(deftest "remove-edges-all, weighted and directional"
  (def test-weighted-digraph (defgraph))
  (make-digraph! test-weighted-digraph)
  (make-weighted! test-weighted-digraph)
  (add-edges test-weighted-digraph [:a :b 15] [:a :c 25])
  (remove-edges-all test-weighted-digraph)
  (test test-weighted-digraph @{:adj @{:a @{}} :attrs @{} :in @{:b @{} :c @{}} :metadata @{:digraph true :graph true :weighted true} :nodeset @{:a true :b true :c true}}))

(deftest "remove-nodes-all, simple"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:a :c])
  (remove-nodes-all test-graph)
  (test test-graph @{:adj @{} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{}}))

(deftest "remove-nodes-all, digraph"
  (def test-digraph (defgraph))
  (make-digraph! test-digraph)
  (add-edges test-digraph [:a :b] [:a :c])
  (remove-nodes-all test-digraph)
  (test test-digraph @{:adj @{} :attrs @{} :in @{} :metadata @{:digraph true :graph true} :nodeset @{}}))

(deftest "remove-nodes-all, weighted"
  (def test-weighted-graph (defgraph))
  (make-weighted! test-weighted-graph)
  (add-edges test-weighted-graph [:a :b 15] [:a :c 25])
  (remove-nodes-all test-weighted-graph)
  (test test-weighted-graph @{:adj @{} :attrs @{} :in @{} :metadata @{:graph true :weighted true} :nodeset @{}}))

(deftest "remove-nodes-all, weighted and directional"
  (def test-weighted-digraph (defgraph))
  (make-digraph! test-weighted-digraph)
  (make-weighted! test-weighted-digraph)
  (add-edges test-weighted-digraph [:a :b 15] [:a :c 25])
  (remove-nodes-all test-weighted-digraph)
  (test test-weighted-digraph @{:adj @{} :attrs @{} :in @{} :metadata @{:digraph true :graph true :weighted true} :nodeset @{}}))

(deftest "transpose-digraph, fails on non-digraph"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:a :c] [:c :d])
  (test-error (transpose-digraph test-graph) "First argument to `transpose-digraph` must be a valid digraph. Got: @{:adj @{ :a @{:b true :c true} :b @{:a true} :c @{:a true :d true} :d @{:c true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{ :a true :b true :c true :d true}}"))

(deftest "transpose-digraph, simple digraph"
  (def test-graph (defgraph))
  (make-digraph! test-graph)
  (add-edges test-graph [:a :b] [:a :c] [:c :d])
  (test (transpose-digraph test-graph) @{:adj @{:b @{:a true} :c @{:a true} :d @{:c true}} :attrs @{} :in @{:a @{:b true :c true} :c @{:d true}} :metadata @{:digraph true :graph true} :nodeset @{ :a true :b true :c true :d true}}))

(deftest "transpose-digraph, weighted digraph"
  (def test-graph (defgraph))
  (make-digraph! test-graph)
  (make-weighted! test-graph)
  (add-edges test-graph [:a :b 100] [:a :c 200] [:c :d 300])
  (test (transpose-digraph test-graph) @{:adj @{:b @{:a 100} :c @{:a 200} :d @{:c 300}} :attrs @{} :in @{:a @{:b 100 :c 200} :c @{:d 300}} :metadata @{:digraph true :graph true :weighted true} :nodeset @{ :a true :b true :c true :d true}}))

(deftest "subgraph"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:b :d] [:d :e] [:c :f])
  (def sub-test-graph (subgraph test-graph [:a :b :c]))
  (test sub-test-graph @{:adj @{:a @{:b true} :b @{:a true :c true} :c @{:b true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:a true :b true :c true}}))

(deftest "add-path"
  (def test-graph (defgraph))
  (add-path test-graph [:a :b :c :d :e])
  (test test-graph @{:adj @{ :a @{:b true} :b @{:a true :c true} :c @{:b true :d true} :d @{:c true :e true} :e @{:d true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{ :a true :b true :c true :d true :e true}}))

(deftest "add-path, two of them"
  (def test-graph (defgraph))
  (add-path test-graph [:a :b :c :d :e])
  (add-path test-graph [:a :b :c :f :g])
  (test test-graph @{:adj @{ :a @{:b true} :b @{:a true :c true} :c @{:b true :d true :f true} :d @{:c true :e true} :e @{:d true} :f @{:c true :g true} :g @{:f true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{ :a true :b true :c true :d true :e true :f true :g true}}))

(deftest "add-path, with digraph"
  (def test-graph (defgraph))
  (make-digraph! test-graph)
  (add-path test-graph [:a :b :c :d :e])
  (test test-graph @{:adj @{ :a @{:b true} :b @{:c true} :c @{:d true} :d @{:e true}} :attrs @{} :in @{ :b @{:a true} :c @{:b true} :d @{:c true} :e @{:d true}} :metadata @{:digraph true :graph true} :nodeset @{ :a true :b true :c true :d true :e true}}))

(deftest "add-path, with weighted graph"
  (def test-graph (defgraph))
  (make-weighted! test-graph)
  (add-path test-graph [:a :b :c :d :e])
  (test test-graph @{:adj @{ :a @{:b 1} :b @{:a 1 :c 1} :c @{:b 1 :d 1} :d @{:c 1 :e 1} :e @{:d 1}} :attrs @{} :in @{} :metadata @{:graph true :weighted true} :nodeset @{ :a true :b true :c true :d true :e true}}))

(deftest "add-path, with weighted digraph"
  (def test-graph (defgraph))
  (make-digraph! test-graph)
  (make-weighted! test-graph)
  (add-path test-graph [:a :b :c :d :e])
  (test test-graph @{:adj @{ :a @{:b 1} :b @{:c 1} :c @{:d 1} :d @{:e 1}} :attrs @{} :in @{ :b @{:a 1} :c @{:b 1} :d @{:c 1} :e @{:d 1}} :metadata @{:digraph true :graph true :weighted true} :nodeset @{ :a true :b true :c true :d true :e true}}))

(deftest "add-cycle"
  (def test-graph (defgraph))
  (add-cycle test-graph [:a :b :c :d :e])
  (test test-graph @{:adj @{ :a @{:b true :e true} :b @{:a true :c true} :c @{:b true :d true} :d @{:c true :e true} :e @{:a true :d true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{ :a true :b true :c true :d true :e true}}))

(deftest "add-cycle, two of them"
  (def test-graph (defgraph))
  (add-cycle test-graph [:a :b :c :d :e])
  (add-cycle test-graph [:a :b :c :f :g])
  (test test-graph @{:adj @{ :a @{:b true :e true :g true} :b @{:a true :c true} :c @{:b true :d true :f true} :d @{:c true :e true} :e @{:a true :d true} :f @{:c true :g true} :g @{:a true :f true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{ :a true :b true :c true :d true :e true :f true :g true}}))

(deftest "add-cycle, with digraph"
  (def test-graph (defgraph))
  (make-digraph! test-graph)
  (add-cycle test-graph [:a :b :c :d :e])
  (test test-graph @{:adj @{ :a @{:b true} :b @{:c true} :c @{:d true} :d @{:e true} :e @{:a true}} :attrs @{} :in @{ :a @{:e true} :b @{:a true} :c @{:b true} :d @{:c true} :e @{:d true}} :metadata @{:digraph true :graph true} :nodeset @{ :a true :b true :c true :d true :e true}}))

(deftest "add-cycle, with weighted graph"
  (def test-graph (defgraph))
  (make-weighted! test-graph)
  (add-cycle test-graph [:a :b :c :d :e])
  (test test-graph @{:adj @{ :a @{:b 1 :e 1} :b @{:a 1 :c 1} :c @{:b 1 :d 1} :d @{:c 1 :e 1} :e @{:a 1 :d 1}} :attrs @{} :in @{} :metadata @{:graph true :weighted true} :nodeset @{ :a true :b true :c true :d true :e true}}))

(deftest "add-cycle, with weighted digraph"
  (def test-graph (defgraph))
  (make-digraph! test-graph)
  (make-weighted! test-graph)
  (add-cycle test-graph [:a :b :c :d :e])
  (test test-graph @{:adj @{ :a @{:b 1} :b @{:c 1} :c @{:d 1} :d @{:e 1} :e @{:a 1}} :attrs @{} :in @{ :a @{:e 1} :b @{:a 1} :c @{:b 1} :d @{:c 1} :e @{:d 1}} :metadata @{:digraph true :graph true :weighted true} :nodeset @{ :a true :b true :c true :d true :e true}}))

(deftest "build-graph, with one graph input"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  
  (test (build-graph (defgraph) test-graph) 
        @{:adj @{:a @{:b true :c true}
                 :b @{:a true :c true}
                 :c @{:a true :b true}}
          :attrs @{}
          :in @{}
          :metadata @{:graph true}
          :nodeset @{:a true :b true :c true}}))

(deftest "build-graph, with two graphs as input"
  (def test-graph1 (defgraph))
  (add-edges test-graph1 [:a :b] [:b :c] [:a :c])
  
  (def test-graph2 (defgraph))
  (add-edges test-graph2 [:d :e] [:e :f] [:f :g] [:f :h])

  (test (build-graph (defgraph) test-graph1 test-graph2) @{:adj @{ :a @{:b true :c true} :b @{:a true :c true} :c @{:a true :b true} :d @{:e true} :e @{:d true :f true} :f @{:e true :g true :h true} :g @{:f true} :h @{:f true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{ :a true :b true :c true :d true :e true :f true :g true :h true}}))

(deftest "build-graph, with one digraph input"
  (def test-graph (defgraph))
  (make-digraph! test-graph)
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  
  (test (build-graph (defgraph) test-graph) @{:adj @{:a @{:b true :c true} :b @{:a true :c true} :c @{:a true :b true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:a true :b true :c true}}))

(deftest "build-graph, one graph and one digraph as input"
  (def test-graph1 (defgraph))
  (make-digraph! test-graph1)
  (add-edges test-graph1 [:a :b] [:b :c] [:a :c])
  
  (def test-graph2 (defgraph))
  (add-edges test-graph2 [:d :e] [:e :f] [:f :g] [:f :h])

  (test (build-graph (defgraph) test-graph1 test-graph2) @{:adj @{ :a @{:b true :c true} :b @{:a true :c true} :c @{:a true :b true} :d @{:e true} :e @{:d true :f true} :f @{:e true :g true :h true} :g @{:f true} :h @{:f true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{ :a true :b true :c true :d true :e true :f true :g true :h true}}))

(deftest "build-graph, with two digraphs as input"
  (def test-graph1 (defgraph))
  (make-digraph! test-graph1)
  (add-edges test-graph1 [:a :b] [:b :c] [:a :c])
  
  (def test-graph2 (defgraph))
  (make-digraph! test-graph2)
  (add-edges test-graph2 [:d :e] [:e :f] [:f :g] [:f :h])

  (test (build-graph (defgraph) test-graph1 test-graph2) @{:adj @{ :a @{:b true :c true} :b @{:a true :c true} :c @{:a true :b true} :d @{:e true} :e @{:d true :f true} :f @{:e true :g true :h true} :g @{:f true} :h @{:f true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{ :a true :b true :c true :d true :e true :f true :g true :h true}}))

(deftest "build-graph, with one weighted graph input"
  (def test-graph (defgraph))
  (make-digraph! test-graph)
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  
  (test (build-graph (defgraph) test-graph) @{:adj @{:a @{:b true :c true} :b @{:a true :c true} :c @{:a true :b true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:a true :b true :c true}}))

(deftest "build-graph, one graph and one weighted graph as input"
  (def test-graph1 (defgraph))
  (make-weighted! test-graph1)
  (add-edges test-graph1 [:a :b 1] [:b :c 1] [:a :c 1])
  
  (def test-graph2 (defgraph))
  (add-edges test-graph2 [:d :e] [:e :f] [:f :g] [:f :h])

  (test (build-graph (defgraph) test-graph1 test-graph2) @{:adj @{ :a @{:b true :c true} :b @{:a true :c true} :c @{:a true :b true} :d @{:e true} :e @{:d true :f true} :f @{:e true :g true :h true} :g @{:f true} :h @{:f true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{ :a true :b true :c true :d true :e true :f true :g true :h true}}))

(deftest "build-graph, with two weighted graphs as input"
  (def test-graph1 (defgraph))
  (make-weighted! test-graph1)
  (add-edges test-graph1 [:a :b 1] [:b :c 1] [:a :c 1])
  
  (def test-graph2 (defgraph))
  (make-weighted! test-graph2)
  (add-edges test-graph2 [:d :e 1] [:e :f 1] [:f :g 1] [:f :h 1])

  (test (build-graph (defgraph) test-graph1 test-graph2) @{:adj @{ :a @{:b true :c true} :b @{:a true :c true} :c @{:a true :b true} :d @{:e true} :e @{:d true :f true} :f @{:e true :g true :h true} :g @{:f true} :h @{:f true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{ :a true :b true :c true :d true :e true :f true :g true :h true}}))

(deftest "build-graph, weighted graph as start, unweighted graph as input"
  (def test-graph1 (defgraph))
  (make-weighted! test-graph1)
  (add-edges test-graph1 [:a :b 2] [:b :c 2] [:a :c 2])
  
  (def test-graph2 (defgraph)) 
  (add-edges test-graph2 [:d :e] [:e :f] [:f :g] [:f :h])

  (test (build-graph test-graph1 test-graph2) @{:adj @{ :a @{:b 2 :c 2} :b @{:a 2 :c 2} :c @{:a 2 :b 2} :d @{:e 1} :e @{:d 1 :f 1} :f @{:e 1 :g 1 :h 1} :g @{:f 1} :h @{:f 1}} :attrs @{} :in @{} :metadata @{:graph true :weighted true} :nodeset @{ :a true :b true :c true :d true :e true :f true :g true :h true}}))

(deftest "build-graph, weighted graph as start, weighted graph as input"
  (def test-graph1 (defgraph))
  (make-weighted! test-graph1)
  (add-edges test-graph1 [:a :b 1] [:b :c 1] [:a :c 1])
  
  (def test-graph2 (defgraph))
  (make-weighted! test-graph2)
  (add-edges test-graph2 [:d :e 1] [:e :f 1] [:f :g 1] [:f :h 1])

  (test (build-graph test-graph1 test-graph2) @{:adj @{ :a @{:b 1 :c 1} :b @{:a 1 :c 1} :c @{:a 1 :b 1} :d @{:e 1} :e @{:d 1 :f 1} :f @{:e 1 :g 1 :h 1} :g @{:f 1} :h @{:f 1}} :attrs @{} :in @{} :metadata @{:graph true :weighted true} :nodeset @{ :a true :b true :c true :d true :e true :f true :g true :h true}}))





(deftest "build-graph, with one edge input"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  
  (test (build-graph (defgraph) [:c :d]) @{:adj @{:c @{:d true} :d @{:c true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:c true :d true}}))

(deftest "build-graph, with two edges as input"
  (def test-graph1 (defgraph))
  (add-edges test-graph1 [:a :b] [:b :c] [:a :c]) 

  (test (build-graph (defgraph) [:c :d] [:d :e]) @{:adj @{:c @{:d true} :d @{:c true :e true} :e @{:d true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:c true :d true :e true}}))

(deftest "build-graph, with one graph and one edge as input"
  (def test-graph1 (defgraph)) 
  (add-edges test-graph1 [:a :b] [:b :c] [:a :c])
  
  (def test-graph2 (defgraph))
  (add-edges test-graph2 [:d :e] [:e :f] [:f :g] [:f :h])
  
  (test (build-graph test-graph1 test-graph2 [:z :x]) @{:adj @{ :a @{:b true :c true} :b @{:a true :c true} :c @{:a true :b true} :d @{:e true} :e @{:d true :f true} :f @{:e true :g true :h true} :g @{:f true} :h @{:f true} :x @{:z true} :z @{:x true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{ :a true :b true :c true :d true :e true :f true :g true :h true :x true :z true}}))

(deftest "build-graph, strings and keywords as input"
  (def test-graph1 (defgraph)) 
  (add-edges test-graph1 [:a :b] [:b :c] [:a :c]) 

  (test (build-graph test-graph1 "this is a string" :keyword) @{:adj @{:a @{:b true :c true} :b @{:a true :c true} :c @{:a true :b true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{ "this is a string" true :a true :b true :c true :keyword true}}))

(deftest "build-graph, graph as start, adjacency map as input"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  
  (test (build-graph (defgraph) (test-graph :adj)) @{:adj @{:a @{:b true :c true} :b @{:a true :c true} :c @{:a true :b true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:a true :b true :c true}}))

(deftest "build-graph, digraph as start, adjacency map as input"
  (def test-graph (defgraph))
  (make-digraph! test-graph)
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  
  (test (build-graph (make-digraph! (defgraph)) (test-graph :adj)) @{:adj @{:a @{:b true :c true} :b @{:c true}} :attrs @{} :in @{:b @{:a true} :c @{:a true :b true}} :metadata @{:digraph true :graph true} :nodeset @{:a true :b true :c true}}))

(deftest "build-graph, example on loom README"
  (test (build-graph (defgraph) [1 2] [2 3] {3 {4 true} 5 {6 true 7 true}} 7 8 9) @{:adj @{ 1 @{2 true} 2 @{1 true 3 true} 3 @{2 true 4 true} 4 @{3 true} 5 @{6 true 7 true} 6 @{5 true} 7 @{5 true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{ 1 true 2 true 3 true 4 true 5 true 6 true 7 true 8 true 9 true}}))

(deftest "graph"
  (test (graph [1 2] [2 3] {3 {4 true} 5 {6 true 7 true}} 7 8 9) @{:adj @{ 1 @{2 true} 2 @{1 true 3 true} 3 @{2 true 4 true} 4 @{3 true} 5 @{6 true 7 true} 6 @{5 true} 7 @{5 true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{ 1 true 2 true 3 true 4 true 5 true 6 true 7 true 8 true 9 true}}))

(deftest "graph with edge labels, works"
  (test (graph [1 2 "this"] [2 3 "that"]) @{:adj @{1 @{2 "this"} 2 @{1 "this" 3 "that"} 3 @{2 "that"}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{1 true 2 true 3 true}}))

(deftest "digraph with edge labels, works"
  (test (digraph [1 2 "this"] [2 3 "that"]) @{:adj @{1 @{2 "this"} 2 @{3 "that"}} :attrs @{} :in @{2 @{1 "this"} 3 @{2 "that"}} :metadata @{:digraph true :graph true} :nodeset @{1 true 2 true 3 true}}))

(deftest "graph with edge labels, falsey"
  (test (graph [1 2 false] [2 3 nil]) @{:adj @{1 @{2 true} 2 @{1 true 3 true} 3 @{2 true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{1 true 2 true 3 true}}))

(deftest "digraph"
  (test (digraph [1 2] [2 3] {3 {4 true} 5 {6 true 7 true}} 7 8 9) @{:adj @{1 @{2 true} 2 @{3 true} 3 @{4 true} 5 @{6 true 7 true}} :attrs @{} :in @{2 @{1 true} 3 @{2 true} 4 @{3 true} 6 @{5 true} 7 @{5 true}} :metadata @{:digraph true :graph true} :nodeset @{1 true 2 true 3 true 4 true 5 true 6 true 7 true 8 true 9 true}}))

(deftest "weighted-graph"
  (test (weighted-graph [1 2 100] [2 3 100] {3 {4 100} 5 {6 100 7 100}} 7 8 9) @{:adj @{ 1 @{2 100} 2 @{1 100 3 100} 3 @{2 100 4 100} 4 @{3 100} 5 @{6 100 7 100} 6 @{5 100} 7 @{5 100}} :attrs @{} :in @{} :metadata @{:graph true :weighted true} :nodeset @{ 1 true 2 true 3 true 4 true 5 true 6 true 7 true 8 true 9 true}}))

(deftest "weighted-digraph"
  (test (weighted-digraph [1 2 100] [2 3 100] {3 {4 100} 5 {6 100 7 100}} 7 8 9) @{:adj @{ 1 @{2 100} 2 @{3 100} 3 @{4 100} 5 @{6 100 7 100}} :attrs @{} :in @{ 2 @{1 100} 3 @{2 100} 4 @{3 100} 6 @{5 100} 7 @{5 100}} :metadata @{:digraph true :graph true :weighted true} :nodeset @{ 1 true 2 true 3 true 4 true 5 true 6 true 7 true 8 true 9 true}}))

(deftest final-time 
  (print "Elapsed time: " (- (os/clock) start) " seconds"))
