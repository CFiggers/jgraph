(use judge) 
(use /src/jgraph)

(def start (os/clock)) 

(defmacro* deftest-g [name & args] 
  ~(deftest ,name
    (test Graph {:adj @{} 
                 :attrs @{} 
                 :in @{} 
                 :metadata @{:graph true} 
                 :nodeset @{}})
    (def before-graph (defgraph))
    (test before-graph @{:adj @{} 
                         :attrs @{} 
                         :in @{} 
                         :metadata @{:graph true} 
                         :nodeset @{}}) 
    (setdyn 'before-graph nil)

    ,;args

    (test Graph {:adj @{} 
                 :attrs @{} 
                 :in @{} 
                 :metadata @{:graph true} 
                 :nodeset @{}})
    (def after-graph (defgraph))
    (test after-graph @{:adj @{} 
                        :attrs @{} 
                        :in @{} 
                        :metadata @{:graph true} 
                        :nodeset @{}})
    (setdyn 'after-graph nil)))

(deftest-g "graph-schema"
  (test (graph-schema Graph) true)
  (def test-graph (defgraph))
  (put test-graph :in nil)
  (test (graph-schema test-graph) nil))

(deftest-g "node-schema"
  (test (node-schema :a)                true)
  (test (node-schema @[:a :b :c])       true)
  (test (node-schema {:x 1 :y 2})       true)
  (test (node-schema @["here" "there"]) true)
  (test (node-schema @[:a @[:c :d]])    true)
  (test (node-schema false)             false)
  (test (node-schema nil)               false))

(deftest-g "unweighted-edge-schema"
  (test (unweighted-edge-schema @[:a :b])          true)
  (test (unweighted-edge-schema @[:a :b :c])       false)
  (test (unweighted-edge-schema {:x 1 :y 2})       nil)
  (test (unweighted-edge-schema @["here" "there"]) true)
  (test (unweighted-edge-schema @[:a @[:c :d]])    true))

(deftest-g "weighted-edge-schema"
  (test (weighted-edge-schema @[:a :b])             nil)
  (test (weighted-edge-schema @[:a :b :c])          false)
  (test (weighted-edge-schema @[:a :b 1])           true)
  (test (weighted-edge-schema {:x 1 :y 2})          nil)
  (test (weighted-edge-schema @["here" "there"])    nil)
  (test (weighted-edge-schema @["here" "there" 50]) true)
  (test (weighted-edge-schema @[:a @[:c :d]])       nil))

(deftest-g "edge-schema"
  (test (edge-schema :a)                false)
  (test (edge-schema @[:a :b :c])       false)
  (test (edge-schema @[:a :b 15])       true)
  (test (edge-schema {:x 1 :y 2})       false)
  (test (edge-schema @["here" "there"]) true)
  (test (edge-schema @[:a @[:c :d]])    true)
  (test (edge-schema false)             false)
  (test (edge-schema nil)               false))

(deftest-g "graph?"
  (test (graph? Graph)  true)
  (def test-graph (defgraph))
  (test (graph? test-graph) true))

(deftest-g "graph?, not a graph" 
  (test (graph? @{}) nil)
  (test (graph? "not a graph") nil)
  (test (graph? @[:a :b :c]) nil))

(deftest-g "metadata"
  (def test-graph (defgraph))
  (test (metadata test-graph) @{:graph true})
  (setdyn 'agraph nil))

(deftest-g "defgraph"
  (def test-graph (defgraph))
  (test (= test-graph Graph) false)
  (test test-graph @{:adj @{} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{}})
  (test-error (defgraph :fails) "optional argument to `defgraph` must be a valid graph"))

(deftest-g "make-digraph!"
  (def test-graph (defgraph))
  (test ((metadata test-graph) :digraph) nil)
  (make-digraph! test-graph)
  (test ((metadata test-graph) :digraph) true))

(deftest-g "make-weighted!, simple"
  (def test-graph-0 (defgraph))
  (test ((metadata test-graph-0) :weighted) nil)
  (make-weighted! test-graph-0)
  (test ((metadata test-graph-0) :weighted) true))

(deftest "make-weighted!, with existing unweighted"
  (def test-graph-1 (defgraph))
  (add-edges test-graph-1 [:a :b] [:b :c] [:b :d])
  (make-weighted! test-graph-1)
  (test test-graph-1 @{:adj @{ :a @{:b 1} :b @{:a 1 :c 1 :d 1} :c @{:b 1} :d @{:b 1}} :attrs @{} :in @{} :metadata @{:graph true :weighted true} :nodeset @{ :a true :b true :c true :d true}})) 

(deftest "make-weighted!, existing unweighted, weight supplied"
  (def test-graph-2 (defgraph))
  (add-edges test-graph-2 [:a :b] [:b :c] [:b :d])
  (make-weighted! test-graph-2 4)
  (test test-graph-2 @{:adj @{:a @{:b 4} :b @{:a 4 :c 4 :d 4} :c @{:b 4} :d @{:b 4}}
                       :attrs @{}
                       :in @{}
                       :metadata @{:graph true :weighted true}
                       :nodeset @{:a true :b true :c true :d true}})) 

(deftest-g "make-weighted!, existing unweighted digraph"
  (def test-graph-1 (defgraph))
  (make-digraph! test-graph-1)
  (add-edges test-graph-1 [:a :b] [:b :c] [:b :d])
  (make-weighted! test-graph-1) 
  (test test-graph-1 @{:adj @{:a @{:b 1} :b @{:c 1 :d 1}} :attrs @{} :in @{:b @{:a 1} :c @{:b 1} :d @{:b 1}} :metadata @{:digraph true :graph true :weighted true} :nodeset @{ :a true :b true :c true :d true}}))

(deftest-g "make-weighted!, existing unweighted digraph, weight supplied"
  (def test-graph-1 (defgraph))
  (make-digraph! test-graph-1)
  (add-edges test-graph-1 [:a :b] [:b :c] [:b :d])
  (make-weighted! test-graph-1 15) 
  (test test-graph-1 @{:adj @{:a @{:b 15} :b @{:c 15 :d 15}} :attrs @{} :in @{:b @{:a 15} :c @{:b 15} :d @{:b 15}} :metadata @{:digraph true :graph true :weighted true} :nodeset @{ :a true :b true :c true :d true}}))

(deftest-g "node?"
  (test (node? @[:a :b])          true)
  (test (node? @[:a :b :c]) true)
  (test (node? {:x 1 :y 2}) true)
  (test (node? @["here" "there"]) true)
  (test (node? @[:a @[:c :d]])    true)
  (test (node? @[:a :b 50])       true)
  (test (node? @[:a :b :c]) true))

(deftest-g "digraph?"
  (def test-graph (defgraph))
  (test ((metadata test-graph) :digraph) nil) 
  (make-digraph! test-graph)
  (test ((metadata test-graph) :digraph) true)
  (test (digraph? test-graph)            true))

(deftest-g "digraph?, not a graph"
  (test (digraph? "not a graph, should fail") false)
  (test (digraph? @{}) nil)
  (test (digraph? @[:a :b :c]) false))

(deftest-g "weighted?"
  (def test-graph (defgraph))
  (test ((metadata test-graph) :weighted) nil)
  (make-weighted! test-graph)
  (test ((metadata test-graph) :weighted) true)
  (test (weighted? test-graph)            true))

(deftest-g "nodes"
  (test (nodes Graph) @[])
  (def test-graph (defgraph))
  (test (nodes test-graph) @[]))

(deftest-g "add-nodes"
  (def test-graph (defgraph))
  (add-nodes test-graph :a :b :c 1 2 3 ["a" "b"] "c")
  (test test-graph @{:adj @{} 
                     :attrs @{} 
                     :in @{} 
                     :metadata @{:graph true} 
                     :nodeset @{ 1 true 2 true 3 true "c" true :a true :b true :c true ["a" "b"] true}}))

(deftest "remove-adj-nodes"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (remove-adj-nodes test-graph [:a] [:b :c])
  (test test-graph @{:adj @{:a @{:b true :c true} :b @{:c true} :c @{:b true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:a true :b true :c true}}))

(deftest "remove-nodes, not passed a graph"
  (test-error (remove-nodes @{} :fails) "Input graph `g` must be a valid graph."))

(deftest "remove-nodes, not in list"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (remove-nodes test-graph :a)
  (test test-graph @{:adj @{:b @{:c true} :c @{:b true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:b true :c true}}))

(deftest "remove-nodes, in a list"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test (remove-nodes test-graph [:a]) @{:adj @{:b @{:c true} :c @{:b true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:b true :c true}})
  (test test-graph @{:adj @{:b @{:c true} :c @{:b true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:b true :c true}}))

(deftest "remove-nodes, not in list"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test (remove-nodes test-graph :a :c) @{:adj @{:b @{}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:b true}})
  (test test-graph @{:adj @{:b @{}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:b true}}))

(deftest "remove-nodes, in a list"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (remove-nodes test-graph [:a :c])
  (test test-graph @{:adj @{:b @{}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:b true}}))

(deftest-g "has-node?"
  (def test-graph (defgraph))
  (add-nodes test-graph :a :b :c 1 2 3 ["a" "b"] "c")
  (test test-graph @{:adj @{} 
                     :attrs @{} 
                     :in @{} 
                     :metadata @{:graph true} 
                     :nodeset @{ 1 true 2 true 3 true "c" true :a true :b true :c true ["a" "b"] true}})
  (test (has-node? test-graph ["a" "b"]) true)
  (test (has-node? test-graph :d) false))

(deftest-g "has-edge?"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:a :c])
  (test (has-edge? test-graph [:a :c]) true)
  (test (has-edge? test-graph [:b :c]) false))

(deftest-g "has-edge?, digraph"
  ((def test-graph (defgraph))
  (make-digraph! test-graph))
  (add-edges test-graph [:a :b] [:a :c])
  (test (has-edge? test-graph [:a :c]) true)
  (test (has-edge? test-graph [:b :c]) false))

(deftest-g "has-edge?, weighted graph"
  (def test-graph (defgraph))
  (make-weighted! test-graph)
  (add-edges test-graph [:a :b 10] [:a :c 20])
  (test (has-edge? test-graph [:a :c]) true)
  (test (has-edge? test-graph [:b :c]) false))

(deftest-g "has-edge?, weighted digraph"
  ((def test-graph (defgraph))
  (make-digraph! test-graph))
  (make-weighted! test-graph)
  (add-edges test-graph [:a :b 100] [:a :c 200])
  (test (has-edge? test-graph [:a :c]) true)
  (test (has-edge? test-graph [:b :c]) false))

(deftest-g "successors"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:a :c])
  (test (successors test-graph :a) @[:c :b]))

(deftest-g "successors, none to find"
  (def test-graph (defgraph))
  (add-nodes test-graph :a :b)
  (test (successors test-graph :a) @[]))

(deftest-g "predecessors, fails if not a digraph"
  (def test-graph (defgraph))
  (test-error (predecessors test-graph :fails) "Input graph `g` must be a digraph."))

(deftest-g "predecessors"
  ((def test-graph (defgraph))
  (make-digraph! test-graph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test (predecessors test-graph :c) @[:a :b]))

(deftest-g "out-edges, fails if `g` is not a valid graph"
  (test-error (out-edges :fails :a) "First argument to `edges` must be a valid graph."))

(deftest-g "out-edges, fails if `node` not member of `g`"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test-error (out-edges test-graph :d) "Provided node is not a member of provided graph."))

(deftest-g "out-edges"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test (out-edges test-graph :a) @[[:a :c] [:a :b]])
  (test (out-edges test-graph :b) @[[:b :c] [:b :a]])
  (test (out-edges test-graph :c) @[[:c :a] [:c :b]]))

(deftest-g "out-edges, none"
  (def test-graph (defgraph))
  (add-nodes test-graph :a)
  (test (out-edges test-graph :a) @[]))

(deftest-g "out-edges, digraph"
  ((def test-graph (defgraph))
  (make-digraph! test-graph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test (out-edges test-graph :a) @[[:a :c] [:a :b]])
  (test (out-edges test-graph :b) @[[:b :c]])
  (test (out-edges test-graph :c) @[]))

(deftest-g "in-edges, not a digraph"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test-error (in-edges test-graph :b) "Input graph `g` must be a digraph."))

(deftest-g "in-edges"
  ((def test-graph (defgraph))
  (make-digraph! test-graph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test (in-edges test-graph :a) @[])
  (test (in-edges test-graph :b) @[[:a :b]])
  (test (in-edges test-graph :c) @[[:a :c] [:b :c]]))

(deftest-g "out-degree"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test (out-degree test-graph :a) 2)
  (test (out-degree test-graph :b) 2)
  (test (out-degree test-graph :c) 2))

(deftest-g "out-degree, none"
  (def test-graph (defgraph))
  (add-nodes test-graph :a)
  (test (out-degree test-graph :a) 0))

(deftest-g "out-degree, digraph"
  ((def test-graph (defgraph))
  (make-digraph! test-graph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test (out-degree test-graph :a) 2)
  (test (out-degree test-graph :c) 0))

(deftest-g "in-degree, not digraph"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test-error (in-degree test-graph :a) "Input graph `g` must be a valid digraph.")
  (test-error (in-degree test-graph :b) "Input graph `g` must be a valid digraph.")
  (test-error (in-degree test-graph :c) "Input graph `g` must be a valid digraph."))

(deftest-g "in-degree, none"
  ((def test-graph (defgraph))
  (make-digraph! test-graph))
  (add-nodes test-graph :a)
  (test (in-degree test-graph :a) 0))

(deftest-g "in-degree"
  ((def test-graph (defgraph))
  (make-digraph! test-graph))
  (add-edges test-graph [:a :b] [:b :c] [:a :c])
  (test (in-degree test-graph :a) 0)
  (test (in-degree test-graph :b) 1)
  (test (in-degree test-graph :c) 2))

(deftest-g "edges, fails if graph `g` is not a valid graph"
  (test-error (edges :fails) "Argument to `edges` must be a valid graph."))

(deftest-g "edges, none"
  (def test-graph (defgraph))
  (add-nodes test-graph :a :b)
  (test (edges test-graph) @[]))

(deftest-g "edges"
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

(deftest-g "add-edges, fails if bad graph"
  (test-error (add-edges :fails [:a :b]) "First argument to `add-edges` must be a valid graph."))

(deftest-g "add-edges, fails if bad edges" 
  (def test-graph (defgraph))
  (test-error (add-edges test-graph :fails) "All edges passed to `add-edges` must be valid edges."))

(deftest-g "add-edges, simple"
  (def test-graph (defgraph))
  (add-edges test-graph [:a :b] [:a :c])
  (test (metadata test-graph) @{:graph true})
  (test test-graph @{:adj @{:a @{:b true :c true} :b @{:a true} :c @{:a true}} :attrs @{} :in @{} :metadata @{:graph true} :nodeset @{:a true :b true :c true}}))

(deftest-g "add-edges, digraph"
  (def test-digraph (defgraph))
  (make-digraph! test-digraph)
  (add-edges test-digraph [:a :b] [:a :c])
  (test (metadata test-digraph) @{:digraph true :graph true})
  (test test-digraph @{:adj @{:a @{:b true :c true}} :attrs @{} :in @{:b @{:a true} :c @{:a true}} :metadata @{:digraph true :graph true} :nodeset @{:a true :b true :c true}}))

(deftest-g "add-edges, weighted, fails if bad edges"
  (def test-weighted-graph (defgraph))
  (make-weighted! test-weighted-graph)
  (test (metadata test-weighted-graph) @{:graph true :weighted true})
  (test-error (add-edges test-weighted-graph [:a :b] [:a :c]) "All edges passed to `add-edges` with a weighted graph must be valid weighted edges."))

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
  (test-error (add-edges test-weighted-digraph [:a :b] [:a :c]) "All edges passed to `add-edges` with a weighted graph must be valid weighted edges."))

(deftest "add-edges, weighted and directional"
  (def test-weighted-digraph (defgraph))
  (make-digraph! test-weighted-digraph)
  (make-weighted! test-weighted-digraph)
  (test (metadata test-weighted-digraph) @{:digraph true :graph true :weighted true})
  (test (add-edges test-weighted-digraph [:a :b 50] [:a :c 100]) @{:adj @{:a @{:b 50 :c 100}} :attrs @{} :in @{:b @{:a 50} :c @{:a 100}} :metadata @{:digraph true :graph true :weighted true} :nodeset @{:a true :b true :c true}}))

(deftest final-time
  (print "Elapsed time: " (- (os/clock) start) " seconds"))