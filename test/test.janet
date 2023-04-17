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
  (test (node-schema @[:a :b])          true)
  (test (node-schema @[:a :b :c])       false)
  (test (node-schema {:x 1 :y 2})       nil)
  (test (node-schema @["here" "there"]) true)
  (test (node-schema @[:a @[:c :d]])    true))

(deftest-g "edge-schema"
  (test (edge-schema @[:a :b])          true)
  (test (edge-schema @[:a :b :c])       false)
  (test (edge-schema {:x 1 :y 2})       nil)
  (test (edge-schema @["here" "there"]) true)
  (test (edge-schema @[:a @[:c :d]])    true))

(deftest-g "weighted-edge-schema"
  (test (weighted-edge-schema @[:a :b])             nil)
  (test (weighted-edge-schema @[:a :b :c])          false)
  (test (weighted-edge-schema @[:a :b 1])           true)
  (test (weighted-edge-schema {:x 1 :y 2})          nil)
  (test (weighted-edge-schema @["here" "there"])    nil)
  (test (weighted-edge-schema @["here" "there" 50]) true)
  (test (weighted-edge-schema @[:a @[:c :d]])       nil))

(deftest-g "graph?"
  (test (graph? Graph)  true)
  (def test-graph (defgraph))
  (test (graph? test-graph) true))

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
  (test test-graph-1 @{:adj @{ :a @{:b 0} :b @{:a 0 :c 0 :d 0} :c @{:b 0} :d @{:b 0}} :attrs @{} :in @{} :metadata @{:graph true :weighted true} :nodeset @{ :a true :b true :c true :d true}})) 

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
  (test test-graph-1 @{:adj @{:a @{:b 0} :b @{:c 0 :d 0}} :attrs @{} :in @{:b @{:a 0} :c @{:b 0} :d @{:b 0}} :metadata @{:digraph true :graph true :weighted true} :nodeset @{ :a true :b true :c true :d true}}))

(deftest-g "make-weighted!, existing unweighted digraph, weight supplied"
  (def test-graph-1 (defgraph))
  (make-digraph! test-graph-1)
  (add-edges test-graph-1 [:a :b] [:b :c] [:b :d])
  (make-weighted! test-graph-1 15) 
  (test test-graph-1 @{:adj @{:a @{:b 15} :b @{:c 15 :d 15}} :attrs @{} :in @{:b @{:a 15} :c @{:b 15} :d @{:b 15}} :metadata @{:digraph true :graph true :weighted true} :nodeset @{ :a true :b true :c true :d true}}))

(deftest-g "node?"
  (test (node? @[:a :b])          true)
  (test (node? @[:a :b :c])       false)
  (test (node? {:x 1 :y 2})       nil)
  (test (node? @["here" "there"]) true)
  (test (node? @[:a @[:c :d]])    true)
  (test (node? @[:a :b 50])       true)
  (test (node? @[:a :b :c])       false))

(deftest-g "digraph?"
  (def test-graph (defgraph))
  (test ((metadata test-graph) :digraph) nil) 
  (make-digraph! test-graph)
  (test ((metadata test-graph) :digraph) true)
  (test (digraph? test-graph)            true))

(deftest-g "weighted?"
  (def test-graph (defgraph))
  (test ((metadata test-graph) :weighted) nil)
  (make-weighted! test-graph)
  (test ((metadata test-graph) :weighted) true)
  (test (weighted? test-graph)            true))

(deftest-g "nodes"
  (test (nodes Graph) @{})
  (def test-graph (defgraph))
  (test (nodes test-graph) @{}))

(deftest-g "add-nodes"
  (def test-graph (defgraph))
  (add-nodes test-graph :a :b :c 1 2 3 ["a" "b"] "c")
  (test test-graph @{:adj @{} 
                     :attrs @{} 
                     :in @{} 
                     :metadata @{:graph true} 
                     :nodeset @{ 1 true 2 true 3 true "c" true :a true :b true :c true ["a" "b"] true}}))

(deftest-g "member-node?"
  (def test-graph (defgraph))
  (add-nodes test-graph :a :b :c 1 2 3 ["a" "b"] "c")
  (test test-graph @{:adj @{} 
                     :attrs @{} 
                     :in @{} 
                     :metadata @{:graph true} 
                     :nodeset @{ 1 true 2 true 3 true "c" true :a true :b true :c true ["a" "b"] true}})
  (test (member-node? test-graph ["a" "b"]) true)
  (test (member-node? test-graph :d) false))

(deftest-g "out-edges")

(deftest-g "edges")

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

(deftest "successors"
  (def test-graph (defgraph))
  (add-nodes test-graph [:a :b :c])
  (add-edges test-graph [:a :b] [:a :c]))

(deftest final-time
  (print "Elapsed time: " (- (os/clock) start) " seconds"))