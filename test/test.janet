(use judge) 
(use /src/jgraph)

(def start (os/clock))
 
(deftest graph-schema
  (test (graph-schema Graph) true)
  (def Graph-dup
    (put (table/clone Graph) :in nil))
  (test (graph-schema Graph-dup) nil))

(deftest node-schema
  (test (node-schema @[:a :b])          true)
  (test (node-schema @[:a :b :c])       false)
  (test (node-schema {:x 1 :y 2})       nil)
  (test (node-schema @["here" "there"]) true)
  (test (node-schema @[:a @[:c :d]])    true))

(deftest metadata
  (defgraph "agraph")
  (test (metadata agraph) @{:graph true}))

(deftest defgraph
  (defgraph "bgraph")
  (test (dyn 'bgraph) @{:graph true :value @{}})
  (test (table/proto-flatten (dyn 'bgraph)) 
        @{:adj @{} :attrs @{} :graph true :in @{} :nodeset @[] :value @{}}))

(deftest make-digraph! 
  (defgraph "bgraph")
  (test ((metadata bgraph) :digraph) nil)
  (make-digraph! bgraph)
  (test ((metadata bgraph) :digraph) true))

(deftest make-weighted! 
  (defgraph "cgraph")
  (test ((metadata cgraph) :weighted) nil)
  (make-weighted! cgraph)
  (test ((metadata cgraph) :weighted) true))

(deftest graph?
  (test (graph? Graph)  true)
  (defgraph "dgraph")
  (test (graph? dgraph) true))

(deftest node?
  (test (node? @[:a :b])          true)
  (test (node? @[:a :b :c])       false)
  (test (node? {:x 1 :y 2})       nil)
  (test (node? @["here" "there"]) true)
  (test (node? @[:a @[:c :d]])    true))

(deftest digraph?
  (defgraph "egraph")
  (test ((metadata egraph) :digraph) nil)
  (make-digraph! egraph)
  (test ((metadata egraph) :digraph) true)
  (test (digraph? egraph)            true))

(deftest weighted? 
  (defgraph "fgraph")
  (test ((metadata fgraph) :weighted) nil)
  (make-weighted! fgraph)
  (test ((metadata fgraph) :weighted) true)
  (test (weighted? fgraph)            true))

(deftest nodes
  (test (nodes Graph)  @[])
  (defgraph "hgraph")
  (test (nodes hgraph) @[]))

(deftest final-time
  (print "Elapsed time: " (- (os/clock) start) " seconds"))