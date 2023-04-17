(use judge) 
(use /src/jgraph)

(def start (os/clock))
 
(deftest schema
  (test (graph-schema Graph) true)
  (def Graph-dup (table/clone Graph))
  (put Graph-dup :in nil)
  (test (graph-schema Graph-dup) nil))

(deftest metadata
  (defgraph "fgraph")
  (test (metadata fgraph) @{:graph true}))

(deftest make-digraph! 
  (defgraph "agraph")
  (test ((metadata agraph) :digraph) nil)
  (make-digraph! agraph)
  (test ((metadata agraph) :digraph) true))

(deftest make-weighted! 
  (defgraph "egraph")
  (test ((metadata egraph) :weighted) nil)
  (make-weighted! egraph)
  (test ((metadata egraph) :weighted) true))

(deftest graph?
  (test (graph? Graph)  true)
  (defgraph "cgraph")
  (test (graph? cgraph) true))

(deftest digraph?
  (defgraph "bgraph")
  (test ((metadata bgraph) :digraph) nil)
  (make-digraph! bgraph)
  (test ((metadata bgraph) :digraph) true)
  (test (digraph? bgraph)            true))

(deftest weighted? 
  (defgraph "dgraph")
  (test ((metadata dgraph) :weighted) nil)
  (make-weighted! dgraph)
  (test ((metadata dgraph) :weighted) true)
  (test (weighted? dgraph)            true))

(deftest final-time
  (print "Elapsed time: " (- (os/clock) start) " seconds"))