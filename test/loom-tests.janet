(use judge)
(use /src/jgraph)

(def start (os/clock))

(deftest "loom-build-graph-test"
  (let [g1 (graph [1 2] [1 3] [2 3] 4)
        g2 (graph {1 {2 true 3 true} 2 {3 true} 4 {}})
        g3 (graph {1 [2 3] 2 [3] 4 []})
        g4 (graph g1)
        g5 (graph g3 (digraph [5 6]) [7 8] 9)
        g6 (graph)]
    (test (nodes g1) @[1 2 3 4])
    (test (edges g1) @[[1 2] [1 3] [2 1] [2 3] [3 1] [3 2]])
    (test (nodes g2) @[4 1 2 3])
    (test (edges g2) @[[1 2] [1 3] [2 1] [2 3] [3 1] [3 2]])
    (test (nodes g3) @[4 1 2 3])
    (test (edges g3) @[[1 2] [1 3] [2 1] [2 3] [3 1] [3 2]]) 
    (test (nodes g4) @[1 2 3 4])
    (test (edges g4) @[[1 2] [1 3] [2 1] [2 3] [3 1] [3 2]])
    (test (nodes g5) @[2 8 3 4 1 9 5 6 7])
    (test (edges g5) @[[2 1] [2 3] [8 7] [3 1] [3 2] [1 2] [1 3] [5 6] [6 5] [7 8]])
    (test (nodes g6) @[])
    (test (edges g6) @[])
    (test (has-node? g1 4) true)
    (test (has-edge? g1 [1 2]) true)
    (test (has-node? g1 5) false)
    (test (has-edge? g1 [4 1]) false)))

(deftest "loom-simple-digraph-test"
  (let [g1 (digraph [1 2] [1 3] [2 3] 4)
        g2 (digraph {1 {2 true 3 true} 2 {3 true} 4 {}})
        g3 (graph {1 [2 3] 2 [3] 4 []})
        g4 (digraph g1)
        g5 (digraph g3 (graph [5 6]) [7 8] 9)
        g6 (digraph)
        g7 (transpose g1)]
    (test (nodes g1) @[1 2 3 4])
    (test (edges g1) @[[1 2] [1 3] [2 3]])
    (test (nodes g2) @[4 1 2 3])
    (test (edges g2) @[[1 2] [1 3] [2 3]])
    (test (nodes g3) @[4 1 2 3])
    (test (edges g3) @[[1 2] [1 3] [2 1] [2 3] [3 1] [3 2]])
    (test (nodes g4) @[1 2 3 4])
    (test (edges g4) @[[1 2] [1 3] [2 3]])
    (test (nodes g5) @[2 8 3 4 1 9 5 6 7])
    (test (edges g5) @[[2 1] [2 3] [3 1] [3 2] [1 2] [1 3] [5 6] [6 5] [7 8]])
    (test (nodes g6) @[])
    (test (edges g6) @[])
    (test (nodes g7) @[1 2 3 4])
    (test (edges g7) @[[2 1] [3 1] [3 2]])
    (test (has-node? g1 4) true)
    (test (has-edge? g1 [1 2]) true)
    (test (has-node? g1 5) false)
    (test (has-edge? g1 [2 1]) false)))                                          

(deftest "loom-simple-weighted-graph-test"
  (let [g1 (weighted-graph [1 2 77] [1 3 88] [2 3 99] 4)
        g2 (weighted-graph {1 {2 77 3 88} 2 {3 99} 4 {}})
        g3 (graph {1 [2 3] 2 [3] 4 []})
        g4 (weighted-graph g1)
        g5 (weighted-graph g3 (weighted-digraph [5 6 88]) [7 8 1] 9)
        g6 (weighted-graph)]
    (test (nodes g1) @[1 2 3 4])
    (test (edges g1) @[[1 2] [1 3] [2 1] [2 3] [3 1] [3 2]])
    (test (nodes g2) @[4 1 2 3])
    (test (edges g2) @[[1 2] [1 3] [2 1] [2 3] [3 1] [3 2]])
    (test (nodes g3) @[4 1 2 3])
    (test (edges g3) @[[1 2] [1 3] [2 1] [2 3] [3 1] [3 2]])
    (test (nodes g4) @[1 2 3 4])
    (test (edges g4) @[[1 2] [1 3] [2 1] [2 3] [3 1] [3 2]])
    (test (nodes g5) @[2 8 3 4 1 9 5 6 7])
    (test (edges g5) @[[2 1] [2 3] [8 7] [3 1] [3 2] [1 2] [1 3] [5 6] [6 5] [7 8]])
    (test (nodes g6) @[])
    (test (edges g6) @[])
    (test (has-node? g1 4) true)
    (test (has-edge? g1 [1 2]) true)
    (test (has-node? g1 5) false)
    (test (has-edge? g1 [2 1]) true)))

(deftest "loom-simple-weighted-digraph-test"
  (let [g1 (weighted-digraph [1 2 77] [1 3 88] [2 3 99] 4)
        g2 (weighted-digraph {1 {2 77 3 88} 2 {3 99} 4 {}})
        g3 (graph {1 [2 3] 2 [3] 4 []})
        g4 (weighted-digraph g1)
        g5 (weighted-digraph g3 (weighted-graph [5 6 88]) [7 8 1] 9)
        g6 (weighted-digraph)
        g7 (transpose g1)]
    (test (nodes g1) @[1 2 3 4])
    (test (edges g1) @[[1 2] [1 3] [2 3]])
    (test (nodes g2) @[4 1 2 3])
    (test (edges g2) @[[1 2] [1 3] [2 3]])
    (test (nodes g3) @[4 1 2 3])
    (test (edges g3) @[[1 2] [1 3] [2 1] [2 3] [3 1] [3 2]])
    (test (nodes g4) @[1 2 3 4])
    (test (edges g4) @[[1 2] [1 3] [2 3]])
    (test (nodes g5) @[2 8 3 4 1 9 5 6 7])
    (test (edges g5) @[[2 1] [2 3] [3 1] [3 2] [1 2] [1 3] [5 6] [6 5] [7 8]])
    (test (nodes g6) @[])
    (test (edges g6) @[])
    (test (nodes g7) @[1 2 3 4])
    (test (edges g7) @[[2 1] [3 1] [3 2]])
    (test (has-node? g1 4) true)
    (test (has-edge? g1 [1 2]) true)
    (test (has-node? g1 5) false)
    (test (has-edge? g1 [2 1]) false)))                                          

(deftest final-time 
  (print "Elapsed time: " (- (os/clock) start) " seconds"))