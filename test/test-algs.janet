(use judge)
(use /src/algs)
(use /test/test-utils)

(use /src/graphs)

(assert-docs "/src/algs")

(def start (os/clock))

(deftest "remove"
  (test (remove :a @[:a :b :c]) @[:b :c])
  (test (remove @[:a :b] @[:a :b :c]) @[:c])
  (test (remove even? @[1 2 3 4 5]) @[1 3 5])
  (test (remove odd? @[1 2 3 4 5]) @[2 4]))

(deftest "iterate"
  (test (take 5 (iterate inc 0)) @[0 1 2 3 4])
  (test (take-while identity (iterate |(get $ :a) {:a {:a {:a {:a 10}}}})) @[{:a {:a {:a {:a 10}}}} {:a {:a {:a 10}}} {:a {:a 10}} {:a 10} 10])
  (test (take 4 (iterate |(/ $ 2) 1000)) @[1000 500 250 125])
  (test (take 4 (iterate |(drop 2 $) "Hello, world")) @["Hello, world" "llo, world" "o, world" " world"]) 
  (test (take 4 (iterate |@[;(array/slice $ 1)] @[1 2 3 4 5])) @[@[1 2 3 4 5] @[2 3 4 5] @[3 4 5] @[4 5]])
  (test (take 4 (iterate |@[;$ "a"] @[])) @[@[] @["a"] @["a" "a"] @["a" "a" "a"]]))

(deftest "trace-paths"
  (test (trace-paths {:a []} :a) @[@[:a]])
  (test (paths       {:a [:b] :b []} [:a]) @[@[:a] @[:a :b]])
  (test (trace-paths {:a [:b] :b []} :a) @[@[:a :b]])
  (test (paths       {:a [:b :c] :b [:d] :c [:d] :d []} [:a]) @[@[:a] @[:a :b] @[:a :b :d] @[:a :c] @[:a :c :d]])
  (test (trace-paths {:a [:b :c] :b [:d] :c [:d] :d []} :a)   @[@[:a :b :d] @[:a :c :d]]))

(deftest "preds->span"
  (test (preds->span {:a :b :c :b}) @{:b [:c :a]}))

(deftest "topsort-dfs 1"
  (def dg (digraph {:a [:b :c] :b [:c :d] :c [:d :e] :d [] :e []}))
  (test (topsort-dfs dg) @[:a :b :c :e :d]))

(deftest "topsort-dfs 2"
  (def dg (digraph {:a [:b :c :d] :b [:d] :c [:d :e]}))
  (test (topsort-dfs dg) @[:a :c :e :b :d]))

(deftest "topsort-dfs 3"
  (def dg (digraph {:a [:b :c] :b [:d :e] :c [:d :f]}))
  (test (topsort-dfs dg) @[:a :c :f :b :e :d]))

(deftest final-time 
  (print "Elapsed time: " (- (os/clock) start) " seconds"))