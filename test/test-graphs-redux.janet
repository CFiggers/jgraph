(use judge)
(use ../src/graphs-redux)

# Nodes 

(deftest "node"
  :should "Declare nodes with vals"
  (test (node) @{})
  (test (node :val @[:a]) @{:val @[:a]}))

(deftest "node methods-1"
  :should "Be able to invoke :get and :v methods on nodes"
  (def n (node :val @[:a]))
  (test (:get n) :a)
  (test (:v n) :a))

(deftest "node methods-2"
  :should "Be able to set :val using :set method on nodes"
  (def n (node :val @[:a]))
  (:set n :b)
  (test n @{:val @[:b]}))

# Edges 

## Edge

(deftest "edge"
  :should "Declare edges with :from and :to nodes"
  (test (edge) @{})
  (test (edge :from (node :val @[:a])
              :to (node :val @[:b]))
        @{:from @{:val @[:a]} :to @{:val @[:b]}}))

(deftest "edge methods"
  :should "Be able to get :from and :to with :f and :t methods on edges"
  (def e (edge :from (node :val @[:a])
               :to (node :val @[:b])))
  (test (:f e) @{:val @[:a]})
  (test (:t e) @{:val @[:b]}))

## Weighted-Edge 

(deftest "weighted-edge"
  :should "Declare weighted edges with :from, :to, and :weight"
  (test (weighted-edge) @{})
  (test (weighted-edge :from (node :val @[:a])
                       :to (node :val @[:b])
                       :weight 100)
    @{:from @{:val @[:a]}
      :to @{:val @[:b]}
      :weight 100}))

(deftest "weighted-edge methods"
  :should "Be able to get :from and :to with :f and :t methods on weighted-edges"
  :should "Be able to get :weight with :w method on weighted-edges"
  (def we (weighted-edge :from (node :val @[:a])
                         :to (node :val @[:b])
                         :weight 100))
  (test (:f we) @{:val @[:a]})
  (test (:t we) @{:val @[:b]})
  (test (:w we) 100))

## Attr-Edge 

(deftest "attr-edge"
  :should "Declare attr edges with :from, :to, and :attrs"
  (test (attr-edge) @{})
  (test (attr-edge :from (node :val @[:a])
                   :to (node :val @[:b])
                   :attrs @{:a true})
    @{:attrs @{:a true}
      :from @{:val @[:a]}
      :to @{:val @[:b]}}))

(deftest "attr-edge methods"
  :should "Be able to get :from and :to with :f and :t methods on attr-edges"
  :should "Be able to get :attrs with :as method on attr-edges"
  (def ae (attr-edge :from (node :val @[:a])
                     :to (node :val @[:b])
                     :attrs @{:a true}))
  (test (:f ae) @{:val @[:a]})
  (test (:t ae) @{:val @[:b]})
  (test (:as ae) @{:a true}))

## Weighted-Attr-Edge

(deftest "weighted-attr-edge"
  :should "Declare weighted attr edges with :from, :to, :weight, and :attrs"
  (test (weighted-attr-edge) @{})
  (test (weighted-attr-edge :from (node :val @[:a])
                            :to (node :val @[:b])
                            :weight 100
                            :attrs @{:a true})
    @{:attrs @{:a true}
      :from @{:val @[:a]}
      :to @{:val @[:b]}
      :weight 100}))

(deftest "weighted-attr-edge methods"
  :should "Be able to get :from and :to with :f and :t methods on weighted-attr-edges"
  :should "Be able to get :attrs with :as method on weighted-attr-edges"
  :should "Be able to get :weight with :w method on weighted-attr-edges"
  (def wae (weighted-attr-edge :from (node :val @[:a])
                               :to (node :val @[:b])
                               :weight 100
                               :attrs @{:a true}))
  (test (:f wae) @{:val @[:a]})
  (test (:t wae) @{:val @[:b]})
  (test (:as wae) @{:a true})
  (test (:w wae) 100))

# Graphs 

## Graph 



## Digraph 



## Weighted-Graph



## Attr-Graph



## Weighted-Digraph 



## Weighted-Attr-Graph



## Attr-Digraph



## Weighted-Attr-Digraph


# Add Archetypes

## Add Digraph



## Add Attr to Edge 



## Add Attr 



## Add Weighted to Edge 



## Add Weighted



# Functions 

