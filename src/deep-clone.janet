(use judge)
(use ./traverse)

(defn deep-clone [ds]

  (defn decision-tree [ds]
    (cond
      (table? ds) (table/clone ds)
      (array? ds) (array/slice ds)
      (buffer? ds) (buffer/slice ds)
      ds))

  (pretraverse decision-tree ds))

(deftest "deep-clone"

  (def tab1 @{:a @{:b @[1 2 3]}})
  (def tab2 tab1)

  (test (= tab1 tab2) true)
  (test (deep= tab1 tab2) true)
  (test (= (tab1 :a) (tab2 :a)) true)
  (test (= (get-in tab1 [:a :b])
           (get-in tab2 [:a :b]))
        true)

  (def tab3 (table/clone tab1))

  (test (= tab1 tab3) false)
  (test (deep= tab1 tab3) true)
  (test (= (tab1 :a) (tab3 :a)) true)
  (test (= (get-in tab1 [:a :b])
           (get-in tab3 [:a :b]))
        true)

  (def tab4 (deep-clone tab1))

  (test (= tab1 tab4) false)
  (test (deep= tab1 tab4) true)
  (test (= (tab1 :a) (tab4 :a)) false)
  (test (= (get-in tab1 [:a :b])
           (get-in tab4 [:a :b]))
        false))
