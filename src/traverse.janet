(use judge)
(use ./dataclasses)

(dataclass- Pair
  :key :array
  :val :array
  {:k (fn [self] (get-in self [:key 0]))
   :v (fn [self] (get-in self [:val 0]))
   :set-k (fn [self f] (update-in self [:key 0] f))
   :set-v (fn [self f] (update-in self [:val 0] f))})

(defn- struct/setproto [s proto]
  (struct/with-proto proto ;(kvs s)))

(defn traverse
  ``
  Iterate over the values in an ast and apply `f`
  to them. Collect the results in a data structure. 
  If the ast is not a table, struct, array, or tuple,
  returns the form.

  Compare with Clojure's `walk` function.
  ``
  [f form]
  (cond
    (and (pair? form) (not (form :seen)))
    (do
      (update-in form [:key 0] f)
      (update-in form [:val 0] f)
      (put form :seen true)
      form)

    (= :pair (first form))
    (f (pair :key @[(get-in form [1 0])]
             :val @[(get-in form [1 1])]))

    (dictionary? form)
    ((if (struct? form) struct/setproto table/setproto)
      (tabseq [p :pairs form
               :let [p+ (traverse f [:pair p])]]
        (:k p+) (:v p+))
      ((if (struct? form) struct/getproto table/getproto) form))

    (indexed? form)
    ((if (tuple? form) tuple array)
      ;(map f form))

    form))

(def pair? pair?)

(defn pretraverse [f form]
  (traverse |(pretraverse f $) (f form)))

(defn posttraverse [f form]
  (f (traverse |(posttraverse f $) form)))

#########

(test-stdout (traverse |(do (pp $) $) {:a 1}) `
  @Pair{:key @[:a] :val @[1]}
`
  {:a 1})
(test (traverse |(if (= (type $) :number) (inc $) $) {:a 1})
      {:a 1})

(test-stdout (traverse |(if (pair? $) (do (pp $) $) $) {:a 1}) `
  @Pair{:key @[:a] :val @[1]}
`
  {:a 1})

(test-stdout (posttraverse |(if (pair? $) (do (pp $) $) $) {:a 1}) `
  @Pair{:key @[:a] :seen true :val @[1]}
`
  {:a 1})
(test-stdout (pretraverse |(if (pair? $) (do (pp $) $) $) {:a 1}) `
  @Pair{:key @[:a] :val @[1]}
`
  {:a 1})

(deftest "pretraverse"
  (test-stdout (pretraverse |(do (pp $) $) {:a 1 :b 2}) `
    {:a 1 :b 2}
    @Pair{:key @[:b] :val @[2]}
    :b
    2
    @Pair{:key @[:a] :val @[1]}
    :a
    1
  `
    {:a 1 :b 2}))

(deftest "posttraverse"
  (test-stdout (posttraverse |(do (pp $) $) {:a 1 :b 2}) `
    :b
    2
    @Pair{:key @[:b] :seen true :val @[2]}
    :a
    1
    @Pair{:key @[:a] :seen true :val @[1]}
    {:a 1 :b 2}
  `
    {:a 1 :b 2}))

(deftest "pretraverse"
  (test-stdout (pretraverse |(do (pp $) $) {:a 1 :b {:c {:d 2}}}) `
    {:a 1 :b {:c {:d 2}}}
    @Pair{:key @[:b] :val @[{:c {:d 2}}]}
    :b
    {:c {:d 2}}
    @Pair{:key @[:c] :val @[{:d 2}]}
    :c
    {:d 2}
    @Pair{:key @[:d] :val @[2]}
    :d
    2
    @Pair{:key @[:a] :val @[1]}
    :a
    1
  `
    {:a 1 :b {:c {:d 2}}}))

(deftest "posttraverse"
  (test-stdout (posttraverse |(do (pp $) $) {:a 1 :b {:c {:d 2}}}) `
    :b
    :c
    :d
    2
    @Pair{:key @[:d] :seen true :val @[2]}
    {:d 2}
    @Pair{:key @[:c] :seen true :val @[{:d 2}]}
    {:c {:d 2}}
    @Pair{:key @[:b] :seen true :val @[{:c {:d 2}}]}
    :a
    1
    @Pair{:key @[:a] :seen true :val @[1]}
    {:a 1 :b {:c {:d 2}}}
  `
    {:a 1 :b {:c {:d 2}}}))

(deftest "pretraverse"
  (test-stdout (pretraverse |(do (pp $) $) {:a 1 :b {:c 1 :d {:e 1 :f 2}}}) `
    {:a 1 :b {:c 1 :d {:e 1 :f 2}}}
    @Pair{:key @[:b] :val @[{:c 1 :d {:e 1 :f 2}}]}
    :b
    {:c 1 :d {:e 1 :f 2}}
    @Pair{:key @[:c] :val @[1]}
    :c
    1
    @Pair{:key @[:d] :val @[{:e 1 :f 2}]}
    :d
    {:e 1 :f 2}
    @Pair{:key @[:e] :val @[1]}
    :e
    1
    @Pair{:key @[:f] :val @[2]}
    :f
    2
    @Pair{:key @[:a] :val @[1]}
    :a
    1
  `
    {:a 1 :b {:c 1 :d {:e 1 :f 2}}}))

(deftest "posttraverse"
  (test-stdout (posttraverse |(do (pp $) $) {:a 1 :b {:c 1 :d {:e 1 :f 2}}}) `
    :b
    :c
    1
    @Pair{:key @[:c] :seen true :val @[1]}
    :d
    :e
    1
    @Pair{:key @[:e] :seen true :val @[1]}
    :f
    2
    @Pair{:key @[:f] :seen true :val @[2]}
    {:e 1 :f 2}
    @Pair{:key @[:d] :seen true :val @[{:e 1 :f 2}]}
    {:c 1 :d {:e 1 :f 2}}
    @Pair{:key @[:b] :seen true :val @[{:c 1 :d {:e 1 :f 2}}]}
    :a
    1
    @Pair{:key @[:a] :seen true :val @[1]}
    {:a 1 :b {:c 1 :d {:e 1 :f 2}}}
  `
    {:a 1 :b {:c 1 :d {:e 1 :f 2}}}))

(deftest "commplex heterogeneous structure"
  (def heterogeneous-files
    [{:type :file
      :accessed "2024-11-11"}
     {:type :file
      :name "file.name"
      :properties [{:accessed "2022-02-02"}
                   {:tags [:foo :bar :baz]}]}
     [:file {:name "path.txt"
             :accessed "2021-01-01"}]])

  (test-stdout (posttraverse
                 |(if (pair? $) (do (pp $) $) $)
                 heterogeneous-files) `
    @Pair{:key @[:accessed] :seen true :val @["2024-11-11"]}
    @Pair{:key @[:type] :seen true :val @[:file]}
    @Pair{:key @[:name] :seen true :val @["file.name"]}
    @Pair{:key @[:accessed] :seen true :val @["2022-02-02"]}
    @Pair{:key @[:tags] :seen true :val @[(:foo :bar :baz)]}
    @Pair{:key @[:properties] :seen true :val @[({:accessed "2022-02-02"} {:tags (:foo :bar :baz)})]}
    @Pair{:key @[:type] :seen true :val @[:file]}
    @Pair{:key @[:name] :seen true :val @["path.txt"]}
    @Pair{:key @[:accessed] :seen true :val @["2021-01-01"]}
  `
    [{:accessed "2024-11-11" :type :file}
     {:name "file.name"
      :properties [{:accessed "2022-02-02"}
                   {:tags [:foo :bar :baz]}]
      :type :file}
     [:file
      {:accessed "2021-01-01"
       :name "path.txt"}]]))

(deftest "commplex heterogeneous structure"
  (def heterogeneous-files
    [{:type :file
      :accessed "2024-11-11"}
     {:type :file
      :name "file.name"
      :properties [{:accessed "2022-02-02"}
                   {:tags [:foo :bar :baz]}]}
     [:file {:name "path.txt"
             :accessed "2021-01-01"}]
     [[:accessed "shouldn't change"]
      {:accessed "should change"}]])

  (test (posttraverse
          |(if (and (pair? $)
                    (= (($ :key) 0) :accessed))
             (pair :key @[:accessed]
                   :val @["2025-10-15 (this worked)"])
             $)
          heterogeneous-files)
        [{:accessed "2025-10-15 (this worked)"
          :type :file}
         {:name "file.name"
          :properties [{:accessed "2025-10-15 (this worked)"}
                       {:tags [:foo :bar :baz]}]
          :type :file}
         [:file
          {:accessed "2025-10-15 (this worked)"
           :name "path.txt"}]
         [[:accessed "shouldn't change"]
          {:accessed "2025-10-15 (this worked)"}]]))

(deftest "posttraverse with length-two tuples"
  (def structure
    {:a [1 2]
     :b {:c [:a 2] :d [2 2] :e 3 4 5}})
  (test (posttraverse
          |(if (and
                 (= :tuple (type $))
                 (= 2 (length $))
                 (= :number (type ($ 0))))
             [(inc ($ 0)) ($ 1)]
             $)
          structure)
        {:a [2 2] :b {:c [:a 2] :d [3 2] :e 3 4 5}}))
