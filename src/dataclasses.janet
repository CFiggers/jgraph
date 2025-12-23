(use judge)
(import spork/schema)

(defn- capitalize [str]
  (string (string/ascii-upper (string/slice str 0 1))
          (string/ascii-lower (string/slice str 1))))

(defn- capitalized? [str]
  (let [a (string/from-bytes (first str))]
    (= a (string/ascii-upper a))))

(defn- interpret-type
  [match what]
  (if (capitalized? match)
    ~(,what (pred ,(symbol (string (string/ascii-lower match) "?"))))
    ~(,what ,(keyword match))))

(test (interpret-type "Node" keys) [@keys [pred node?]])

(test (peg/match
        ~{:k? (? (/ (* "k:" '(to (+ ":" -1)))
                    ,|(interpret-type $ 'keys)))
          :v? (? (/ (* "v:" '(to -1))
                    ,|(interpret-type $ 'values)))
          :main (* "table:" :k? (? ":") :v?)}
        :table:k:keyword:v:boolean)
      @[[keys :keyword] [values :boolean]])

(test (peg/match
        ~{:k? (? (/ (* "k:" '(to (+ ":" -1)))
                    ,|(interpret-type $ 'keys)))
          :v? (? (/ (* "v:" '(to -1))
                    ,|(interpret-type $ 'values)))
          :main (* "table:" :k? (? ":") :v?)}
        :table:k:Node:v:boolean)
      @[[keys [pred node?]] [values :boolean]])

(defn interpret-props [chopped-props]
  (tabseq [[n t] :in (partition 2 chopped-props)]
    n (cond
        (symbol? t)
        [~(pred ,(symbol (string (string/ascii-lower t)) "?")) n (symbol t "-Nil")]

        (and (indexed? t) (keyword? (first t)))
        [(t 0) n (t 1)]

        (string/has-prefix? "array:" t)
        [~(and :array (values (pred ,(symbol (string (string/ascii-lower (string/slice t 6)) "?"))))) n @[]]

        (string/has-prefix? "table:" t)
        [~(and :table
               ,;(peg/match
                   ~{:k? (? (/ (* "k:" '(to (+ ":" -1)))
                               ,|(interpret-type $ 'keys)))
                     :v? (? (/ (* "v:" '(to -1))
                               ,|(interpret-type $ 'values)))
                     :main (* "table:" :k? (? ":") :v?)}
                   t))
         n
         @{}]

        (case t
          :string [:string n ""]
          :buffer [:buffer n @""]
          :array [:array n @[]]
          :tuple [:tuple n []]
          :struct [:struct n {}]
          :table [:table n @{}]
          :number [:number n 0]
          :keyword [:keyword n :nothing]
          :boolean [:boolean n true]))))

(defn- dataclass* [name private & props]

  # Make names
  (def $Name (string/join (map capitalize (string/split "-" name)) "-"))
  (def $name (string/ascii-lower name))
  (def $assert-name (string "assert-" $name))
  (def $name? (string $name "?"))

  # Interpret params
  (def [chopped-props methods]
    (if (dictionary? (last props))
      [(tuple/slice props 0 -2)
       (last props)]
      [props {}]))

  (def interpreted-props (interpret-props chopped-props))

  (def interpreted-methods
    (seq [[k v] :pairs methods]
      [:method k v]))

  (defn any-proto? [ds proto]
    (cond
      (= (getproto ds) proto) true
      (getproto ds) (any-proto? (getproto ds) proto)
      false))

  (def schema-def
    ~(and (or :table :struct)
          (props
            :type :keyword
            :_name :keyword
            ,;(catseq [[t n _] :in (values interpreted-props)] [n t]))
          (pred |(,any-proto? $ ,(symbol $Name)))))

  # Make definitions
  ~(upscope
     (,(if private 'def- 'def)
       ,(symbol $Name)
       (merge-into ,(tabseq [[_ n v] :in [;(values interpreted-props) ;interpreted-methods]] n v)
                   {:type (keyword ,$Name)
                    :_name (keyword ,$Name)
                    :schema ,(tabseq [[t n _] :in [;(values interpreted-props) ;interpreted-methods]] n (if (= :tuple (type t)) ~(quote ,t) t))}))

     (,(if private 'def- 'def)
       ,(symbol $assert-name)
       (as-macro ,schema/validator ,schema-def))

     (,(if private 'def- 'def)
       ,(symbol $name?)
       (as-macro ,schema/predicate ,schema-def))

     (,(if private 'defn- 'defn)
       ,(symbol $name) [&named ,;(map |(symbol ($ 1)) (values interpreted-props))]
       (,(symbol $assert-name) (table/setproto
                                 ,(tabseq [[_ n _] :in (values interpreted-props)] n (symbol n))
                                 ,(symbol $Name))))

     (,(if private 'def- 'def)
       ,(symbol (string name "-Nil"))
       (,(symbol $name)))))

(defmacro dataclass [name & props]
  (dataclass* name false ;props))

(defmacro dataclass- [name & props]
  (dataclass* name true ;props))

(defn methods [d]
  (let [method-keys (->> (pairs (d :schema))
                         (filter |(= :method ($ 1)))
                         (map first))]
    (tabseq [method-key :in method-keys]
      method-key |(d $))))

(defn schema [d]
  (d :schema))

#########

(deftest "methods"
  (dataclass Pair :key :array :val :array {:k |(($ :k) 0) :v |(($ :val) 0)})
  (def p (pair :key @["hello"] :val @["world"]))
  (test (methods p)
        @{:k @short-fn
          :v @short-fn}))

(deftest "schema"
  (dataclass Pair :key :array :val :array {:k |(($ :k) 0) :v |(($ :val) 0)})
  (def p (pair :key @["hello"] :val @["world"]))
  (test (schema p)
        @{:k :method
          :key :array
          :v :method
          :val :array}))

(test-macro (dataclass Fact :content :string :source :string)
            (upscope
              (def Fact (merge-into @{:content "" :source ""} {:_name (keyword "Fact") :schema @{:content :string :source :string} :type (keyword "Fact")}))
              (def assert-fact (as-macro @validator (and (or :table :struct) (props :type :keyword :_name :keyword :content :string :source :string) (pred (short-fn (@any-proto? $ Fact))))))
              (def fact? (as-macro @predicate (and (or :table :struct) (props :type :keyword :_name :keyword :content :string :source :string) (pred (short-fn (@any-proto? $ Fact))))))
              (defn fact
                [&named content source]
                (assert-fact (table/setproto @{:content content :source source} Fact)))
              (def Fact-Nil (fact))))

(test-macro (dataclass Pair :key :array :val :array {:k |(($ :key) 0) :v |(($ :val) 0)})
            (upscope
              (def Pair (merge-into @{:k (short-fn (($ :key) 0)) :key @[] :v (short-fn (($ :val) 0)) :val @[]} {:_name (keyword "Pair") :schema @{:k :method :key :array :v :method :val :array} :type (keyword "Pair")}))
              (def assert-pair (as-macro @validator (and (or :table :struct) (props :type :keyword :_name :keyword :val :array :key :array) (pred (short-fn (@any-proto? $ Pair))))))
              (def pair? (as-macro @predicate (and (or :table :struct) (props :type :keyword :_name :keyword :val :array :key :array) (pred (short-fn (@any-proto? $ Pair))))))
              (defn pair
                [&named val key]
                (assert-pair (table/setproto @{:key key :val val} Pair)))
              (def Pair-Nil (pair))))

(deftest "typed table 0"
  (dataclass Node :val :array)
  (test-macro (dataclass Graph :nodes :table:k:Node)
              (upscope
                (def Graph (merge-into @{:nodes @{}} {:_name (keyword "Graph") :schema @{:nodes (quote (and :table (keys (pred node?))))} :type (keyword "Graph")}))
                (def assert-graph (as-macro @validator (and (or :table :struct) (props :type :keyword :_name :keyword :nodes (and :table (keys (pred node?)))) (pred (short-fn (@any-proto? $ Graph))))))
                (def graph? (as-macro @predicate (and (or :table :struct) (props :type :keyword :_name :keyword :nodes (and :table (keys (pred node?)))) (pred (short-fn (@any-proto? $ Graph))))))
                (defn graph
                  [&named nodes]
                  (assert-graph (table/setproto @{:nodes nodes} Graph)))
                (def Graph-Nil (graph)))))

(deftest "typed table 1"
  (dataclass Node :val :array)
  (dataclass Graph :nodes :table:k:Node)
  (test Graph
        @{:_name :Graph
          :nodes @{}
          :schema @{:nodes [and :table [keys [pred node?]]]}
          :type :Graph}))

(deftest "typed table 2"
  (print "ping")
  (dataclass Node :val :array)
  (dataclass Graph :nodes :table:v:Node)
  (test-error (graph :nodes @{:anode 1}) "failed clause (pred node?), predicate node? failed for value 1")
  (def n (node))
  (test (graph :nodes @{:anode n}) @{:nodes @{:anode @{}}}))

(deftest "typed array"
  (dataclass Fact :content :string :source :string)
  (test-macro (dataclass Knowledge :facts :array:Fact)
              (upscope
                (def Knowledge (merge-into @{:facts @[]} {:_name (keyword "Knowledge") :schema @{:facts (quote (and :array (values (pred fact?))))} :type (keyword "Knowledge")}))
                (def assert-knowledge (as-macro @validator (and (or :table :struct) (props :type :keyword :_name :keyword :facts (and :array (values (pred fact?)))) (pred (short-fn (@any-proto? $ Knowledge))))))
                (def knowledge? (as-macro @predicate (and (or :table :struct) (props :type :keyword :_name :keyword :facts (and :array (values (pred fact?)))) (pred (short-fn (@any-proto? $ Knowledge))))))
                (defn knowledge
                  [&named facts]
                  (assert-knowledge (table/setproto @{:facts facts} Knowledge)))
                (def Knowledge-Nil (knowledge)))))

(deftest "dependent macro expanded"
  (dataclass Fact :content :string :source :string)
  (dataclass Knowledge :facts :array:Fact)
  (def None (knowledge :facts @[]))
  (test-macro (dataclass Person :knows Knowledge)
              (upscope
                (def Person (merge-into @{:knows Knowledge-Nil} {:_name (keyword "Person") :schema @{:knows (quote (pred knowledge?))} :type (keyword "Person")}))
                (def assert-person (as-macro @validator (and (or :table :struct) (props :type :keyword :_name :keyword :knows (pred knowledge?)) (pred (short-fn (@any-proto? $ Person))))))
                (def person? (as-macro @predicate (and (or :table :struct) (props :type :keyword :_name :keyword :knows (pred knowledge?)) (pred (short-fn (@any-proto? $ Person))))))
                (defn person
                  [&named knows]
                  (assert-person (table/setproto @{:knows knows} Person)))
                (def Person-Nil (person)))))

(deftest "testing dataclass"

  (dataclass Fact
             :content :string
             :source :string)

  (test Fact
        @{:_name :Fact
          :content ""
          :schema @{:content :string :source :string}
          :source ""
          :type :Fact})
  (test fact? @check)
  (test assert-fact @validate)
  (test fact @fact)

  (test (fact :content "This is a fact" :source "My brain")
        @{:content "This is a fact"
          :source "My brain"})
  (test (assert-fact (fact :content "This is a fact" :source "My brain"))
        @{:content "This is a fact"
          :source "My brain"})
  (test (fact? (fact :content "This is a fact" :source "My brain"))
        true)
  (test (table/proto-flatten (fact :content "This is a fact" :source "My brain"))
        @{:_name :Fact
          :content "This is a fact"
          :schema @{:content :string :source :string}
          :source "My brain"
          :type :Fact}))

(deftest "dependent definition"

  (dataclass Fact :content :string :source :string)

  (dataclass Knowledge :facts :array:Fact)

  (def None (knowledge :facts @[]))

  (def sky-blue (knowledge :facts @[(fact :content "Sky is blue" :source "Observation")]))
  (test sky-blue
        @{:facts @[@{:content "Sky is blue"
                     :source "Observation"}]})
  (test (assert-knowledge sky-blue)
        @{:facts @[@{:content "Sky is blue"
                     :source "Observation"}]})
  (test (knowledge? sky-blue)
        true)
  (test (table/proto-flatten sky-blue)
        @{:_name :Knowledge
          :facts @[@{:content "Sky is blue"
                     :source "Observation"}]
          :schema @{:facts [and :array [values [pred fact?]]]}
          :type :Knowledge})

  (dataclass Person :name :string :knows Knowledge)

  (test Person
        @{:_name :Person
          :knows @{}
          :name ""
          :schema @{:knows [pred knowledge?]
                    :name :string}
          :type :Person})
  (test person @person)
  (test person? @check)

  (def nicole (person :name "Nicole" :knows (knowledge :facts @[(fact :content "Sky is blue" :source "Observation")])))

  (test nicole
        @{:knows @{:facts @[@{:content "Sky is blue"
                              :source "Observation"}]}
          :name "Nicole"})
  (test (assert-person nicole)
        @{:knows @{:facts @[@{:content "Sky is blue"
                              :source "Observation"}]}
          :name "Nicole"})
  (test (person? nicole)
        true)
  (test (table/proto-flatten nicole)
        @{:_name :Person
          :knows @{:facts @[@{:content "Sky is blue"
                              :source "Observation"}]}
          :name "Nicole"
          :schema @{:knows [pred knowledge?]
                    :name :string}
          :type :Person}))
