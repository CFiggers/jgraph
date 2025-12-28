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
          :boolean [:boolean n true]
          :function [:function n identity]))))

(defn- dataclass* [name private & props]

  (assertf (or (= :symbol (type name)) (= :string (type name)))
           "Dataclasses must be named using either a symbol or string. Got: %q" name)

  # Make names
  (def $Name (string/join (map capitalize (string/split "-" name)) "-"))
  (def $name (string/ascii-lower name))
  (def $assert-name (string "assert-" $name))
  (def $name? (string $name "?"))

  (assertf (or (even? (length props))
               (and (odd? (length props))
                    (dictionary? (last props))
                    (all |(function? (eval $)) (values (last props)))))
           "Props passed to %s must be pairs of names and types. A final, unpaired argument may be passed, in which case it must be a dictionary (table or struct) containing method definitions (all values must be functions)."
           # (even? (length props)) (odd? (length props)) (dictionary? (last props)) (all |(function? (eval $)) (values (last props))) (values (last props))
           (if private "dataclass-" "dataclass"))

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

(defmacro dataclass [name parent:tuple & props]
  (assertf (or (symbol? name) (string? name))
           "The first argument to `dataclass` ('name') must be a symbol or string. Got: %s" name)
  (assertf (tuple? parent:tuple)
           "The second argument to `dataclass` ('[parent]') must be a tuple. Got: %q" parent:tuple)
  (unless (empty? parent:tuple)
    (assertf (= 1 (length parent:tuple))
             "The parent tuple passed to `dataclass` ('[parent]') can have at max one element. Got: %q" parent:tuple)
    (assertf (symbol? (first parent:tuple))
             "The parent tuple passed to `dataclass` ('[parent]') must contain a symbol. Got: %q" (first parent:tuple))
    (assertf (every? ((juxt |($ :_name) |($ :type) |($ :schema)) (dyn (first parent:tuple))))
             "The symbol passed via `dataclass`'s parent tuple ('[parent]') must refer to an existing dataclass definition. Got: %q" (first parent:tuple)))
  (assertf (or (even? (length props)) (and (odd? (length props)) (dictionary? (last props))))
           "The props list passed to `dataclass` ('props') must be a series of prop name/type pairs. Got: %q" props)
  (dataclass* name false ;props))
(set ((dyn 'dataclass) :doc)
     (string/join
       ["(dataclass name [parent] & props-list &opt {:method function [...]})"
        "Define a dataclass."]
       "\n\n"))

(defmacro dataclass-
  [name parent:tuple & props]
  (assertf (or (symbol? name) (string? name))
           "The first argument to `dataclass-` ('name') must be a symbol or string. Got: %s" name)
  (assertf (tuple? parent:tuple)
           "The second argument to `dataclass-` must be a tuple. Got: %q" parent:tuple)
  (unless (empty? parent:tuple)
    (assertf (= 1 (length parent:tuple))
             "The parent tuple passed to `dataclass-` ('[parent]') can have at max one element. Got: %q" parent:tuple)
    (assertf (symbol? (first parent:tuple))
             "The parent tuple passed to `dataclass-` ('[parent]') must contain a symbol. Got: %q" (first parent:tuple))
    (assertf (every? ((juxt |($ :_name) |($ :type) |($ :schema)) (dyn (first parent:tuple))))
             "The symbol passed via `dataclass-`'s parent tuple ('[parent]') must refer to an existing dataclass definition. Got: %q" (first parent:tuple)))
  (assertf (or (even? (length props)) (and (odd? (length props)) (dictionary? (last props))))
           "The props list passed to `dataclass-` ('props') must be a series of prop name/type pairs. Got: %q" props)
  (dataclass* name true ;props))
(set ((dyn 'dataclass-) :doc)
     (string/join
       ["(dataclass- name [parent] & props-list &opt {:method function [...]})"
        "Define a dataclass, but privately."]
       "\n\n"))

(defn methods [d]
  (let [method-keys (->> (pairs (d :schema))
                         (filter |(= :method ($ 1)))
                         (map first))]
    (tabseq [method-key :in method-keys]
      method-key |(d $))))

(defn schema [d]
  (d :schema))

#########
# Tests
#########

(deftest "Dataclass Specification"
  :should ``
  The `dataclass` and `dataclass-` macros must:
  
  - [ ] 1) Correctly return an `upscope` form containing:
    - [ ] 1.A) A dataclass prototype definition (e.g. `Graph`) which:
      - [x] 1.A.I  ) Must be a table
      - [x] 1.A.II ) Must have a `:_name` key set to the dataclass name                     
      - [x] 1.A.III) Must have a `:type` key set to keyword of the dataclass name 
      - [x] 1.A.IV ) Must have a `:schema` key set to a struct of original props and methods
      - [x] 1.A.V  ) Must have a prototype assigned if a parent dataclass is provided (see 3)
      - [x] 1.A.VI ) Must have each prop (see 4) set to a default value of the correct type
      - [ ] 1.A.VII) Must have each method (see 5) assigned as passed in
    - [ ] 1.B) A constructor function (e.g. `graph`) which:
      - [x] 1.B.I  ) Takes keyword args corresponding to each prop in the dataclass definition
      - [x] 1.B.II ) Ignores keyword args that are not part of the dataclass's schema
      - [x] 1.B.III) When an invalid value is passed to a prop's keyword arg, raises an error
                     (with "validity" of a prop defined by the type of the prop in the 
                     dataclass's `:schema` definition)
      - [x] 1.B.IV ) Returns a valid instance of the dataclass with each valid passed-in prop 
                     assigned (with "validity" of an instance defined as passing the 
                     dataclass's validator/predicate functions; see 1.D and 1.E)
    - [ ] 1.C) A nil instance of the dataclass (e.g. `Graph-Nil`)
      - [ ] 1.C.I  ) The nil instance should be equivalent to the instance created by calling 
                     the constructor function with no keyword arguments
    - [ ] 1.D) A validator function (e.g. `assert-graph`)
      - [ ] 1.D.I  ) The validator, if called with a valid instance of the dataclass, returns
                     that instance unchanged as though by the `identity` function 
      - [ ] 1.D.II ) The validator, if called with an invalid instance of the dataclass or any 
                     other value, raises an error
    - [ ] 1.E) A predicate function (e.g. `graph?`)
      - [ ] 1.E.I  ) The predicate, if called with a valid instance of the dataclass, returns
                     true 
      - [ ] 1.E.II ) The predicate, if called with an invalid instance of the dataclass or any 
                     other value, returns false
  
  - [ ] 2) Correctly require and process a dataclass name
    - [ ] 2.A) When the first argument to the `dataclass` or `dataclass-` macro is not a 
               symbol or string, the macro raises an error 
    - [ ] 2.B) When the first argument to `dataclass` or `dataclass-` macro is a symbol or 
               string, the symbol or string is used to create names for the forms returned in 
               the `upscope` form as follows: 
      - [ ] 2.B.I  ) The prototype definition (see 1.A) is named with a title-cased variant of 
                     the name symbol/string (e.g. `Graph` or 'Graph-Node')
      - [ ] 2.B.II ) The constructor function (see 1.B) is named with an all lower-case
                     variant of the name symbol/string (e.g. `graph` or `graph-node`)
      - [ ] 2.B.III) The nil instance of the dataclass (see 1.C) is named with a title-cased
                     variant of the name symbol/string, appended with "-Nil" (e.g. `Graph-Nil` 
                     or `Graph-Node-Nil`)
      - [ ] 2.B.IV ) The validator function (see 1.D) is named with an all lower-case variant 
                     of the name symbol/string, prepended with "assert-" (e.g. `assert-graph` 
                     or `assert-graph-node`)
      - [ ] 2.B.V  ) The predicate function (see 1.E) is named with an all lower-case variant 
                     of the name symbol/string, appended with "?" (e.g. `graph?` or 
                     `graph-node?`)

  - [ ] 3) Correctly require and process a parent dataclass tuple
    - [ ] 3.A) When the second argument to the `dataclass` or `dataclass-` macro is not a 
               tuple, the macro raises an error
    - [ ] 3.B) When the parent dataclass tuple has more than one value, the macro raises an 
               error 
    - [ ] 3.C) When the parent dataclass tuple has one value and that value is not a symbol, 
               the macro raises an error
    - [ ] 3.D) When the parent dataclass tuple has one symbol value and that symbol does not 
               refer to the prototype definition of an existing dataclass, the macro raises an 
               error
    - [ ] 3.E) When the parent dataclass tuple has one symbol value that refers to the 
               prototype definition of an existing dataclasses, that existing dataclass 
               definition is used as a parent dataclass in the definition of the new 
               dataclass, as follows:
      - [ ] 3.E.I  ) Props (see 4)
        - [ ] 3.E.I.a) Props from the parent dataclass are merged into the new prototype 
                       definition, with child props overriding inherited ones
        - [ ] 3.E.I.b) Props from the parent dataclasses are merged into the child dataclass's 
                       `:schema` with child props overriding inherited ones
      - [ ] 3.E.II ) Methods (see 5)
        - [ ] 3.E.II.a) Methods from the parent dataclasses are merged into the new prototype 
                        definition, with child methods overriding inherited ones
        - [ ] 3.E.II.b) Methods from the parent dataclasses are merged into the child 
                        dataclass's `:schema` with child methods overriding inherited ones
      - [ ] 3.E.III) Prototype
        - [ ] 3.E.III.a) The parent dataclass is set as the next-level prototype of the new 
                         prototype definition
        - [ ] 3.E.III.b) Well-formed instances of the new dataclass pass the validation and 
                         predicate functions of the parent dataclass
        - [ ] 3.E.III.c) If the parent dataclass is itself the child of yet another dataclass 
                         defined in this way, the new child will also pass the validation and 
                         predicate functions of its grandparent dataclass (and great-grand- 
                         parent dataclass, etc.)

  - [ ] 4) Correctly process dataclass prop names and types  
    - [ ] 4.A) When, in the prop list (defined as all arguments between the parent dataclass 
               tuple [see 3] and the optional final, unpaired dictionary indicating methods 
               [see 5]; of which there may be zero), any ordinally odd value (subsequently 
               "prop name") is not a keyword or string, the macro raises an error
    - [ ] 4.B) Each prop name should be paired with a value indicating the prop's type, which 
               is handled as follows:
      - [ ] 4.B.I  ) When the value is a keyword: 
        - [ ] 4.B.I.a) When the keyword is not a recognized type keyword (`:string`, 
                       `:buffer`, `:array[:{type}] `, `:tuple[:k:{type}][:v:{type}]`, 
                       `:struct`, `:table`, `:number`, `:keyword`, `:boolean`, or 
                       `:function`), the macro raises an error
        - [ ] 4.B.I.b) When the keyword is a recognized type keyword, that keyword is the 
                       prop's type
      - [ ] 4.B.II ) When the value is a symbol:
        - [ ] 4.B.II.a) When the symbol does not refer to another dataclass's prototype 
                        definition, the macro raises an error 
        - [ ] 4.B.II.b) When the symbol refers to another dataclass's prototype definition, 
                        that dataclass is the prop's type
      - [ ] 4.B.III) When the value is a two-value tuple: 
        - [ ] 4.B.III.a) When the first value in the tuple does not satisfy either the keyword 
                         constraints of 4.B.I or the symbol constraints of 4.B.II, the macro
                         raises an error
        - [ ] 4.B.III.b) When the first value in the tuple does satisfy either keyword type
                         constraints (see 4.B.I) or dataclass symbol constraints (see 4.B.II), 
                         that value is the prop's type
        - [ ] 4.B.III.c) The second value in the tuple is set as the prop's default value in 
                         the dataclass's prototype definition
      - [ ] 4.B.IV ) When a value satisfies none of the above conditions, the macro raises an 
                     error
    - [ ] 4.C) Each prop is added to the dataclass's prototype definition as follows:
      - [ ] 4.C.I  ) The prop is added to the dataclass prototype definition as a key with a 
                     default value based on the prop's type (or a custom default, see 4.B.III)
      - [ ] 4.C.II ) The prop name and type are included as a key and value pair in the 
                     `:schema` on the dataclass's prototype definition (see 1.A.III)

  - [ ] 5) Correctly process and assign dataclass methods
    - [ ] 5.A) When a final, unpaired argument is passed to the `dataclass` or `dataclass-` 
               macro and the argument is not a dictionary (table or struct) the macro raises 
               an error 
    - [ ] 5.B) When a final, unpaired argument is passed to the `dataclass` or `dataclass-` 
               macro and the argument is a dictionary (table or struct) and any values of the 
               dictionary are not functions, the constructor raises an error
    - [ ] 5.C) When a final, unpaired argument is passed to the `dataclass` or `dataclass-` 
               macro and the argument is a dictionary (table or struct) and all values of the 
               dictionary are functions, the functions are processed and added as methods on 
               the dataclass prototype definition as follows: 
      - [ ] 5.C.I  ) Each key in the dictionary is treated as a method name (converted to a 
                     keyword if necessary)
      - [ ] 5.C.II ) The method name and keyword type of `:function` are added as a key-value 
                     pair in the `:schema` on the dataclass's prototype definition
      - [ ] 5.C.III) The function is assigned to the corresponding method name key in the 
                     dataclass's prototype definition
 
  - [ ] 6) Correctly define forms in the `upscope` form as public or private
    - [ ] 6.A) When the dataclass is defined using the `dataclass` macro, definitions in the 
               `upscope` form use `def` and `defn` (aka public definitions)
    - [ ] 6.B) When the dataclass is defined using the `dataclass-` macro, definitions in the 
               `upscope` form use `def-` and `defn-` (aka private definitions)
``)

(deftest "Basic dataclass definitions"
  :should ``
  The `dataclass` and `dataclass-` macros must:
  
  - [x] 1) Correctly return an `upscope` form containing:
    - [x] 1.A) A prototype definition (e.g. `Graph`)
      - [x] 1.A.I  ) Must have a `:_name` key set to the dataclass name
      - [x] 1.A.II ) Must have a `:type` key set to keword of dataclass name 
      - [x] 1.A.III) Must have a `:schema` key set to a struct of original props and methods
      - [x] 1.A.IV ) Must have each prop set to a default value of the correct type
      - [x] 1.A.V  ) Must have each method passed in assigned
    - [x] 1.B) A constructor function (e.g. `graph`)
    - [x] 1.C) A nil instance of the dataclass (e.g. `Graph-Nil`)
    - [x] 1.D) A validator (e.g. `assert-graph`)
    - [x] 1.E) A predicate (e.g. `graph?`)
  ``

  # 1, 1.A, 1.A.I, 1.A.II, 1.A.III, 1.B, 1.C, 1.D, 1.E
  (test-macro
    (dataclass Fact [] :content :string :source :string)
    (upscope
      (def Fact (merge-into @{:content "" :source ""} {:_name (keyword "Fact") :schema @{:content :string :source :string} :type (keyword "Fact")}))
      (def assert-fact (as-macro @validator (and (or :table :struct) (props :type :keyword :_name :keyword :content :string :source :string) (pred (short-fn (@any-proto? $ Fact))))))
      (def fact? (as-macro @predicate (and (or :table :struct) (props :type :keyword :_name :keyword :content :string :source :string) (pred (short-fn (@any-proto? $ Fact))))))
      (defn fact
        [&named content source]
        (assert-fact (table/setproto @{:content content :source source} Fact)))
      (def Fact-Nil (fact))))

  (def fact-form (macex1 '(dataclass Fact [] :content :string :source :string)))

  # 1.A
  (test (find |(= 'Fact ($ 1)) (tuple/slice fact-form 1))
        [def Fact [merge-into @{:content "" :source ""} {:_name [keyword "Fact"] :schema @{:content :string :source :string} :type [keyword "Fact"]}]])
  # 1.B
  (test (find |(= 'fact ($ 1)) (tuple/slice fact-form 1))
        [defn fact [&named content source] [assert-fact [table/setproto @{:content content :source source} Fact]]])

  # 1.C
  (test (find |(= 'Fact-Nil ($ 1)) (tuple/slice fact-form 1))
        [def Fact-Nil [fact]])
  # 1.D
  (test (find |(= 'assert-fact ($ 1)) (tuple/slice fact-form 1))
        [def assert-fact [as-macro @validator [and [or :table :struct] [props :type :keyword :_name :keyword :content :string :source :string] [pred [short-fn [@any-proto? $ Fact]]]]]])
  # 1.E
  (test (find |(= 'fact? ($ 1)) (tuple/slice fact-form 1))
        [def fact? [as-macro @predicate [and [or :table :struct] [props :type :keyword :_name :keyword :content :string :source :string] [pred [short-fn [@any-proto? $ Fact]]]]]])

  # 1, 1.A, 1.A.I, 1.A.II, 1.A.III, 1.A.IV 1.B, 1.C, 1.D, 1.E
  (test-macro
    (dataclass Pair [] :key :array :val :array {:k |(($ :key) 0) :v |(($ :val) 0)})
    (upscope
      (def Pair (merge-into @{:k (short-fn (($ :key) 0)) :key @[] :v (short-fn (($ :val) 0)) :val @[]} {:_name (keyword "Pair") :schema @{:k :method :key :array :v :method :val :array} :type (keyword "Pair")}))
      (def assert-pair (as-macro @validator (and (or :table :struct) (props :type :keyword :_name :keyword :val :array :key :array) (pred (short-fn (@any-proto? $ Pair))))))
      (def pair? (as-macro @predicate (and (or :table :struct) (props :type :keyword :_name :keyword :val :array :key :array) (pred (short-fn (@any-proto? $ Pair))))))
      (defn pair
        [&named val key]
        (assert-pair (table/setproto @{:key key :val val} Pair)))
      (def Pair-Nil (pair))))

  (def pair-form (macex1 '(dataclass Pair [] :key :array :val :array {:k |(($ :key) 0) :v |(($ :val) 0)})))

  # 1.A
  (test (find |(= 'Pair ($ 1)) (tuple/slice pair-form 1))
        [def Pair [merge-into @{:k [short-fn [[$ :key] 0]] :key @[] :v [short-fn [[$ :val] 0]] :val @[]} {:_name [keyword "Pair"] :schema @{:k :method :key :array :v :method :val :array} :type [keyword "Pair"]}]])
  # 1.B
  (test (find |(= 'pair ($ 1)) (tuple/slice pair-form 1))
        [defn pair [&named val key] [assert-pair [table/setproto @{:key key :val val} Pair]]])
  # 1.C
  (test (find |(= 'Pair-Nil ($ 1)) (tuple/slice pair-form 1))
        [def Pair-Nil [pair]])
  # 1.D
  (test (find |(= 'assert-pair ($ 1)) (tuple/slice pair-form 1))
        [def assert-pair [as-macro @validator [and [or :table :struct] [props :type :keyword :_name :keyword :val :array :key :array] [pred [short-fn [@any-proto? $ Pair]]]]]])
  # 1.E
  (test (find |(= 'pair? ($ 1)) (tuple/slice pair-form 1))
        [def pair? [as-macro @predicate [and [or :table :struct] [props :type :keyword :_name :keyword :val :array :key :array] [pred [short-fn [@any-proto? $ Pair]]]]]])

  # 1, 1.A, 1.A.I, 1.A.II, 1.A.III, 1.B, 1.C, 1.D, 1.E
  (test-macro
    (dataclass- Private-Fact [] :content :string :source :string)
    (upscope
      (def- Private-Fact (merge-into @{:content "" :source ""} {:_name (keyword "Private-Fact") :schema @{:content :string :source :string} :type (keyword "Private-Fact")}))
      (def- assert-private-fact (as-macro @validator (and (or :table :struct) (props :type :keyword :_name :keyword :content :string :source :string) (pred (short-fn (@any-proto? $ Private-Fact))))))
      (def- private-fact? (as-macro @predicate (and (or :table :struct) (props :type :keyword :_name :keyword :content :string :source :string) (pred (short-fn (@any-proto? $ Private-Fact))))))
      (defn- private-fact
        [&named content source]
        (assert-private-fact (table/setproto @{:content content :source source} Private-Fact)))
      (def- Private-Fact-Nil (private-fact))))

  (def private-fact-form (macex1 '(dataclass- Private-Fact [] :content :string :source :string)))

  # 1.A
  (test (find |(= 'Private-Fact ($ 1)) (tuple/slice private-fact-form 1))
        [def-
         Private-Fact
         [merge-into
          @{:content "" :source ""}
          {:_name [keyword "Private-Fact"]
           :schema @{:content :string :source :string}
           :type [keyword "Private-Fact"]}]])
  # 1.B
  (test (find |(= 'private-fact ($ 1)) (tuple/slice private-fact-form 1))
        [defn- private-fact [&named content source] [assert-private-fact [table/setproto @{:content content :source source} Private-Fact]]])
  # 1.C
  (test (find |(= 'Private-Fact-Nil ($ 1)) (tuple/slice private-fact-form 1))
        [def- Private-Fact-Nil [private-fact]])
  # 1.D
  (test (find |(= 'assert-private-fact ($ 1)) (tuple/slice private-fact-form 1))
        [def- assert-private-fact [as-macro @validator [and [or :table :struct] [props :type :keyword :_name :keyword :content :string :source :string] [pred [short-fn [@any-proto? $ Private-Fact]]]]]])
  # 1.E
  (test (find |(= 'private-fact? ($ 1)) (tuple/slice private-fact-form 1))
        [def- private-fact? [as-macro @predicate [and [or :table :struct] [props :type :keyword :_name :keyword :content :string :source :string] [pred [short-fn [@any-proto? $ Private-Fact]]]]]])

  # 1, 1.A, 1.A.I, 1.A.II, 1.A.III, 1.A.IV 1.B, 1.C, 1.D, 1.E
  (test-macro
    (dataclass- Private-Pair [] :key :array :val :array {:k |(($ :key) 0) :v |(($ :val) 0)})
    (upscope
      (def- Private-Pair (merge-into @{:k (short-fn (($ :key) 0)) :key @[] :v (short-fn (($ :val) 0)) :val @[]} {:_name (keyword "Private-Pair") :schema @{:k :method :key :array :v :method :val :array} :type (keyword "Private-Pair")}))
      (def- assert-private-pair (as-macro @validator (and (or :table :struct) (props :type :keyword :_name :keyword :val :array :key :array) (pred (short-fn (@any-proto? $ Private-Pair))))))
      (def- private-pair? (as-macro @predicate (and (or :table :struct) (props :type :keyword :_name :keyword :val :array :key :array) (pred (short-fn (@any-proto? $ Private-Pair))))))
      (defn- private-pair
        [&named val key]
        (assert-private-pair (table/setproto @{:key key :val val} Private-Pair)))
      (def- Private-Pair-Nil (private-pair))))

  (def private-pair-form (macex1 '(dataclass- Private-Pair [] :key :array :val :array {:k |(($ :key) 0) :v |(($ :val) 0)})))

  # 1.A
  (test (find |(= 'Private-Pair ($ 1)) (tuple/slice private-pair-form 1))
        [def- Private-Pair [merge-into @{:k [short-fn [[$ :key] 0]] :key @[] :v [short-fn [[$ :val] 0]] :val @[]} {:_name [keyword "Private-Pair"] :schema @{:k :method :key :array :v :method :val :array} :type [keyword "Private-Pair"]}]])
  # 1.B
  (test (find |(= 'private-pair ($ 1)) (tuple/slice private-pair-form 1))
        [defn- private-pair [&named val key] [assert-private-pair [table/setproto @{:key key :val val} Private-Pair]]])
  # 1.C
  (test (find |(= 'Private-Pair-Nil ($ 1)) (tuple/slice private-pair-form 1))
        [def- Private-Pair-Nil [private-pair]])
  # 1.D
  (test (find |(= 'assert-private-pair ($ 1)) (tuple/slice private-pair-form 1))
        [def- assert-private-pair [as-macro @validator [and [or :table :struct] [props :type :keyword :_name :keyword :val :array :key :array] [pred [short-fn [@any-proto? $ Private-Pair]]]]]])
  # 1.E
  (test (find |(= 'private-pair? ($ 1)) (tuple/slice private-pair-form 1))
        [def- private-pair? [as-macro @predicate [and [or :table :struct] [props :type :keyword :_name :keyword :val :array :key :array] [pred [short-fn [@any-proto? $ Private-Pair]]]]]]))

(deftest "Dataclass definition requirements"
  :should ``
  The `dataclass` and `dataclass-` macros must:
  
  - [x] 1) Correctly return an `upscope` form containing:
    - [x] 1.A) A prototype definition (e.g. `Graph`)
      - [x] 1.A.I  ) Must have a `:_name` key set to the dataclass name
      - [x] 1.A.II ) Must have a `:type` key set to keword of dataclass name 
      - [x] 1.A.III) Must have a `:schema` key set to a struct of original props and methods
      - [x] 1.A.IV ) Must have each prop set to a default value of the correct type
      - [x] 1.A.V  ) Must have each method passed in assigned
    - [x] 1.B) A constructor function (e.g. `graph`)
    - [x] 1.C) A nil instance of the dataclass (e.g. `Graph-Nil`)
    - [x] 1.D) A validator (e.g. `assert-graph`)
    - [x] 1.E) A predicate (e.g. `graph?`)
  ``

  (dataclass Fact [] :content :string :source :string)
  (def fact-form (macex1 '(dataclass- Fact [] :content :string :source :string)))

  # 1.A.I
  (test (table? Fact) true)
  # 1.A.II 
  (test (truthy? (Fact :_name)) true)
  # 1.A.III 
  (test (truthy? (Fact :type)) true)
  # 1.A.IV 
  (test (truthy? (Fact :schema)) true)
  # 1.A.V 
  (test (nil? (table/getproto Fact)) true)
  # 1.A.VI 
  (test (truthy? (Fact :content)) true)
  (test (= "" (Fact :content)) true)
  (test (truthy? (Fact :source)) true)
  (test (= "" (Fact :source)) true)

  # 1.B.I 
  (test (has-value? ((find |(= 'fact ($ 1)) (tuple/slice fact-form 1)) 2) 'content) true)
  (test (has-value? ((find |(= 'fact ($ 1)) (tuple/slice fact-form 1)) 2) 'source) true)
  # 1.B.II 
  (test (nil? ((fact :bogus true) :bogus)) true)
  # 1.B.III 
  (test-error (fact :content :errors) "failed clause :string, expected value of type string, got :errors")
  # 1.B.IV 
  (test (assert-fact (fact :content "test" :source "test")) @{:content "test" :source "test"})
  (test (fact? (fact :content "test" :source "test")) true)

  # 1.C.I
  (test (deep= Fact-Nil (fact)) true)

  # 1.D.I
  (test (assert-fact (fact :content "test" :source "test")) @{:content "test" :source "test"})
  # 1.D.II
  (test-error (assert-fact @{:bogus true}) "failed clause :keyword, expected value of type keyword, got nil")

  # 1.E.I
  (test (= true (fact? (fact :content "test" :source "test"))) true)
  # 1.E.II
  (test (= false (fact? @{:bogus true})) true)

  (dataclass Pair [] :key :array :val :array {:k |(($ :key) 0) :v |(($ :val) 0)})
  (def pair-form (macex1 '(dataclass Pair [] :key :array :val :array {:k |(($ :key) 0) :v |(($ :val) 0)})))

  # 1.A.I
  # 1.A.II
  # 1.A.III
  # 1.A.IV
  # 1.A.V
  # 1.A.VI
  # 1.A.VII

  # 1.B.I 
  # 1.B.I 
  # 1.B.I 
  # 1.B.I 

  # 1.C.I

  # 1.D.I
  # 1.D.II

  # 1.E.I
  # 1.E.II

  (dataclass- Private-Fact [] :content :string :source :string)
  (def private-fact-form (macex1 '(dataclass- Private-Fact [] :content :string :source :string)))

  (dataclass- Private-Pair [] :key :array :val :array {:k |(($ :key) 0) :v |(($ :val) 0)})
  (def private-pair-form (macex1 '(dataclass- Private-Pair [] :key :array :val :array {:k |(($ :key) 0) :v |(($ :val) 0)}))))

(deftest "testing dataclass"
  :should ``
  The `dataclass` and `dataclass-` macros must:
  
  - [ ] 1) Correctly return an `upscope` form containing:
    - [x] 1.A) A prototype definition (e.g. `Graph`)
    - [x] 1.B) A constructor function (e.g. `graph`)
      - [x] 1.B.I  ) The constructor instance takes keyword args of each prop
      - [x] 1.B.II ) If a prop is not passed in, the returned instance falls back to 
                     the default values in the prototype
    - [x] 1.C) A nil instance of the dataclass (e.g. `Graph-Nil`)
      - [x] 1.C.I  ) The nil instance should contain no keys itself, but have the prototype 
                     assigned so that all keys fall back to the prototype
      - [x] 1.C.II ) The nil instance should be equivalent to the instance created
                     by calling the constructor function with no arguments
    - [x] 1.D) A validator (e.g. `assert-graph`)
      - [x] 1.D.I  ) The validator, if called with a valid instance of the dataclass, returns
                     that instance unchanged as though by the `identity` function 
      - [x] 1.D.II ) The validator, if called with an invalid instance of the dataclass or 
                     any other value, raises an error
    - [x] 1.E) A predicate (e.g. `graph?`)
      - [x] 1.E.I ) The predicate, if called with a valid instance of the dataclass, returns
                     true 
      - [x] 1.E.II) The predicate, if called with an invalid instance of the dataclass or 
                    any other value, returns false
  ``

  (dataclass Fact [] :content :string :source :string)
  (dataclass- Private-Fact [] :content :string :source :string)

  # 1.A 
  (test Fact
        @{:_name :Fact
          :content ""
          :schema @{:content :string :source :string}
          :source ""
          :type :Fact})
  (test Private-Fact
        @{:_name :Private-Fact
          :content ""
          :schema @{:content :string :source :string}
          :source ""
          :type :Private-Fact})
  # 1.B
  (test fact @fact)
  (test private-fact @private-fact)
  # 1.B.I
  (test (fact :content "This is a fact" :source "My brain")
        @{:content "This is a fact"
          :source "My brain"})
  (test (private-fact :content "This is a private fact" :source "My brain")
        @{:content "This is a private fact"
          :source "My brain"})
  # 1.B.II
  (test (table/proto-flatten (fact :content "This is a fact" :source "My brain"))
        @{:_name :Fact
          :content "This is a fact"
          :schema @{:content :string :source :string}
          :source "My brain"
          :type :Fact})
  (test (table/proto-flatten (private-fact :content "This is a fact" :source "My brain"))
        @{:_name :Private-Fact
          :content "This is a fact"
          :schema @{:content :string :source :string}
          :source "My brain"
          :type :Private-Fact})
  # 1.C, 1.C.I
  (test Fact-Nil @{})
  (test Private-Fact-Nil @{})
  # 1.C.I
  (test (table/getproto Fact-Nil)
        @{:_name :Fact
          :content ""
          :schema @{:content :string :source :string}
          :source ""
          :type :Fact})
  (test (table/getproto Private-Fact-Nil)
        @{:_name :Private-Fact
          :content ""
          :schema @{:content :string :source :string}
          :source ""
          :type :Private-Fact})
  # 1.C.II
  (test (deep= (fact) Fact-Nil) true)
  (test (deep= (private-fact) Private-Fact-Nil) true)
  # 1.D 
  (test assert-fact @validate)
  (test assert-private-fact @validate)
  # 1.D.I
  (test (assert-fact (fact :content "This is a fact" :source "My brain"))
        @{:content "This is a fact"
          :source "My brain"})
  (test (assert-private-fact (private-fact :content "This is a private fact" :source "My brain"))
        @{:content "This is a private fact"
          :source "My brain"})
  # 1.D.II
  (test-error (assert-fact :not-a-fact) "failed clause (or :table :struct), choice failed: :not-a-fact")
  (test-error (assert-private-fact :not-a-fact) "failed clause (or :table :struct), choice failed: :not-a-fact")
  # 1.E
  (test fact? @check)
  (test private-fact? @check)
  # 1.E.I
  (test (fact? (fact :content "This is a fact" :source "My brain"))
        true)
  (test (private-fact? (fact :content "This is a fact" :source "My brain"))
        false)
  # 1.E.II 
  (test (fact? :not-a-fact) false)
  (test (private-fact? :not-a-fact) false))

(deftest "`methods` auxiliary function"
  :should "When passed an instance of a dataclass with methods, return a table containing the methods"
  (dataclass Pair [] :key :array :val :array {:k |(($ :k) 0) :v |(($ :val) 0)})
  (def p (pair :key @["hello"] :val @["world"]))
  (test (methods p)
        @{:k @short-fn
          :v @short-fn}))

(deftest "`schema` auxiliary function"
  :should "When passed an instance of a dataclass, return a table of the dataclass's schema"
  (dataclass Pair [] :key :array :val :array {:k |(($ :k) 0) :v |(($ :val) 0)})
  (def p (pair :key @["hello"] :val @["world"]))
  (test (schema p)
        @{:k :method
          :key :array
          :v :method
          :val :array}))


(deftest "typed table 0"
  (dataclass Node [] :val :array)
  (test-macro (dataclass Graph [] :nodes :table:k:Node)
              (upscope
                (def Graph (merge-into @{:nodes @{}} {:_name (keyword "Graph") :schema @{:nodes (quote (and :table (keys (pred node?))))} :type (keyword "Graph")}))
                (def assert-graph (as-macro @validator (and (or :table :struct) (props :type :keyword :_name :keyword :nodes (and :table (keys (pred node?)))) (pred (short-fn (@any-proto? $ Graph))))))
                (def graph? (as-macro @predicate (and (or :table :struct) (props :type :keyword :_name :keyword :nodes (and :table (keys (pred node?)))) (pred (short-fn (@any-proto? $ Graph))))))
                (defn graph
                  [&named nodes]
                  (assert-graph (table/setproto @{:nodes nodes} Graph)))
                (def Graph-Nil (graph)))))

(deftest "typed table 1"
  (dataclass Node [] :val :array)
  (dataclass Graph [] :nodes :table:k:Node)
  (test Graph
        @{:_name :Graph
          :nodes @{}
          :schema @{:nodes [and :table [keys [pred node?]]]}
          :type :Graph}))

(deftest "typed table 2"
  (dataclass Node [] :val :array)
  (dataclass Graph [] :nodes :table:v:Node)
  (test-error (graph :nodes @{:anode 1}) "failed clause (pred node?), predicate node? failed for value 1")
  (def n (node))
  (test (graph :nodes @{:anode n}) @{:nodes @{:anode @{}}}))

(deftest "typed array"
  (dataclass Fact [] :content :string :source :string)
  (test-macro (dataclass Knowledge [] :facts :array:Fact)
              (upscope
                (def Knowledge (merge-into @{:facts @[]} {:_name (keyword "Knowledge") :schema @{:facts (quote (and :array (values (pred fact?))))} :type (keyword "Knowledge")}))
                (def assert-knowledge (as-macro @validator (and (or :table :struct) (props :type :keyword :_name :keyword :facts (and :array (values (pred fact?)))) (pred (short-fn (@any-proto? $ Knowledge))))))
                (def knowledge? (as-macro @predicate (and (or :table :struct) (props :type :keyword :_name :keyword :facts (and :array (values (pred fact?)))) (pred (short-fn (@any-proto? $ Knowledge))))))
                (defn knowledge
                  [&named facts]
                  (assert-knowledge (table/setproto @{:facts facts} Knowledge)))
                (def Knowledge-Nil (knowledge)))))

(deftest "dependent macro expanded"
  (dataclass Fact [] :content :string :source :string)
  (dataclass Knowledge [] :facts :array:Fact)
  (def None (knowledge :facts @[]))
  (test-macro (dataclass Person [] :knows Knowledge)
              (upscope
                (def Person (merge-into @{:knows Knowledge-Nil} {:_name (keyword "Person") :schema @{:knows (quote (pred knowledge?))} :type (keyword "Person")}))
                (def assert-person (as-macro @validator (and (or :table :struct) (props :type :keyword :_name :keyword :knows (pred knowledge?)) (pred (short-fn (@any-proto? $ Person))))))
                (def person? (as-macro @predicate (and (or :table :struct) (props :type :keyword :_name :keyword :knows (pred knowledge?)) (pred (short-fn (@any-proto? $ Person))))))
                (defn person
                  [&named knows]
                  (assert-person (table/setproto @{:knows knows} Person)))
                (def Person-Nil (person)))))


(deftest "dependent definition"

  (dataclass Fact [] :content :string :source :string)

  (dataclass Knowledge [] :facts :array:Fact)

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

  (dataclass Person [] :name :string :knows Knowledge)

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
