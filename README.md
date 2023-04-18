# jgraph

This library provides graph, directional graph (digraph), and weighted graph data structures and algorithms in [Janet](janet-lang/janet), for [Janet](janet-lang/janet). 

Very much **work in progress.** Issues and pull requests welcome!

## Basics

Install `jgraph`:

```bash
$ sudo jpm install https://github.com/CFiggers/jgraph
```

Import jgraph:

```janet
(import jgraph) # To prefix jgraph functions with "jgraph", e.g. `jgraph/graph`
# or
(import jgraph :as j) # To prefix jgraph functions with "j", e.g. `j/graph`
# or
(use jgraph) # To not prefix jgraph functions at all (beware collisions)
```

Create a graph:

```janet
# Initialize graphs with any combination of: edges, adacency maps, nodes, other graphs

#              Edges ---------          Nodes --------
#                      |     |                   | | |
#                      v     v                   v v v
(def my-graph (graph [1 2] [2 3] {3 [4] 5 [6 7]} 7 8 9))
#                                       ^
#                                       |
#                  Adjacency map --------
```

Create digraphs and weighted graphs:

```janet
# Digraph adds one-way edges and tracks incoming separate from outgoing
(def my-digraph (digraph [1 2] [2 3] {3 [4] 5 [6 7]} 7 8 9))

# Weighted graph requires weights for each edge (will error if not provided)
(def my-weighted-graph (weighted-graph {:a {:b 10 :c 20} :c {:d 30} :e {:b 5 :d 5}}))

# Weighted digraph combines features of weighted graph and digraph
(def my-weighted-digraph (weighted-digraph [:a :b 10] [:a :c 20] [:c :d 30] [:d :b 10]))

# Add weighted or digraph features (or both) to an existing graph (and automatically 
# update edges if necessary)
(def my-new-graph (graph [:a :b] [:b :c] [:a :c]))
(make-digraph! my-new-graph)
(make-weighted! my-new-graph)
(metadata my-new-graph)
=> @{:digraph true :graph true :weighted true}
```

Inspect a graph:
```janet
(metadata my-graph)
=> @{:graph true}

(nodes my-graph)
=> @[1 2 3 4 5 6 7 8 9]

(edges my-weighted-digraph)
=> @[(:a :c) (:a :b) (:c :d) (:d :b)]

(successors my-graph 3)
=> @[2 4]

(predecessors my-weighted-digraph :b)
=> @[:a :d]

(out-degree my-graph 3)
=> 2

(in-degree my-weighted-digraph :b)
=> 2

(get-weight my-weighted-graph [:a :c])
=> 20

(map (juxt graph? digraph? weighted?) [my-graph my-weighted-digraph])
=> ([true nil nil] [true true true])
```

Add/remove items (most mutate in place):

```janet
(add-nodes my-graph "foobar" {:name "baz"} [1 2 3])

(add-edges my-graph [10 11] ["foobar" {:name "baz"}])

(add-edges my-weighted-graph [:e :f 40] [:f :g 50]) # weighted edges have a numeric third term

(remove-nodes my-graph 1 2 3)

(remove-edges my-graph [1 2] [2 3])

(subgraph my-graph [5 6 7]) # Does not mutate original graph, returns new one
```

## Dependencies

* [Janet](https://www.github.com/janet-lang/janet)
* [Spork](https://www.github.com/janet-lang/spork)
* [Dev/Testing Only] [Ianthehenry/Judge](https://www.github.com/ianthehenry/judge)

## TODO
- [x] Basic graph construction and add/remove functions
- [ ] Graph traversal algorithms
- [ ] Pathfinding algorithms
- [ ] Adding, updating, querying Attributes

## Prior Art

This library is hugely inspired by and structured after [Loom](https://www.github.com/aysylu/loom), [Clojure](https://clojure.org)'s most popular graph library.

Loom is Copyright (C) 2010-2016 Aysylu Greenberg & Justin Kramer (jkkramer@gmail.com) and distributed under the [Eclipse Public License](http://opensource.org/licenses/eclipse-1.0.php), the same as Clojure.