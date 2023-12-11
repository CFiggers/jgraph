(declare-project 
  :name "jgraph"
  :description "TODO: Write a cool description"
  :dependencies ["https://github.com/ianthehenry/judge"
                 "https://github.com/janet-lang/spork"])

(declare-source
  :prefix "jgraph"
  :source ["src/init.janet" 
           "src/algs.janet"
           "src/graphs.janet"])