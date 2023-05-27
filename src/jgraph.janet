(import ./graphs :prefix "" :export true)
(import ./algs :prefix "" :export true)

# The following `setdyn`s manually remove certain bindings 
# from the imported environment table. This is equivalent
# to marking the bindings below as `:private`, but with
# the advantage of still being able to `import` them for 
# testing purposes.
# 
# This would be unnecessary if Janet could selectively
# import bindings from modules.

### From `graphs.janet`

(setdyn 'Graph nil)
(setdyn 'graph-schema nil)
(setdyn 'unweighted-edge-schema nil)
(setdyn 'weighted-edge-schema nil)
(setdyn 'edge-schema nil)
(setdyn 'node-schema nil)

(setdyn 'src nil)
(setdyn 'dest nil)
(setdyn 'weight nil)

(setdyn 'defgraph nil)
(setdyn 'make-digraph! nil)
(setdyn 'make-weighted! nil)
(setdyn 'build-graph nil)

### From `algs.janet`

(setdyn 'remove nil)
(setdyn 'iterate nil)