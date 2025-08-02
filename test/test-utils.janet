(defn assert-docs
  ```
  Assert that all symbols, when module on the path is required,
  have proper doc string
  ```
  [path]
  (loop [[sym val] :pairs (require path)
         :when (and (symbol? sym)
                    (not (val :private))
                    (not (val :ref))
                    (not (val :is-private)))]
    (assert (and (val :doc)
                 (peg/match '(* (+ (* "(" (thru ")\n\n"))
                                   (not "("))
                                (some 1) -1)
                            (get val :doc "")))
            (string sym " does not have proper doc"))))
