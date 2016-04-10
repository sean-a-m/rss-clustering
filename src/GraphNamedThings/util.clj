(ns GraphNamedThings.util)

(defn in? [elm coll]
  (some #(= elm %) coll))

;TODO: If I have to use this then something is probably wrong
(defn to-lists [i]
 (map #(list %) i))

(to-lists 3)
