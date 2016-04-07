(ns GraphNamedThings.util)

(defn in?
  [elm coll]
  (some #(= elm %) coll))
