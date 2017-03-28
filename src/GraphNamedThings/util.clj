(ns GraphNamedThings.util)

(defn tails
  [xs]
  (lazy-seq
    (if (< 0 (count xs))
      (cons xs (tails (rest xs)))
      nil)))

(defn in?
  "Is an element in a collection?"
  [elm coll]
  (some #{elm} (into () coll)))
