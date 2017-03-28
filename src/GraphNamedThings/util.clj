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

(defn select-keys-trans
  "A way to select-key from a transient map, probably not a good idea for some reason"
  [m ks]
  (apply merge (map #(hash-map % (get m %)) ks)))

