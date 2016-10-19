(ns GraphNamedThings.util
  (:require digest))

(defn tails
  [xs]
  (lazy-seq
    (if (< 0 (count xs))
      (cons xs (tails (rest xs)))
      nil)))

(defn uuid! []
  "Create a java uuid"
  (str (java.util.UUID/randomUUID)))

(defn in?
  "Is an element in a collection?"
  [elm coll]
  (some #{elm} (into () coll)))

(defn select-keys-trans
  "A way to select-key from a transient map, probably not a good idea for some reason"
  [m ks]
  (apply merge (map #(hash-map % (get m %)) ks)))
