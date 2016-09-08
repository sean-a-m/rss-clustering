(ns GraphNamedThings.util
  (require [clojure.zip :as zip])
  (require digest))

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


