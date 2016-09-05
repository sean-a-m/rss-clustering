(ns GraphNamedThings.util
  (require [clojure.zip :as zip])
  (require digest))


(defn in?
  "Is an element in a collection?"
  [elm coll]
  (some #{elm} (into () coll)))

(defn uuid! []
  "Create a java uuid"
  (str (java.util.UUID/randomUUID)))

(defn abs
  "Absolute value"
  [n]
  (max n (- n)))

(defn true-or-fn
  "Return value if comparison is true, otherwise return value of function
  TODO: remember what this is called"
  [value comp-fn false-fn]
  (if (comp-fn value)
    value
    false-fn))

(defn tails
  [xs]
  (lazy-seq
    (if (< 0 (count xs))
      (cons xs (tails (rest xs)))
      nil)))

