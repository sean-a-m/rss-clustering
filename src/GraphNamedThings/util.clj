(ns GraphNamedThings.util
  (require [clojure.zip :as zip])
  (require digest))


(defn in?
  "Is an element in a collection?"
  [elm coll]
  (some #(= elm %) coll))

(defn uuid! []
  "Create a java uuid"
  (str (java.util.UUID/randomUUID)))

(defn abs
  "Absolute value"
  [n]
  (max n (- n)))

(defn count-seqs [coll]
  "Returns number of collections the next level down in a collection"
  (count
    (filter seq? coll)))

(defn adjacent?
  "True if A and B are adjacent integers"
  [a b]
  (= 1 (abs (- a b))))

(defn hash-string
  "MDF hash of string"
  [string]
  (digest/md5 string))

(defn nested-index
  "returns which nth list within a list an element belongs to"
  [elm]
  (let [n-elm (zip/next elm)]
    (cond
      (zip/end? n-elm) nil
      (= 2 (count (zip/path n-elm)))
        (cons (count-seqs (-> n-elm zip/up zip/lefts)) (nested-index n-elm))
      :else (nested-index n-elm))))

(defn true-or-fn
  "Return value if comparison is true, otherwise return value of function
  TODO: remember what this is called"
  [value comp-fn false-fn]
  (if (comp-fn value)
    value
    false-fn))






