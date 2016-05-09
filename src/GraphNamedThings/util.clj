(ns GraphNamedThings.util
  (require [clojure.zip :as zip]))


(defn in? [elm coll]
  (some #(= elm %) coll))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn abs [n] (max n (- n)))

(defn count-seqs [coll]
  (count
    (filter seq? coll)))

(defn adjacent? [a b]
  (= 1 (abs (- a b))))

;returns which nth list within a list an element belongs to
(defn nested-index [elm]
  (let [n-elm (zip/next elm)]
    (cond
      (zip/end? n-elm) nil
      (= 2 (count (zip/path n-elm)))
        (cons (count-seqs (-> n-elm zip/up zip/lefts)) (nested-index n-elm))
      :else (nested-index n-elm))))





