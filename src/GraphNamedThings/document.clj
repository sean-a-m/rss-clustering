(ns GraphNamedThings.document
  (:require [loom.graph :as graph]
            [clojure.math.combinatorics :as combo]))


(defn set-to-loom-vector
  [esw]
  (let [edges (first esw)
        weight (second esw)]
    (vector (first edges) (second edges) weight)))

(defn entity-freqs
  "ent-doc-map is the result of (ent-doc-sets doc-recs)"
  [ent-doc-map]
  (frequencies (map first ent-doc-map)))

(defn edges-from-shared-entity [ids freq]
  (let [combos (combo/combinations ids 2)]
    (if (< 1 (count ids))
      (map #(vector (set (list (first %) (second %))) (/ 1 freq)) combos) ;take the inverse of the frequency to weight towards less common edges
      nil)))

(defn- update-edge-sum
  [m c]
  (let [[edge weight] c]
    (update m edge (fnil #(+ weight %) 0))))

(defn add-like-edges
  [weighted-edges]
    (reduce update-edge-sum {} weighted-edges))

(defn create-graph-edges
  [ent-doc-sets]
  (let [grouped (group-by first ent-doc-sets)
        ef (entity-freqs ent-doc-sets)
        shared-entity-sets (zipmap (keys grouped) (map #(map second %) (vals grouped)))]
    (apply concat (map #(edges-from-shared-entity (val %) (get ef (key %))) shared-entity-sets))))

(defn create-graph-new
  [graph-edges]
  (let [wg (graph/weighted-graph)]
    (reduce graph/add-edges wg graph-edges)))

(defn create-document-graph
  [doc-recs]
  (->> doc-recs
       (create-graph-edges)
       (add-like-edges)
       (map set-to-loom-vector)
       (create-graph-new)))