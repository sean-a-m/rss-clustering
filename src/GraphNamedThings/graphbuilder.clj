(ns GraphNamedThings.graphbuilder
  (:require [loom.alg]
            [medley.core :as me]
            [GraphNamedThings.dbio :as dbio]
            [clojure.math.combinatorics :as combo]))

(defn collect-multiples [ent-recs]
   (group-by :id ent-recs))

(defn get-string-pairs [ent-recs]
  (let [id-coll (collect-multiples ent-recs)]
    (mapcat #(combo/combinations (map :entstring (val %)) 2) id-coll)))

(defn graph-multiples  [ent-recs]
  (apply loom.graph/graph
         (concat
           (get-string-pairs ent-recs)
           (map :entstring ent-recs))))   ;ensure that lone strings are also included in the graph

(defn connected-components [ent-recs]
  (let [multiples (graph-multiples ent-recs)]
    (loom.alg/connected-components multiples)))

(defn collect-ents-on-strings [ent-recs string-set]
  (filter #(contains? string-set (:entstring %)) ent-recs))

(defn connect-ents [ent-recs]
  (let [connected-sets (map set (connected-components ent-recs))]
    (pmap (partial collect-ents-on-strings ent-recs) connected-sets)))

(defn connect-doc-set2
  [ent-recs]
  (combo/combinations
    (me/distinct-by :docid ent-recs) 2))

(defn build-vector-weight-pairs [string-counts total-count entity-pair]
  ;TODO: calculate total in this function
  (let [[first-ent second-ent] entity-pair]
    (vector
      (into #{}
            (list
              (:docid first-ent)
              (:docid second-ent)))
      (if (= (:entstring first-ent) (:entstring second-ent))
        (/ total-count (get string-counts (:entstring first-ent)))
        (/ total-count (+ (get string-counts (:entstring first-ent)) (get string-counts (:entstring second-ent))))))))

(defn format-graph [item]
  (vector
    (first (key item))
    (second (key item))
    (reduce + (map last (val item)))))

(defn connect-docs
  [ent-recs]
  "from connected entities"
  (let [string-counts (reduce #(assoc %1 (:entstring %2) (:count %2)) {} (dbio/get-string-counts (map :entstring ent-recs)))
        total-count (reduce + (vals string-counts))]
    (->> ent-recs
         (connect-ents)
         (mapcat connect-doc-set2)
         (map (partial build-vector-weight-pairs string-counts total-count))
         (group-by first)
         (map format-graph))))




