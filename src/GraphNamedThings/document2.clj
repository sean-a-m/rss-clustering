(ns GraphNamedThings.document2
  (:require [GraphNamedThings.entity :as entity]
            [loom.graph :as graph]
            [clojure.set :as s]
            [clj-uuid :as uuid]
            [clojure.core.reducers :as r]
            [loom.alg]
            [loom.graph :as graph]
            [medley.core :as me]
            [GraphNamedThings.dbio :as dbio]
            [clojure.math.combinatorics :as combo]))



(defn matches? [coll-item string id]
  "Uses exclusive or since the only item in the collection that matches both the string and the id is the item itself"
  (or (contains? (:strings (val coll-item)) string)
      (contains? (:ids (val coll-item)) id)))

(defn match-entity [matched-coll ent-rec]
  "matched-coll is a list of {:strings #{} :ids #{}}"
  (or (first (filter #(matches? % (:entstring ent-rec) (:id ent-rec)) matched-coll))
      (first (hash-map (uuid/v1) (hash-map :strings #{} :ids #{})))))

(defn merge-matched-entity [matching matched-coll ent-rec]
  (assoc matched-coll  (key matching)
       (hash-map :strings (s/union (:strings (val matching)) #{(:entstring ent-rec)})
                 :ids (s/union (:ids (val matching)) #{(:id ent-rec)}))))

(defn merge-entity [matched-coll ent-rec]
  (let [matching (match-entity matched-coll ent-rec)]
    ;uses first since I accidentally defined a map instead of a map entry
        (merge-matched-entity matching matched-coll ent-rec)))

(defn gather-entities [ent-recs]
  (reduce merge-entity {} ent-recs))

(defn gather-entities2 [ent-recs]
  (loop [ents ent-recs ent-coll {}]
    (if (not-empty ents)
      (recur (rest ents) (merge-entity ent-coll (first ents)))
      ent-coll)))

(defn make-graph [ent-recs]
  (let [ent-docid-idx (zipmap (map :id ent-recs) (map :docid ent-recs))
        ent-set (gather-entities2 ent-recs)]
    (apply graph/weighted-graph
     (apply concat
      (for [ent-ids (vals ent-set)]
        (let [docid-set (distinct (map #(get ent-docid-idx %) (:ids ent-ids)))]
          (combo/combinations docid-set 2)))))))


(defn get-doc-source-index [doc-records]
      (zipmap (map :id doc-records) (map :id_feed doc-records)))

(defn collect-multiples [ent-recs]
  ;(filter #(< 1 (count (val %))) (group-by :id ent-recs)))
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

(defn connect-doc-set-filter
  [ent-recs]
  (let [doc-records (dbio/get-doc-sources (map :docid ent-recs))
        ds-idx (get-doc-source-index doc-records)]
    (filter #(not= (get ds-idx (first %)) (get ds-idx (last %)))
      (combo/combinations
        (distinct
          (map :docid ent-recs)) 2))))

(defn connect-doc-set
  [ent-recs]
 ; (let [doc-records (dbio/get-doc-sources (map :docid ent-recs))
 ;       ds-idx (get-doc-source-index doc-records)]
            (combo/combinations
              (distinct
                (map :docid ent-recs)) 2))

(defn connect-doc-set2
  [ent-recs]
  ; (let [doc-records (dbio/get-doc-sources (map :docid ent-recs))
  ;       ds-idx (get-doc-source-index doc-records)]
  (combo/combinations
    (me/distinct-by :docid ent-recs) 2))


(defn connect-docs
  [ent-recs]
  "from connected entities"
  (let [connected-ents (connect-ents ent-recs)]
    (mapcat connect-doc-set connected-ents)))

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

(defn format-graph2 [item]
  (vector
    (first (key item))
    (second (key item))
    (reduce + (map last (val item)))))

(defn connect-docs2
  [ent-recs]
  "from connected entities"
  (let [connected-ents (connect-ents ent-recs)
        string-counts (reduce #(assoc %1 (:entstring %2) (:count %2)) {} (dbio/get-string-counts (map :entstring ent-recs)))
        total-count (reduce + (vals string-counts))]
    (->> ent-recs
         (connect-ents)
         (mapcat connect-doc-set2)
         (map (partial build-vector-weight-pairs string-counts total-count))
         (group-by first)
         (map format-graph2))))

    ;(mapcat connect-doc-set2 connected-ents)))

(defn format-graph [set-weight-pair]
  (let [[edge weight] set-weight-pair]
    (vector (first edge) (second edge) weight)))



(defn weight-docs [ent-recs]
  (map format-graph
    (frequencies
      (map (partial into #{}) (connect-docs ent-recs)))))

(defn add-doc-entry
  [collected-strings doc-idx ent-rec]
  (let [containing-set (first (filter #(contains? % (:entstring ent-rec)) collected-strings))]
    (update doc-idx (:docid ent-rec) conj containing-set)))

;return a map of documents to merged strings
(defn merge-docs
  [ent-recs]
  (let [collected-strings (map set (connected-components ent-recs))]
  (reduce (partial add-doc-entry collected-strings) ent-recs)))


