(ns GraphNamedThings.document
  (:require [GraphNamedThings.entity :as entity]
            [loom.graph :as graph]
            [clojure.set :as cset]
            [clojure.math.combinatorics :as combo])
  (:import (javax.sound.midi MidiMessage)))


(defrecord doc-rec [id title text entities])


(defn create-document-record
  "Create a document record given document data and functions to map data to id, title, text, and entity records"
  [id-map title-map text-map entity-record-map data]
  (->doc-rec
    (id-map data)
    (title-map data)
    (text-map data)
    (entity-record-map data)))

(defn create-document-records
  "Map set of data to create document records"
  [id-map title-map text-map entity-record-map data]
  (map (partial create-document-record
                id-map
                title-map
                text-map
                entity-record-map) data))

(defn get-entities
  "Map a list of entities to entity IDs"
  [ent-coref-map entities]
  (map (partial entity/get-entity-id ent-coref-map) entities))

(defn item-entry
  [doc-rec entity]
  [entity (:id doc-rec)])

(defn ent-doc-set
  "Returns the set of all entity-document relations for one document"
  [ent-coref-map doc-rec]
  (let [entities (get-entities ent-coref-map (:entities doc-rec))]
    (map (partial item-entry doc-rec) entities)))

(defn ent-doc-sets
  "Returns the set of all entity-document relations"
  [doc-recs]
  (let [ent-coref-map (entity/get-entity-merge-map {} (flatten (map :entities doc-recs)))]
    (into #{} (mapcat (partial ent-doc-set ent-coref-map) doc-recs))))

(defn set-to-loom-vector
  [esw]
  (let [edges (first esw)
        weight (second esw)]
    (vector (first edges) (second edges) weight)))

(defn entity-freqs
  "Map pointing from entity (resolved) to frequency in set of documents"
  [doc-recs grouped]
    (zipmap (keys grouped) (map count (vals grouped))))

(defn edges-from-shared-entity [ids freq]
  (let [combos (combo/combinations ids 2)]
    (if (< 1 (count ids))
    ;  (map #(vector (first %) (second %) (/ 1 freq)) combos) ;take the inverse of the frequency to weight towards less common edges
      (map #(vector (set (list (first %) (second %))) (/ 1 freq)) combos) ;take the inverse of the frequency to weight towards less common edges
      nil)))

(defn add-like-edges
  "this takes an argument [#{e1 e2} weight] to sum edges with matching #{e1 e2}'s"
  [weighted-edges]
  (for [distinct-edge (distinct (map first weighted-edges))]  ;for each edge with a distinct set of vertices in the set of all edges...
    (let [edge-set (filter #(= (first %) distinct-edge) weighted-edges) ;select all edges with that vertex
          edge-sum (reduce + (map second edge-set))]  ;sum the weights (second element in the vector)
      (vector distinct-edge edge-sum))))



(defn create-graph-edges
  [doc-recs]
  (let [grouped (group-by first (ent-doc-sets doc-recs))
        ef (entity-freqs doc-recs grouped)
        shared-entity-sets (zipmap (keys grouped) (map #(map second %) (vals grouped)))]
    (apply concat (map #(edges-from-shared-entity (val %) (get ef (key %))) shared-entity-sets))))

(defn create-graph-new
  [graph-edges]
  (let [wg (graph/weighted-graph)]
    (reduce graph/add-edges wg graph-edges)))


;(defn add-like-edges
;  "Sum edges with the same nodes in a weighted graph"
;  [g]
;  (for [group (:adj g)]
;    (let [conns (val group)
;          dstnct-conns (distinct (keys conns))]
;      dstnct-conns)))

(defn create-document-graph-alt
  [doc-recs]
  (->> doc-recs
       (create-graph-edges)
       (add-like-edges)
       (map set-to-loom-vector)
       (create-graph-new)))

;TODO: Deal with nodes with no connection

(defn nodes-from-set
  "Return all pairs of connected nodes (based on shared entities)"
  [doc-tuple]
  (let [doc-set (map second (second doc-tuple))]
  (if (< 1 (count doc-set ))
    (combo/combinations doc-set 2)
    doc-set)))

;TODO: this name is too long
(defn node-freq-to-weighted-arg-vec
  "Turn a (first second) frequency or item frequency into a [first second weight] vector to pass to loom"
  [item]
  ;TODO: figure out how to handle isolated nodes
  (let [[maybe-pair weight] item]
    (if (seq? maybe-pair)
      (vector (first maybe-pair) (second maybe-pair) weight))))

(defn create-document-graph
  "Create a graph where nodes represent document records and edges represent shared entities"
  [doc-recs]
  (apply graph/weighted-graph
         ;node-freq-to-weighted-arg-vec returns nil on unconnected items and I'm not sure what that will do to the graph algorithms yet
    (remove nil?
      (map node-freq-to-weighted-arg-vec
        ;Generate a list of document pairs with frequencies counts, which will be used as weights
        (frequencies
          (mapcat nodes-from-set
          ;Entity - [Entity, Document Record] kv pair
            (group-by first (ent-doc-sets doc-recs))))))))

