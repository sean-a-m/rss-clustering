(ns GraphNamedThings.document
  (:require [GraphNamedThings.entity :as entity]
           [loom.graph :as graph]
           [clojure.math.combinatorics :as combo]))


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

