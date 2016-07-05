(ns GraphNamedThings.document
  (require [GraphNamedThings.util :as util]
           [GraphNamedThings.inputs :as inputs]
           [GraphNamedThings.entity :as entity]
           [loom.graph :as graph]
           [clojure.math.combinatorics :as combo]))


(defrecord doc-rec [title text entities])

(defn doc-title
  "This is just a placeholder for returning the document title from whatever data source will be used later"
  []
  (util/uuid!))

(defn create-document-record
  "Creating a document record is sort of slow since it calls the nlp library"
  [title text nlp-pipe]
  (->doc-rec
    title
    text
    ;Filter undesired tags out
    ;TODO: move this somewhere more appropriate
    (filter #(every? (partial not= (:ner-tag %)) ["DATE" "NUMBER" "ORDINAL"])
      (inputs/token-entities text nlp-pipe))))

(defn get-entities
  "Get real entities from a list of entities"
  [ent-coref-map entities]
  (map (partial entity/get-entity-id ent-coref-map) entities))

(defn item-entry
  [doc-rec entity]
  [entity (:title doc-rec)])

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

;TODO: this name is too lnog
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
