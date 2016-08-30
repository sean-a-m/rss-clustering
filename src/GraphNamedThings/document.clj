(ns GraphNamedThings.document
  (require [GraphNamedThings.util :as util]
           [GraphNamedThings.inputs :as inputs]
           [GraphNamedThings.entity :as entity]
           [GraphNamedThings.diskio :as diskio]
           [loom.graph :as graph]
           [clojure.math.combinatorics :as combo]
           [clojure.java.jdbc :as sql]
           [clojure.set :as cset]
           [taoensso.nippy :as nippy]
           [korma.db :refer :all]
           [korma.core :refer :all]))


(defrecord doc-rec [id title text entities])

(defn build-and-write-new-entity-records
  "Create new entity records from a list of ids and write them to a database"
  [ids pipe]
  (let [doc-content (diskio/doc-content-by-id ids)
        entities (inputs/get-entity-lists (vals doc-content) pipe)
        entities-serialized (map nippy/freeze entities)
        id-entity-list (zipmap (keys doc-content) entities)
        id-entity-list-serialized (map #(sorted-map :k %1 :v %2) (keys doc-content) entities-serialized)]
    (do
      (insert diskio/entitytest
              (values id-entity-list-serialized))
      id-entity-list)))

(defn get-ent-record-list
  "Get list of document records from text ids"
  [ids pipe]
  (let [db-records (diskio/select-id-list ids)
        existing-rec-ids (into #{} (map :k db-records))
        pending-rec-ids (cset/difference (into #{} ids) existing-rec-ids)
        kv-recs-existing (zipmap (map :k db-records) (map #(nippy/thaw (:v %)) db-records))
        kv-recs-pending (if (seq pending-rec-ids)
                          (build-and-write-new-entity-records pending-rec-ids pipe)
                          nil)]
    (merge kv-recs-existing kv-recs-pending)))

(defn create-document-record
  [id-entity-pairs document]
  (->doc-rec
    (:ID document)
    (:TITLE document)
    (diskio/doc-content document)
    (filter #(every? (partial not= (:ner-tag %)) ["DATE" "NUMBER" "ORDINAL" "MISC" "MONEY" "DURATION"])
            (get id-entity-pairs (:ID document)))))


(defn create-document-records
  "Creating a document record may be slow since it calls the nlp library for records not already in the database"
  [ids nlp-pipe]
  (let [documents (diskio/docs-by-id ids)
        id-entity-pairs (get-ent-record-list (map :ID documents) nlp-pipe)]
    (map (partial create-document-record id-entity-pairs) documents)))

(defn create-document-records-split
  "Call the create document records method, splitting into separate batches of ID's first"
  [ids nlp-pipe batch-size]
  (let [id-batches (partition-all batch-size ids)]
    (mapcat #(create-document-records % nlp-pipe) id-batches)))


(defn get-entities
  "Get real entities from a list of entities"
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

