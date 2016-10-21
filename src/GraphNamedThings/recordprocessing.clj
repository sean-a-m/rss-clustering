(ns GraphNamedThings.recordprocessing
  (:require [GraphNamedThings.inputs :as inputs]
            [GraphNamedThings.dbio :as diskio]
            [GraphNamedThings.document :as document]
            [clojure.set :as cset]
            [taoensso.nippy :as nippy]
            [clojure.data.json :as json]
            [korma.db :refer :all]
            [korma.core :refer :all]))

;tags that shouldn't be used for classifying documents
(def excluded-tags ["DATE" "NUMBER" "ORDINAL" "MISC" "MONEY" "DURATION" "TIME" "PERCENT" "SET"])

(defn- build-and-write-new-entity-records
  "Create new entity records from a list of ids and write them to a database"
  [ids pipe]
  (let [doc-content (diskio/doc-content-by-id ids)
        entities (inputs/get-entity-lists (vals doc-content) pipe)
        entities-json (map json/write-str entities)
        id-entity-list (zipmap (keys doc-content) entities)
        id-entity-list-json (map #(sorted-map :id %1 :val %2) (keys doc-content) entities-json)]
    (do
      (insert diskio/entities
              (values id-entity-list-json))
      id-entity-list)))

(defn id-mapping
  "Get ID from a document from the database"
  [data-item]
  (:id data-item))

(defn title-mapping
  "Get title from a document in the database"
  [data-item]
  (:title data-item))

(defn content-mapping
  "Get text content from a document in the database"
  [data-item]
  (diskio/doc-content data-item))

(defn entity-record-mapping
  "Takes set of entity records and individual document item and returns the entity records corresponding to that document"
  [entity-records data-item]
  (filter #(every? (partial not= (:ner-tag %)) excluded-tags)
          (get entity-records (:id data-item))))

(defn create-entity-records
  "Return the list of entity records from a list of document database records.  Requires NLP pipeline object for processing new document records"
  [pipe data]
  (let [document-ids (map :id data)
        existing-entity-records (diskio/select-id-list document-ids)
        existing-rec-ids (into #{} (map :id existing-entity-records))
        pending-rec-ids (cset/difference (into #{} document-ids) existing-rec-ids)
        kv-recs-existing (zipmap (map :id existing-entity-records) (map #(json/read-str (str (:val %)) :key-fn keyword) existing-entity-records))  ;records that are already in the database
        kv-recs-pending (if (seq pending-rec-ids)                                             ;records that still need to be built
                          (build-and-write-new-entity-records pending-rec-ids pipe)
                          nil)]
    (merge kv-recs-existing kv-recs-pending)))

(defn create-document-records
    [nlp-pipe documents]
    (let [entity-records (create-entity-records nlp-pipe documents)
          entity-mapping (partial entity-record-mapping entity-records)]
      (map (partial document/create-document-record id-mapping title-mapping content-mapping entity-mapping) documents)))

(defn create-document-records-batched-by-id
  "Create a set of document records given a set of document ids corresponding to database record, an NLP pipleine object, and the size of the document batches to process"
  [nlp-pipe batch-size doc-ids]
  (let [id-batches (partition-all batch-size doc-ids)]
    (mapcat #(create-document-records nlp-pipe (diskio/docs-by-id %)) id-batches)))

(defn create-document-records-batched
  "Create a set of document records given a set of document ids corresponding to database record, an NLP pipleine object, and the size of the document batches to process"
  [nlp-pipe batch-size docs]
  (let [doc-batches (partition-all batch-size docs)]
    (mapcat (partial create-document-records nlp-pipe) doc-batches)))
