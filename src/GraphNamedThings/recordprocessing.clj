(ns GraphNamedThings.recordprocessing
  (:require [GraphNamedThings.inputs :as inputs]
            [GraphNamedThings.dbio :as diskio]
            [GraphNamedThings.util :as util]
            [GraphNamedThings.document :as document]
            [clojure.set :as cset]
            [GraphNamedThings.entity :as e]
            [taoensso.nippy :as nippy]
            [clojure.data.json :as json]))

;tags that shouldn't be used for classifying documents
(def excluded-tags ["DATE" "NUMBER" "ORDINAL" "MISC" "MONEY" "DURATION" "TIME" "PERCENT" "SET" "NULL"])

(defn entityjson
  "Using the old (k: document-id v: [entity records]) collection, create a list of (k: sha256 hash v: {:doc-id :ner-tag :strings[]}) records to write to databse"
  [id-entity-list-item]
  (if (not-empty (val id-entity-list-item))
    (map #(sorted-map :id (util/sha256-bytes (clojure.string/join (flatten (list (:strings %) (str (rand-int 90000)) (str (key id-entity-list-item))))))
                       :val (json/write-str {:doc-id (key id-entity-list-item) :ner-tag (:ner-tag %) :strings (:strings %)})) (val id-entity-list-item))
    (map #(sorted-map :id (util/sha256-bytes (clojure.string/join (flatten (list % (str (rand-int 90000)) (str (key id-entity-list-item))))))
                      :val (json/write-str {:doc-id (key id-entity-list-item) :ner-tag "NULL" :strings %})) (list ""))))


;(defn- build-and-write-new-entity-records
 ; "Create new entity records from a list of ids and write them to a database"
 ; [ids pipe]
 ; (let [doc-content (diskio/doc-content-by-id ids)
 ;       entities (inputs/get-entity-lists (vals doc-content) pipe)
 ;       entities-json (map json/write-str entities)
 ;       id-entity-list (zipmap (keys doc-content) entities)
 ;       id-entity-list-json (map #(sorted-map :id %1 :val %2) (keys doc-content) entities-json)
 ;       entity-string-table (mapcat entityjson id-entity-list)]
 ;   (do
 ;     (println entity-string-table)
 ;     (if (not-empty entity-string-table)
 ;     (insert diskio/entities
 ;             (values id-entity-list-json))
 ;       (insert diskio/entitystrings
 ;               (values entity-string-table)))
 ;     id-entity-list)))

(defn build-entity-string-list [entity]
  "Build a list of entity strings from an entity record"
  (let [string-freqs (frequencies (:strings entity))]
    (map #(hash-map :entstring (key %) :count (val %) :id (:id entity)) string-freqs)))

(defn build-entity-record [doc-id entity]
  (-> entity
    (dissoc :strings)
    (assoc :docid doc-id)))

(defn build-and-write-new-entity-record
  "Create new entity records from a list of ids and write them to a database"
  [id nlp-pipe]
  (let [doc-text (val (first (diskio/doc-content-by-id (list id))))
        entity-list (inputs/get-entity-list doc-text nlp-pipe)
        entity-string-list (flatten (map build-entity-string-list entity-list))
        entity-record-list (map (partial build-entity-record id) entity-list)]
    (try
        (diskio/write-entities entity-record-list entity-string-list)
        (catch Exception e (diskio/log-result id false "1"))
        (finally (diskio/log-result id true "1")))))

(defn build-and-write-new-entity-records
  "Create new entity records from a list of ids and write them to a database"
  [ids nlp-pipe]
  (map #(build-and-write-new-entity-record % nlp-pipe) ids))


;   (do
 ;     (println entity-string-table)
 ;     (if (not-empty entity-string-table)
 ;       ;     (insert diskio/entities
 ;       ;             (values id-entity-list-json))
 ;       (insert diskio/entitystrings
 ;               (values entity-string-table)))
 ;     id-entity-list)))

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

(defn return-existing-entity-records
  [id-list]
  (->> id-list
       (map str)
       (diskio/select-entities-by-doc-id)
       (map #(json/read-str (str (:val %)) :key-fn keyword))
       (group-by :doc-id)))


(defn create-entity-records-old
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

(defn create-entity-records
  "Return the list of entity records from a list of document database records.  Requires NLP pipeline object for processing new document records"
  [pipe data]
  (let [document-ids (map :id data)
        existing-entity-records (return-existing-entity-records document-ids)
        existing-rec-ids (into #{} (keys existing-entity-records))
        pending-rec-ids (
                          cset/difference (into #{} document-ids) existing-rec-ids)
        kv-recs-existing existing-entity-records  ;records that are already in the database
        kv-recs-pending (if (seq pending-rec-ids)                                             ;records that still need to be built
                          (build-and-write-new-entity-records pending-rec-ids pipe)
                          nil)]
    (merge kv-recs-existing kv-recs-pending)))



(defn create-document-records
    [nlp-pipe documents]
    (let [entity-records (create-entity-records nlp-pipe documents)
          entity-mapping (partial entity-record-mapping entity-records)]
      (map (partial document/create-document-record id-mapping title-mapping content-mapping entity-mapping) documents)))

(defn- merge-map [coll]
  (let [strings (mapcat :entstring coll)]
    (assoc (first coll) :entstring strings)))

(defn merge-maps [coll]
  (->> coll
       (map #(update % :entstring list))
       (group-by :id)
       (vals)
       (map merge-map)))

(defn get-entity-doc-vector
  [entity-coref-map entity-record]
  (let [resolved-entity-record (e/get-entity-id entity-coref-map entity-record)
        doc-id (:docid entity-record)]
    (vector resolved-entity-record doc-id)))

(defn get-entity-doc-vectors
  [entity-coref-map entity-records]
  (map (partial get-entity-doc-vector entity-coref-map) entity-records))

(defn create-entity-records-newish
  [ids]
  (let [db-entities (diskio/get-entity-records ids)]
    (->> db-entities
         (filter #(every? (partial not= (:tag %)) excluded-tags))
        (merge-maps)
        (map #(clojure.set/rename-keys % {:entstring :strings, :tag :ner-tag})))))

(defn create-document-records-newish
  [ids]
  (let [entity-records (create-entity-records-newish ids)
        entity-coref-map (e/get-entity-merge-map {} entity-records)]
    (get-entity-doc-vectors entity-coref-map entity-records)))

(defn create-entity-records-from-documents
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




