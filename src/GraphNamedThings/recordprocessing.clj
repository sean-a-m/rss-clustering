(ns GraphNamedThings.recordprocessing
  (:require [GraphNamedThings.inputs :as inputs]
            [GraphNamedThings.dbio :as dbio]
            [GraphNamedThings.entity :as e]))

;tags that shouldn't be used for classifying documents
(def excluded-tags ["DATE" "NUMBER" "ORDINAL" "MISC" "MONEY" "DURATION" "TIME" "PERCENT" "SET" "NULL"])

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
  (let [doc-text (val (first (dbio/doc-content-by-id (list id))))
        entity-list (inputs/get-entity-list doc-text nlp-pipe)
        entity-string-list (flatten (map build-entity-string-list entity-list))
        entity-record-list (map (partial build-entity-record id) entity-list)]
    (try
      (dbio/write-entities entity-record-list entity-string-list)
      (catch Exception e (dbio/log-result id false))
      (finally (dbio/log-result id true)))))

(defn build-and-write-new-entity-records
  "Create new entity records from a list of ids and write them to a database"
  [ids nlp-pipe]
  (map #(build-and-write-new-entity-record % nlp-pipe) ids))

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

(defn create-entity-records
  [ids]
  (let [db-entities (dbio/get-entity-records ids)]
    (->> db-entities
         (filter #(every? (partial not= (:tag %)) excluded-tags))
        (merge-maps)
        (map #(clojure.set/rename-keys % {:entstring :strings, :tag :ner-tag})))))

(defn create-document-records
  [ids]
  (let [entity-records (create-entity-records ids)
        entity-coref-map (e/get-entity-merge-map {} entity-records)]
    (get-entity-doc-vectors entity-coref-map entity-records)))


