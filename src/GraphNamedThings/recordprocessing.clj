(ns GraphNamedThings.recordprocessing
  (:require [GraphNamedThings.entities :as inputs]
            [GraphNamedThings.dbaccess :as dbio]
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]))

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
      (catch Exception e
        (error e)
        (dbio/log-result id false))
      (finally (dbio/log-result id true)))))

(defn build-and-write-new-entity-records
  "Create new entity records from a list of ids and write them to a database"
  [ids nlp-pipe]
  (map #(build-and-write-new-entity-record % nlp-pipe) ids))

(defn- merge-map [coll]
  (let [strings (mapcat :entstring coll)]
    (assoc (first coll) :entstring strings)))




