;Data processing functions specific to a type of database or document source

(ns GraphNamedThings.dbio
  (:require [clojure-csv.core :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clj-time.core :as t]
            [clj-time.coerce :as coerce]
            [korma.db :refer :all]
            [clojure.data.json :as json]
            [taoensso.nippy :as nippy]
            [korma.core :refer :all]
            [clj-uuid :as uuid]
            [GraphNamedThings.config :as config])
  (:import org.jsoup.Jsoup))

;defines one group of related document, where group-id can be an arbitrary id and doc-ids is a collection of document ids
(defrecord doccluster [group-id doc-ids])

(def psqldb
  {:classname   "org.postgresql.Driver"
   :subprotocol "postgresql"
   :subname     config/output-db-path
   :user        config/psql-user
   :password    config/psql-pass
   :stringtype "unspecified"
   })

(defdb mydb psqldb)

(defentity entry
           (database psqldb)
           (entity-fields :id :title :link :date :content :id_feed))

(defentity entry-ids
           (database psqldb)
           (entity-fields :id)
           (table :entry))

(defentity processlog
           (database mydb)
           (entity-fields :id :success :ver))

(defentity processlog-ids
           (database mydb)
           (table :processlog)
           (entity-fields :id))

(defentity namedentities
           (database mydb)
           (entity-fields :id :tag :docid))

(defentity strings
           (database mydb)
           (belongs-to namedentities)
           (entity-fields :id :entstring :count))


(defn parse-html-fragment
  "Assumes the text is always part of the document body"
  [html-fragment]
  (let [jsoup-doc  (. Jsoup parseBodyFragment html-fragment)]
    (.text
      (.body jsoup-doc))))

(defn docs-by-id
  "Select input documents by ID"
  [id-list]
  (select entry
          (where {:id [in id-list]
                  :id_feed [in config/selected-feed-ids]})))

(defn select-newest-unprocessed [batch-size]
  (select entry-ids
      (where (and (not (in :id (subselect processlog-ids)))
              (in :id_feed config/selected-feed-ids)))
          (limit batch-size)
          (order :id :DESC)))

(defn processed-docs-from-time-range
  "Time is epoch in  frss table"
  [start-time end-time]
  (select entry
          (where {:date [between [start-time end-time]]
                  :id_feed [in config/selected-feed-ids]
                  :id [in (subselect processlog-ids)]})))

(defn log-result
  [doc-id success? version]
  (insert processlog
          (values [{:id doc-id :success success? :ver version}])))

(defn doc-content-with-title
  "Returns title + content (parsed from html) for a given document entry as returned by docs-by-id"
  [doc-entry]
  (str/join ". " (list
                         (:title doc-entry)
                         (parse-html-fragment (str/join (list " " (:content doc-entry))))))) ;adding a space is a hack to stop jsoup from crashing and should be handle smarter somehow

(defn doc-content
  "Returns title + content (parsed from html) for a given document entry as returned by docs-by-id"
  [doc-entry]
    (parse-html-fragment (:content doc-entry)))

(defn doc-content-by-id
  "List of document ID's (contained in database) -> list of document contents "
  [id-list]
  (into {} (map #(vector (:id %) (doc-content %)) (docs-by-id id-list))))

(defn get-entity-records [docids]
  (select namedentities
          (fields :strings.entstring)
          (where {:docid [in docids]})
          (join strings (= :strings.id :id))))

(defn write-entities [entity-records string-lists]
  (transaction
    (if (seq entity-records)
      (insert namedentities
             (values entity-records)))
    (if (seq string-lists)
      (insert strings
              (values string-lists)))))

