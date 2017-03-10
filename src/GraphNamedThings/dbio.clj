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
            [clj-postgresql.core :as pg]
            [clojure.java.jdbc :as jdbc]
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

(def db2 (pg/pool :host config/dbhost :user config/psql-user :dbname config/dbname :password config/psql-pass))

(defentity entry
           (database psqldb)
           (entity-fields :id :title :link :date :content :id_feed :accessed :scrape :scrape_success))

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

;TODO: replace this with better table schema
(defentity doc-source
           (database mydb)
           (table :entry)
           (entity-fields :id :id_feed))


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
                  :id [in (subselect processlog-ids)]
                  :accessed true})))

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
  (if (= (:scrape_success doc-entry) true)
    (:scrape doc-entry)
    (parse-html-fragment (:content doc-entry))))

(defn doc-content-by-id
  "List of document ID's (contained in database) -> list of document contents "
  [id-list]
  (into {} (map #(vector (:id %) (doc-content %)) (docs-by-id id-list))))

(defn get-entity-records [docids]
  (let [bad-tags '("DATE" "NUMBER" "ORDINAL" "DURATION" "TIME" "PERCENT" "MONEY")
        bad-strings '("guardian" "reuters" "politico" "associated press" "ap" "getty" "getty images")]
    (select namedentities
            (fields :strings.entstring)
            (where (and {:docid [in docids]}
                        (not (in :tag bad-tags))
                        (not (in :strings.entstring bad-strings))))
            (join strings (= :strings.id :id)))))

(defn write-entities [entity-records string-lists]
  (transaction
    (if (seq entity-records)
      (insert namedentities
             (values entity-records)))
    (if (seq string-lists)
      (insert strings
              (values string-lists)))))

(defn get-doc-sources [doc-ids]
  (select doc-source
          (where {:id [in doc-ids]})))


(defn get-doc-out [doc-ids]
  (let [prepared-stuff (clojure.string/join ", " (take (count doc-ids) (repeat "?::bigint")))
        query (str "WITH document_items AS (
                      WITH document_strings AS (
                        SELECT namedentities.docid, array_agg(strings.entstring)
                         FROM namedentities, strings
                         WHERE namedentities.id = strings.id AND NOT namedentities.tag IN ('DATE', 'NUMBER', 'ORDINAL', 'DURATION', 'TIME', 'PERCENT')
                         GROUP BY namedentities.docid)
                  SELECT entry.id, entry.title, entry.link, entry.date, entry.id_feed, document_strings.array_agg
                    FROM entry, document_strings
                    WHERE document_strings.docid=entry.id)
                  SELECT * FROM document_items WHERE document_items.id IN (" prepared-stuff ")")
        stmnt (apply vector query doc-ids)]
    (jdbc/query db2 stmnt)))

