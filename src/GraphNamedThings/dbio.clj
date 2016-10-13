;Data processing functions specific to a type of database or document source

(ns GraphNamedThings.dbio
  (:require [clojure-csv.core :as csv]
           [clojure.java.io :as io]
           [clojure.string :as str]
            [clj-time.core :as t]
            [clj-time.coerce :as coerce]
           [korma.db :refer :all]
           [korma.core :refer :all]
           [GraphNamedThings.config :as config])
  (:import org.jsoup.Jsoup))

;defines one group of related document, where group-id can be an arbitrary id and doc-ids is a collection of document ids
(defrecord doccluster [group-id doc-ids])


(def ttrss-db
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     config/ttrss-db-path
   })

(def frss-db
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     config/frss-db-path
   })

(def psqldb
  {:classname   "org.postgresql.Driver"
   :subprotocol "postgresql"
   :subname     config/output-db-path
   :user        config/psql-user
   :password    config/psql-pass})

(defentity rss_entries
           (database psqldb)
           (entity-fields :id :title :link :date_entered :content :feed_id))

(defentity entry
           (database psqldb)
           (entity-fields :id :title :link :date :content :id_feed))

(defentity processed
           (database psqldb)
           (entity-fields :id :startdate :enddate))

(defentity docrelations
           (database psqldb)
           (entity-fields :processed_id :group_id :doc_id))

(defn parse-html-fragment
  "Assumes the text is always part of the document body"
  [html-fragment]
  (let [jsoup-doc  (. Jsoup parseBodyFragment html-fragment)]
    (.text
      (.body jsoup-doc))))

(defentity entitytest (database psqldb) (entity-fields :k :v))

(defn select-id-list
  "Select processed documents by ID"
  [id-list]
  (select entitytest
          (where {:k [in id-list]})))

(defn docs-by-id
  "Select input documents by ID"
  [id-list]
  (select entry
          (where {:id [in id-list]
                  :id_feed [in config/selected-feed-ids]})))

(defn select-newest-unprocessed! [batch-size]
  "Select the most recent documents that haven't been processed to related entities yet.  "
  (exec-raw psqldb ["SELECT id, title, link, date, content, id_feed FROM entry WHERE id NOT IN (SELECT k FROM entitytest) AND id_feed IN (3, 4, 7, 9, 13, 14, 15, 16, 17, 18, 72, 79, 86, 97, 98, 99, 100) ORDER BY date DESC LIMIT ?" [batch-size]] :results))

(defn select-newest-unprocessed!2 [batch-size]
  (select entry
          (where (and (not {:id [in (map :k (select entitytest))]})
                      {:id [in config/selected-feed-ids]}))))
(defn docs-from-time-range-raw
  [start-time end-time]
  (exec-raw frss-db ["SELECT * FROM entry WHERE date BETWEEN ? AND ?" [start-time end-time]] :results))

(defn docs-from-time-range
  "Time is epoch in  frss table"
  [start-time end-time]
  (select entry
          (where {:date [between [start-time end-time]]
                  :id_feed [in config/selected-feed-ids]})))

(defn doc-content
  "Returns title + content (parsed from html) for a given document entry as returned by docs-by-id"
  [doc-entry]
  (str/join ". " (list
                         (:title doc-entry)
                         (parse-html-fragment (str/join (list " " (:content doc-entry))))))) ;adding a space is a hack to stop jsoup from crashing and should be handle smarter somehow

(defn doc-content-by-id
  "List of document ID's (contained in database) -> list of document contents "
  [id-list]
  (into {} (map #(vector (:id %) (doc-content %)) (docs-by-id id-list))))

(defn select-cluster-candidates
  "Return all processed table entries that have a time interval between start date and end date"
  [start-date end-date]
  (let [sql-start (coerce/to-sql-time start-date)
        sql-end (coerce/to-sql-time end-date)]
    (select processed
            (where (and (>= sql-start :startdate)
                        (<= sql-end :enddate))))))


(defn select-overlapping-clusters
  "Select all clusters that overlap with start-end interval"
  [start-date end-date]
  (let [sql-start (coerce/to-sql-time start-date)
        sql-end (coerce/to-sql-time end-date)]
    (select processed
            (where (or (<= sql-start :enddate)
                        (>= sql-end :startdate))))))

(defn select-best-cluster
  "Given start and end timestamps, select the best document group.  Right now the best one is whichever is over the smallest invterval"
  [start-date end-date]
  (let [candidates (select-cluster-candidates start-date end-date)]
    (:id (first
          (sort-by #(t/in-seconds  ;convert to seconds because clj-time/interval is not comparable
                       (t/interval (coerce/from-sql-time (:startdate %)) (coerce/from-sql-time (:enddate %))))
                   candidates)))))


(defn select-best-cluster-set
  "Incomplete, intended to select the set of overlapping clusters that would give the most accurate final result"
  [start-date end-date]
  (let [candidates (select-overlapping-clusters start-date end-date)
        processed-ids (map :id candidates)]
    (select docrelations
      (where {:processed_id [in processed-ids]}))))

(defn write-new-cluster-entry
  "Write entry for this document cluster into the index, with a starting time stamp and postgres interval, and return the id"
  [start-date end-date]
  (let [start-entry (coerce/to-sql-time start-date)
        end-entry (coerce/to-sql-time end-date)]
    (insert processed
            (values [{:startdate start-entry :enddate end-entry}]))))

(defn create-entries-from-cluster
  "Write a single group of document ids to the database"
  [id cluster]
  (map #(hash-map :processed_id id :group_id (:group-id cluster) :doc_id %) (:doc-ids cluster)))


(defn write-clusters
  [clusters start-date end-date]
  "Write the resulting set of clusters to the database"
  (let [id (:id (write-new-cluster-entry start-date end-date))

        cluster-entries (mapcat (partial create-entries-from-cluster id) clusters)]
      (insert docrelations (values (doall cluster-entries)))))

(defn group-to-cluster-entry
  [group]
  (->doccluster
    (key group)
    (map :doc_id (val group))))

(defn read-doc-cluster
  "Read db entries into doc-cluster record"
  [id]
  (let [doc-relations (select docrelations
                              (where (= :processed_id id)))
        doc-relations-grouped (group-by :group_id doc-relations)]
    (map group-to-cluster-entry doc-relations-grouped)))


