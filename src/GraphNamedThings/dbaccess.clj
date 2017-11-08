(ns GraphNamedThings.dbaccess
  (:require [clj-postgresql.core :as pg]
            [clojure.java.jdbc :as jdbc]
            [GraphNamedThings.db :as db]
            [GraphNamedThings.config :as config]
            [medley.core :as me])
  (:import org.jsoup.Jsoup))



(def psqldb (pg/pool :host config/dbhost :user config/psql-user :dbname config/dbname :password config/psql-pass))

(defn parse-html-fragment
  "Assumes the text is always part of the document body"
  [html-fragment]
  (let [jsoup-doc  (. Jsoup parseBodyFragment html-fragment)]
    (.text
      (.body jsoup-doc))))

(defn docs-by-id
  "Select input documents by ID"
  [id-list]
  (db/docs-by-id psqldb {:id-list id-list :feed-ids config/selected-feed-ids}))

(defn select-newest-unprocessed [batch-size]
  (db/select-newest-unprocessed psqldb {:batch-size batch-size :feed-ids config/selected-feed-ids}))

(defn processed-docs-from-time-range
  "Time is epoch in  frss table"
  [start-time end-time]
  (db/processed-docs-from-time-range psqldb {:feed-ids config/selected-feed-ids :start-time start-time :end-time end-time}))

(defn log-result
  [doc-id success?]
  (db/log-result psqldb {:success? success? :doc-id doc-id}))

(defn doc-content
  "Returns content (parsed from html) for a given document entry as returned by docs-by-id"
  [doc-entry]
  (if (= (:scrape_success doc-entry) true)
    (:scrape doc-entry)
    (parse-html-fragment (:content doc-entry))))

(defn doc-content-by-id
  "List of document ID's (contained in database) -> list of document contents "
  [id-list]
  (into {} (map #(vector (:id %) (doc-content %)) (docs-by-id id-list))))

(defn get-entity-records
  [start end]
  (db/get-entity-records psqldb {:start start :end end}))

(defn write-entities [entity-records string-lists]
  (jdbc/with-db-transaction [tx psqldb]
      (doall
        (map (partial db/write-entity-record tx) entity-records))
      (doall
        (map (partial db/write-string tx) string-lists))))


(defn get-doc-summary
  [doc-ids]
  (db/get-doc-summary psqldb {:doc-ids doc-ids}))

(defn get-string-counts
  [strings]
  (db/get-string-counts psqldb {:strings strings}))

(defn related-article-ids [terms time-before limit offset]
  (db/related-article-ids psqldb {:strings terms :time-before time-before :limit limit :offset offset}))
