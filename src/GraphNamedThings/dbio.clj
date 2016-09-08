;Data processing functions specific to a type of database or document source

(ns GraphNamedThings.dbio
  (require [clojure-csv.core :as csv]
           [clojure.java.io :as io]
           [clojure.string :as str]
           [GraphNamedThings.bad :as bad]
           [korma.db :refer :all]
           [korma.core :refer :all]
           [GraphNamedThings.config :as config])
  (:import org.jsoup.Jsoup))

(defn sqlite-db
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     config/input-db-path
   })

(defentity rss_entries
           (database sqlite-db)
           (entity-fields :id :title :content :feed_id))

(def psqldb
  {:classname   "org.postgresql.Driver"
   :subprotocol "postgresql"
   :subname     config/output-db-path
   :user        config/psql-user
   :password    config/psql-pass})

(defentity entitytest (database psqldb) (entity-fields :k :v))

(defn select-id-list
  "Select processed documents by ID"
  [id-list]
  (select entitytest
          (where {:k [in id-list]})))

(defn docs-by-id
  "Select input documents by ID"
  [id-list]
  (select rss_entries
          (where {:id [in id-list]
                  :feed_id [in config/selected-feed-ids]})))

(defn doc-content
  "Returns title + content (parsed from html) for a given document entry as returned by docs-by-id"
  [doc-entry]
  (str/join ". " (list
                         (:TITLE doc-entry)
                         (parse-html-fragment (str/join (list " " (:CONTENT doc-entry))))))) ;adding a space is a hack to stop jsoup from crashing and should be handle smarter somehow

(defn doc-content-by-id
  "List of document ID's (contained in database) -> list of document contents "
  [id-list]
  (into {} (map #(vector (:ID %) (doc-content %)) (docs-by-id id-list))))

(defn parse-html-fragment
  "Assumes the text is always part of the document body"
  [html-fragment]
  (let [jsoup-doc  (. Jsoup parseBodyFragment html-fragment)]
    (.text
      (.body jsoup-doc))))



