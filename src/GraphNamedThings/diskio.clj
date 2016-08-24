;Date reading functions go here
(ns GraphNamedThings.diskio
  (require [clojure-csv.core :as csv]
           [clojure.java.jdbc :as jdbc]
           [clojure.java.io :as io]
           [clojure.string :as str]
           [taoensso.nippy :as nippy]
           [GraphNamedThings.bad :as bad]
           [korma.db :refer :all]
           [korma.core :refer :all])
  (:import org.jsoup.Jsoup))

(defn parse-html-fragment
  "Assumes the text is always part of the document body"
  [html-fragment]
  (let [jsoup-doc  (. Jsoup parseBodyFragment html-fragment)]
    (.text
      (.body jsoup-doc))))

(defn parse-date-time
  "Parse date/time field, however they came in"
  []
  "The date and time should go here!")


;columns used in a test file for the feed data
(def data-header [:id :title :url :date_entered :content :lang :author :feed_id])

(def feed-header [:id :title :cat_id :feed_url :update_interval :last_updated :last_error])

(defn read-csv-file
  "Read a CSV file.  It's not appropriate for very large files since it reads the entire file into memory"
  [file header-map]
  (map
    #(zipmap header-map %)
      (with-open [fcsv (io/reader file)]
        (doall
          (csv/parse-csv fcsv)))))



(defn sqlite-db
  "Generate a database spec for sqlite using the given path"
  [path]
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     path
   })

(defdb sq-kdb (sqlite-db bad/sqlite-path))

(defentity rss_entries
           (database sq-kdb)
           (entity-fields :id :title :content :feed_id))

(def psqldb
  {:classname "org.postgresql.Driver"
   :subprotocol "postgresql"
   :subname "//localhost:5432/adb"
   ; Any additional keys are passed to the driver
   ; as driver-specific properties.
   :user bad/psql-user
   :password bad/psql-pass})

(defentity entitytest (database psqldb) (entity-fields :k :v))

(defn select-id-list
  [id-list]
  (select entitytest
          (where {:k [in id-list]})))

(defn docs-by-id
  [id-list]
  (select rss_entries
          (where {:id [in id-list]
                  :feed_id [in '(52 54 62 65 77 102)]})))

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




