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

;path to test database
(def sqlite-path "test/GraphNamedThings/data/documents.db")

;query for tt-rss database
(def sqlite-query "SELECT id, title, link, date_entered, content, lang, author, feed_id FROM rss_entries")

(defn sqlite-db
  "Generate a database spec for sqlite using the given path"
  [path]
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     path
   })

(defdb sq-kdb (sqlite-db sqlite-path))

(defentity rss_entries
           (database sq-kdb)
           (entity-fields :id :title :content))

(defentity entrecordstest
           (database sq-kdb)
           (entity-fields :k :v))

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
          (where {:id [in id-list]})))

(defn doc-content
  "Returns title + content (parsed from html) for a given document entry as returned by docs-by-id"
  [doc-entry]
  (str/join ". " (list
                         (:TITLE doc-entry)
                         (parse-html-fragment (:CONTENT doc-entry)))))

(defn doc-content-by-id
  "List of document ID's (contained in database) -> list of document contents "
  [id-list]
  (map doc-content (docs-by-id id-list)))

(defn insert-ent-list
  [ent-list]
  (insert entrecordstest
    (values ent-list)))

(defn read-or-add
  "Given a k v pair and a database, return v if k exists
  if k doesn't exist, evaluate f, write k f and return f"
  [f k db-entity]
  (let [db-value (:v (first (select db-entity
                             (where {:k k}))))]
    (if-not (nil? db-value)
      (do
        (print "not nil")
        (nippy/thaw db-value))
      (do
        (print "nil")
          (insert db-entity
                  (values {:k k :v (nippy/freeze f)}))
          f))))


(defn read-from-sqlite
(
  [query-string db]
    (jdbc/query db query-string)
  ))

(def test-db (sqlite-db sqlite-path))

(defn read-test-db
  []
  (read-from-sqlite sqlite-query test-db))


