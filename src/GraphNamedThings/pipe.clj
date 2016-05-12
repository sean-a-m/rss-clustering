;Date reading functions go here
;TODO: move test functions out of this file to somewhere more appropriate
(ns GraphNamedThings.pipe
  (use GraphNamedThings.document)
  (use clojure-csv.core)
  (use clojure.java.jdbc)
  (use clojure.java.io)
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
      (with-open [fcsv (reader file)]
        (doall
          (parse-csv fcsv)))))



;path to test database
(def sqlite-path "test/documents.db")

;query for tt-rss database
(def sqlite-query "SELECT id, title, link, date_entered, content, lang, author, feed_id FROM rss_entries")

(defn sqlite-db
  "Generate a database spec for sqlite using the given path"
  [path]
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     path
   })

(defn read-from-sqlite
(
  [query-string db]
    (query db query-string)
  ))

(def test-db (sqlite-db sqlite-path))

(defn read-test-db
  []
  (read-from-sqlite sqlite-query test-db))


