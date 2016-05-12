;data comes in here!
(ns GraphNamedThings.core
  (use GraphNamedThings.document)
  (use clojure-csv.core)
  (use clojure.java.jdbc)
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


(defn read-from-csv
  "Read the lines matching the predicates from the given csv file"
  [file line-predicate]
  "There should be data here, but there isn't!"
)

(def sqlite-path "test/documents.db")

(defn sqlite-db
  "Create a database spec for sqlite using the given path"
  [path]
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     path
   })

(defn read-from-sqlite)
(
  []

  )
