;data comes in here!
(ns GraphNamedThings.core
  (use GraphNamedThings.document)
  (use clojure-csv.core)
  (:import org.jsoup.Jsoup))

(defn parse-html-fragment
  "Parse HTML fragment using jsoup library.  Assumes the text is always part of the document body"
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
  [file line-predicates]
  "There should be data here, but there isn't!"
)