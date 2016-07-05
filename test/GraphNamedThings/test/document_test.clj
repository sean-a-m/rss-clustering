(ns GraphNamedThings.test.entity_test
  (:require [GraphNamedThings.document :refer :all]
            [GraphNamedThings.test.testdata :refer :all]
            [GraphNamedThings.diskio :as diskio]
            [GraphNamedThings.test.testutil :as tu]
            [clojure.string :as str]
            [clojure.test :refer :all]))



(def doccsv (diskio/read-csv-file "test/GraphNamedThings/data/data132.csv" diskio/data-header))

(def doc-names
  (map :title doccsv))

(def doc-text
  (map #(str/join ". " (list
                        (:title %)
                        (diskio/parse-html-fragment (:content %))))  doccsv))

(def doc-recs
  (map #(create-document-record %1 %2 tu/pipeline) doc-names doc-text))

(def doc-graph (document/create-document-graph (take 100 doc-recs)))