(ns GraphNamedThings.test.document_test
  (:require [GraphNamedThings.document :refer :all]
            [GraphNamedThings.test.testdata :refer :all]
            [GraphNamedThings.dbio :as dbio]
            [GraphNamedThings.test.testutil :as tu]
            [GraphNamedThings.recordprocessing :as processing]
            [clj-time.core :as t]
            [clj-time.coerce :as coerce]
            [clojure.test :refer :all]))




;(def doc-recs
;  (pmap #(create-document-record %1 %2 %3 tu/pipeline) doc-ids doc-names doc-text))

;(time
;  (def doc-graph (document/create-document-graph (take 100 doc-recs))))

(def docs
  (let [t0 (t/date-time 2016 9 28)
        t1 (t/date-time 2016 9 29)]
    (dbio/docs-from-time-range (coerce/to-epoch t0) (coerce/to-epoch t1))))

(def doc-recs
  (processing/create-document-records-batched tu/pipeline 100 docs))

(time
(deftest create-graph-edges-test
  (create-graph-edges doc-recs)))