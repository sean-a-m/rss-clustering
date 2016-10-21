(ns GraphNamedThings.test.entity_test
  (:require [GraphNamedThings.recordprocessing :as rp]
            [GraphNamedThings.test.testdata :refer :all]
            [clojure.test :refer :all]
            [GraphNamedThings.document :as document]
            [clj-time.core :as t]
            [clj-time.coerce :as coerce])
  (:import [edu.stanford.nlp pipeline.StanfordCoreNLP pipeline.Annotation]))



(def props  (doto (java.util.Properties.)
              (.put "annotators" "tokenize, ssplit, pos, lemma, ner, parse, dcoref")))
(def nlp-pipe (new StanfordCoreNLP props))
(def docs (dbio/docs-from-time-range (coerce/to-epoch (t/date-time 2016 10 1 13)) (coerce/to-epoch (t/date-time 2016 10 2 12))))
(def s1 (->> docs
             (processing/create-document-records-batched nlp-pipe 15)))