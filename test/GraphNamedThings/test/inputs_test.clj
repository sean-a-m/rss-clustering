(ns GraphNamedThings.test.inputs_test
  (:require [GraphNamedThings.inputs :refer :all]
            [clojure.test :refer :all]
            [GraphNamedThings.test.testdata :refer :all]
            [GraphNamedThings.util :as util]
            [GraphNamedThings.nlputil :as nlputil]
            [GraphNamedThings.corenlpdefs :as nlpdefs])
   (:import [edu.stanford.nlp pipeline.StanfordCoreNLP pipeline.Annotation]))

;Articles from http://archive.ics.uci.edu/ml/datasets/Reuters-21578+Text+Categorization+Collection

(def props  (doto (java.util.Properties.)
              (.put "annotators" "tokenize, ssplit, pos, lemma, ner, parse, dcoref")))

(def pipeline (new StanfordCoreNLP props))

(def nlp-processed (nlpdefs/corenlp-annotate-doc doc1a pipeline))

(def nlp-processed-simple (nlpdefs/corenlp-annotate-doc docsimple pipeline))

(def nlp-tokens (nlpdefs/get-tokens nlp-processed))

(def nlp-tokens-simple (nlpdefs/get-tokens nlp-processed-simple))

(def tag-list (map nlpdefs/get-ner-tag nlp-tokens))

(def tag-list-simple (map nlpdefs/get-ner-tag nlp-tokens-simple))

(def nlp-corefs (util/core-coref-list
                      (nlpdefs/get-corefs nlp-processed)))



(deftest verify-ner-id-list
  (let [nlp-tokens-simple (nlpdefs/get-tokens nlp-processed-simple)
        tag-list-simple (map nlpdefs/get-ner-tag nlp-tokens-simple)]
    (is (= (ner-ids-from-tokens nlp-tokens-simple) '(0 0 0 1 2 2 2 2 2 2 2 2 2)))))


(deftest verify-token-groups
  (let [test-tokens (token-groups doc1a pipeline)]
    ;check hierarchy
    (is (= 2 (-> test-tokens
                 first
                 first
                 count)))
    (is (= 15 (-> test-tokens
                  flatten
                  count)))
    (is (instance? edu.stanford.nlp.ling.CoreLabel (-> test-tokens
                                                       first
                                                       first
                                                       first
                                                       type)))))

