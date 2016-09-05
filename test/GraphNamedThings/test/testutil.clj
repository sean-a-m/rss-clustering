;Utilities for testing.


(ns GraphNamedThings.test.testutil
  (:require [GraphNamedThings.util :as util]
            [clojure.core.matrix :as m])
  (:import [edu.stanford.nlp pipeline.StanfordCoreNLP pipeline.Annotation]))


(defn write-doc [doc-rec]
  (spit "test/doc" (prn doc-rec)))

(defn read-doc [file]
  (read-string (slurp file)))

(def props  (doto (java.util.Properties.)
              (.put "annotators" "tokenize, ssplit, pos, lemma, ner, parse, dcoref")))

(def pipeline (new StanfordCoreNLP props))

;TODO: notes regarding better floating point comparisons: http://www.boost.org/doc/libs/1_59_0/libs/test/doc/html/boost_test/testing_tools/extended_comparison/floating_point/floating_points_comparison_impl.html
(defn fuzzy-float-eq
  "Floating point comparison using absolute tolerance"
  [eps first second]
  (> eps
     (Math/abs
       (- first second))))

