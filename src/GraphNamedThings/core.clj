
(ns GraphNamedThings.core
  (:require [GraphNamedThings.document :as document]
            [GraphNamedThings.louvain :as louvain]
            [GraphNamedThings.runthing :as runthing])
  (:import [edu.stanford.nlp pipeline.StanfordCoreNLP pipeline.Annotation]))


(defn -main
  [& args]
  (
    (runthing/dorun)
    ))




