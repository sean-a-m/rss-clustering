(ns GraphNamedThings.core
  (:require [GraphNamedThings.server :as server]
            [GraphNamedThings.processdata :as pc]
            [GraphNamedThings.weboutput :as webout]
            [clj-time.core :as t]
            [clj-time.coerce :as coerce]
            [GraphNamedThings.config :as config])
  (:import (edu.stanford.nlp.pipeline StanfordCoreNLP))
  (:gen-class))

(defn update-clusters
  "Repeatedly update the document clusters returned by the server"
  [article-clusters]
  (loop []
    (let [start-epoch (-> (t/hours 24) t/ago coerce/to-epoch)
          end-epoch (coerce/to-epoch (t/now))]
      (println "Updating results")
      (webout/update-results article-clusters start-epoch end-epoch)
      (Thread/sleep config/update-delay))
      (recur)))

(defn document-processor [nlp-pipe batch-size]
  (loop []
    (pc/process-documents nlp-pipe batch-size)
    (recur)))

(defn -main
  [& args]
  (let [props (doto (java.util.Properties.)
                (.put "annotators" "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
                (.put "threads" config/corenlp-threads)
                (.put "timeout" 30000))
        nlp-pipe (new StanfordCoreNLP props)
        article-clusters (atom nil)]
    (future (document-processor nlp-pipe config/corenlp-batch-size))
    (future (update-clusters article-clusters))
    (future (server/run-server article-clusters))))


