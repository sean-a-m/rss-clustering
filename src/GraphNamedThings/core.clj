(ns GraphNamedThings.core
  (:require [GraphNamedThings.server :as server]
            [GraphNamedThings.processdata :as pc]
            [GraphNamedThings.weboutput :as webout]
            [clj-time.core :as t]
            [clj-time.coerce :as coerce]
            [GraphNamedThings.config :as config])
  (:import [edu.stanford.nlp pipeline.StanfordCoreNLP pipeline.Annotation]))

(defn update-response
  "Repeatedly update the document clusters returned by the server"
  [app-state]
  (loop []
    (let [start-epoch (coerce/to-epoch
                        (t/ago (t/hours 24)))
          end-epoch (coerce/to-epoch
                      (t/now))]

      (println "Updating results")
      (webout/update-results app-state start-epoch end-epoch)
      (Thread/sleep config/update-delay))
      (recur)))

(defn process-things [nlp-pipe batch-size]
  (loop []
    (pc/process-documents nlp-pipe batch-size)
    (recur)))

(defn -main
  [& args]
  (let [props (doto (java.util.Properties.)
                (.put "annotators" "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
                (.put "threads" 2)
                (.put "timeout" 30000))
        nlp-pipe (new StanfordCoreNLP props)
        app-state (atom nil)]
    (future (process-things nlp-pipe 1))
    (future (update-response app-state))
    (future (server/runserver app-state))))


