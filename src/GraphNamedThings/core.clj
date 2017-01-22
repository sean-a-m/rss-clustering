
(ns GraphNamedThings.core
  (:require [GraphNamedThings.server :as server]
            [GraphNamedThings.processdata :as pc]
            [GraphNamedThings.weboutput :as webout]
            [clj-time.core :as t]
            [clj-time.coerce :as coerce])
  (:import [edu.stanford.nlp pipeline.StanfordCoreNLP pipeline.Annotation]))


;(defn app [req]
;  {:status 200
;   :headers {"Content-Type" "text/html"}
;   :body "hello HTTP!"})

(defn update-response
  "Repeatedly update the document clusters returned by the server"
  [app-state]
  (future
    (loop []
      (let [start-epoch (coerce/to-epoch
                          (t/ago (t/hours 24)))
            end-epoch (coerce/to-epoch
                        (t/now))]
        (webout/update-results app-state start-epoch end-epoch)
        (Thread/sleep 60000))
      (recur))))

(defn -main
  [& args]
  (let [props (doto (java.util.Properties.)
                (.put "annotators" "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
                (.put "threads" 1)
                (.put "timeout" 30000))
        nlp-pipe (new StanfordCoreNLP props)
        app-state (atom nil)]
    (future
      (loop []
        (pc/process-things! nlp-pipe 1)
        (println "processing finished...")
        (recur)))
    (update-response app-state)
    (server/runserver app-state)))