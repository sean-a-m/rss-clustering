
(ns GraphNamedThings.core
  (:require [GraphNamedThings.server :as server]
            [GraphNamedThings.processdata :as pc])
  (:import [edu.stanford.nlp pipeline.StanfordCoreNLP pipeline.Annotation]))


;(defn app [req]
;  {:status 200
;   :headers {"Content-Type" "text/html"}
;   :body "hello HTTP!"})


(defn -main
  [& args]
  (let [props (doto (java.util.Properties.)
                (.put "annotators" "tokenize, ssplit, pos, lemma, ner, parse, dcoref"))
        nlp-pipe (new StanfordCoreNLP props)]
    (def processing-thread
      future (pc/process-things! nlp-pipe 150))
    (server/runserver nlp-pipe)))

  ;(http/run-server app {:port 9002}))




