(ns GraphNamedThings.core
  (:require [GraphNamedThings.server :as server]
            [GraphNamedThings.processdata :as pc]
            [GraphNamedThings.weboutput :as webout]
            [clj-time.core :as t]
            [clj-time.coerce :as coerce]
            [GraphNamedThings.config :as config]
            [clojure.java.io :as io]
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]
            [taoensso.timbre.appenders.core :as appenders])
  (:import (edu.stanford.nlp.pipeline StanfordCoreNLP))
  (:gen-class))

(defn configure-logging! []
  (let [log-file-name "log"]
    (io/delete-file log-file-name :quiet)
    (timbre/merge-config!
      {:appenders {:spit (appenders/spit-appender {:fname log-file-name})}})
    (timbre/merge-config! {:appenders {:spit {:enabled? true
                                              :min-level :error}}})))



(defn update-clusters
  "Repeatedly update the document clusters returned by the server"
  [article-clusters]
  (loop []
    (let [start-epoch (-> (t/hours 24) t/ago coerce/to-epoch)
          end-epoch (coerce/to-epoch (t/now))]
      (try
        (info "Updating results")
        (webout/update-results article-clusters start-epoch end-epoch)
        (catch Exception e (error e)))
        (Thread/sleep config/update-delay)
      (recur))))

(defn document-processor [nlp-pipe batch-size]
  (loop []
    (try
      (pc/process-documents nlp-pipe batch-size)
      (catch Exception e (error e)))
    (recur)))

(defn -main
  [& args]
  (let [props (doto (java.util.Properties.)
                (.put "annotators" "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
                (.put "threads" config/corenlp-threads)
                (.put "timeout" 30000))
        nlp-pipe (new StanfordCoreNLP props)
        article-clusters (atom nil)]
    (configure-logging!)
    (future (document-processor nlp-pipe config/corenlp-batch-size))
    (future (update-clusters article-clusters))
    (future (server/run-server article-clusters))))


