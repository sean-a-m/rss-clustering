(ns GraphNamedThings.processdata
  (:require [clj-time.core :as t]
            [clj-time.format :as f]
            [GraphNamedThings.recordprocessing :as processing]
            [GraphNamedThings.dbio :as dbio]
            [clj-time.coerce :as coerce]
            [GraphNamedThings.config :as config]))

(defn process-next-document-set! [nlp-pipe batch-size]
  (let [doc-set (dbio/select-newest-unprocessed! batch-size)]
    (processing/create-document-records-batched nlp-pipe config/batch-size doc-set)))

(defn process-things! [nlp-pipe batch-size]
  (def processed
    (future (process-next-document-set! nlp-pipe batch-size)))
  (println "Processed: " (count @processed))
  (Thread/sleep 600000)
  (recur nlp-pipe batch-size))




