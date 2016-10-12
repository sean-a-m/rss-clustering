(ns GraphNamedThings.processdata
  (:require [clj-time.core :as t]
            [clj-time.format :as f]
            [GraphNamedThings.recordprocessing :as processing]
            [GraphNamedThings.dbio :as dbio]
            [clj-time.coerce :as coerce]
            [GraphNamedThings.config :as config]))

(defn process-next-document-set! [nlp-pipe batch-size]
  (let [doc-set (dbio/select-newest-unprocessed! batch-size)]
    (println "Processing " (count doc-set) " documents")
    (println "Doc set is: ")
    (println doc-set)
    (processing/create-document-records-batched nlp-pipe config/batch-size doc-set)))

(defn process-things! [nlp-pipe batch-size]
  	(let [processed (process-next-document-set! nlp-pipe batch-size)]
		(if (empty? processed)
			(Thread/sleep 300000)
  			(Thread/sleep 1000))))
;  (recur nlp-pipe batch-size))




