(ns GraphNamedThings.processdata
  (:require [clj-time.core :as t]
            [clj-time.format :as f]
            [GraphNamedThings.recordprocessing :as processing]
            [GraphNamedThings.dbio :as dbio]
            [clj-time.coerce :as coerce]
            [GraphNamedThings.config :as config]))

(defn process-next-document-set! [nlp-pipe batch-size]
  (let [doc-ids (map :id (dbio/select-newest-unprocessed-newest batch-size))]
    (println "Processing " (count doc-ids) " documents")
    (println "Doc set is: ")
    (println doc-ids)
    (processing/build-and-write-new-entity-records doc-ids nlp-pipe)))
   ; (processing/create-document-records-batched nlp-pipe config/batch-size doc-set)))

(defn process-things! [nlp-pipe batch-size]
  	(let [processed (process-next-document-set! nlp-pipe batch-size)]
		(if (empty? processed)
			(Thread/sleep 300000)
  			(Thread/sleep 1000))))
;  (recur nlp-pipe batch-size))




