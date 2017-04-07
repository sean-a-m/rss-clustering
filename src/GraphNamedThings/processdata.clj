(ns GraphNamedThings.processdata
  (:require [GraphNamedThings.recordprocessing :as processing]
            [GraphNamedThings.dbaccess :as dbio]))

(defn process-next-document-set [nlp-pipe batch-size]
  (let [doc-ids (map :id (dbio/select-newest-unprocessed batch-size))]
    (println "Processing " (count doc-ids) " documents")
    (println "Doc set is: ")
    (println doc-ids)
    (processing/build-and-write-new-entity-records doc-ids nlp-pipe)))

(defn process-documents [nlp-pipe batch-size]
  	(let [processed (process-next-document-set nlp-pipe batch-size)]
		(if (empty? processed)
			(Thread/sleep 30000)
  			(Thread/sleep 300))))




