(ns GraphNamedThings.processdata
  (:require [GraphNamedThings.recordprocessing :as processing]
            [GraphNamedThings.dbaccess :as dbio]
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]))

(defn process-next-document-set [nlp-pipe batch-size]
  (let [doc-ids (map :id (dbio/select-newest-unprocessed batch-size))]
    (info "Processing entries: " doc-ids)
    (processing/build-and-write-new-entity-records doc-ids nlp-pipe)))

(defn process-documents [nlp-pipe batch-size]
  	(let [processed (process-next-document-set nlp-pipe batch-size)]
		(if (empty? processed)
			(Thread/sleep 30000)
  			(Thread/sleep 300))))




