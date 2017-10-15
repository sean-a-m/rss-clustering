(ns GraphNamedThings.buildclusters
  (:require [GraphNamedThings.dbaccess :as dbio]
            [GraphNamedThings.louvain :as l]
            [GraphNamedThings.graphbuilder :as graphbuilder]
            [GraphNamedThings.util :as util]
            [loom.graph :as graph]
            [medley.core :as me]
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]))

(defn get-distinct [entity-records]
  ;FIXME: this should be done before retrieving the records from the database
  (let [distinct-ids (->> entity-records
                               (me/distinct-by #(select-keys % [:title :content :scrape]))
                               (map :docid))]
        (filter #(util/in? (:docid %) distinct-ids) entity-records)))

(defn louvain-comms [start-epoch end-epoch]
  (let [records (dbio/get-entity-records start-epoch end-epoch)]
    (info "Generating new cluster...")
    (->> records
         (get-distinct)
         (graphbuilder/connect-docs)
         (apply graph/weighted-graph)
         (l/iterate-louvain-modularity '()))))