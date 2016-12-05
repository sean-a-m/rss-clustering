(ns GraphNamedThings.buildclusters
  (:require [GraphNamedThings.config :as config]
            [GraphNamedThings.dbio :as dbio]
            [GraphNamedThings.louvain :as louvain]
            [GraphNamedThings.recordprocessing :as processing]
            [GraphNamedThings.document :as document]
            [clj-time.core :as t]
            [clj-time.coerce :as coerce])
  (:import [edu.stanford.nlp pipeline.StanfordCoreNLP pipeline.Annotation]
           [GraphNamedThings.dbio doccluster]))


(defn community-item-to-doc-cluster
  [c]
  (doccluster.
    (key c)
    (val c)))

(defn louvain-output-to-clusters
  "Takes a community set from louvain iteration method and returns a collection of doc-cluster records"
  [cs]
  (map community-item-to-doc-cluster cs))


(defn create-document-clusters-by-date-range
  "Return set of document clusters from a start and end date as clj-time"
  [start end]
  (let [start-epoch (coerce/to-epoch start)
        end-epoch (coerce/to-epoch end)
        docs (dbio/processed-docs-from-time-range start-epoch end-epoch)
        ids (map :id docs)]
      ;  cached (dbio/select-best-cluster-set start end)]
    (->> ids
         (processing/create-document-records)
         (document/create-document-graph)
         (louvain/iterate-louvain-modularity '())
         (first) ;above should return a data structure that's more clear than a vector, but while it returns a vector, the first element is the list of communities
         (louvain-output-to-clusters))))
