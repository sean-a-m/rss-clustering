(ns GraphNamedThings.buildclusters
  (:require [GraphNamedThings.dbio :as dbio]
            [GraphNamedThings.louvain :as l]
            [GraphNamedThings.document2 :as d2]
            ;[clmcll.mcl :as mcl]
            ;[louvainloom.louvain :as l]
            [GraphNamedThings.recordprocessing :as processing]
            [GraphNamedThings.document :as document]
            [GraphNamedThings.feature-matrix :as fm]
            [GraphNamedThings.kmeans :as kmeans]
            [loom.graph :as graph])
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
  [start-epoch end-epoch]
  (let [;start-epoch (coerce/to-epoch start)
        ;end-epoch (coerce/to-epoch end)
        docs (dbio/processed-docs-from-time-range start-epoch end-epoch)
        ids (map :id docs)]
    ;  cached (dbio/select-best-cluster-set start end)]
    (->> ids
         (processing/create-document-records)
         (document/create-document-graph))))
;(l/iterate-louvain-modularity '()))))
;(mcl/cluster))))
;(louvain/iterate-louvain-modularity '())
;(first) ;above should return a data structure that's more clear than a vector, but while it returns a vector, the first element is the list of communities
;(louvain-output-to-clusters))))

(defn kmeans-comms [start-epoch end-epoch]
  (let [docs (dbio/processed-docs-from-time-range start-epoch end-epoch)
        ids (map :id docs)
        ent-recs (dbio/get-entity-records ids)
        rev-doc-idx (zipmap (iterate inc 0)
                            (distinct (map :docid ent-recs)))
        K 40]
    (let [cluster-results (->> ent-recs
                               (fm/create-feature-matrix)
                               (kmeans/find-centers K))]
      (for [cluster (vals cluster-results)]
        (map #(get rev-doc-idx %) cluster)))))

(defn louvain-comms [start-epoch end-epoch]
  (let [docs (dbio/processed-docs-from-time-range start-epoch end-epoch)
        ids (map :id docs)]
    (println "Generating new cluster...")
    (->> ids
         (dbio/get-entity-records)
         (filter #(comp contains? #{"reuters" "politico" "guardian" "associated press"} (:entstring %)))
         (filter #(comp contains? #{10} (:id_feed %)))
         (d2/connect-docs2)
         (apply graph/weighted-graph)
         (l/iterate-louvain-modularity '()))))