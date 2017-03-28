(ns GraphNamedThings.buildclusters
  (:require [GraphNamedThings.dbio :as dbio]
            [GraphNamedThings.louvain :as l]
            [GraphNamedThings.graphbuilder :as d2]
            ;[clmcll.mcl :as mcl]
            ;[louvainloom.louvain :as l]
            [GraphNamedThings.feature-matrix :as fm]
            [GraphNamedThings.kmeans :as kmeans]
            [loom.graph :as graph])
  (:import [edu.stanford.nlp pipeline.StanfordCoreNLP pipeline.Annotation]))


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
         (d2/connect-docs)
         (apply graph/weighted-graph)
         (l/iterate-louvain-modularity '()))))