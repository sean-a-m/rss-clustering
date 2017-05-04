(ns GraphNamedThings.buildclusters
  (:require [GraphNamedThings.dbaccess :as dbio]
            [GraphNamedThings.louvain :as l]
            [GraphNamedThings.graphbuilder :as graphbuilder]
            ;[clmcll.mcl :as mcl]
            ;[louvainloom.louvain :as l]
            [GraphNamedThings.feature-matrix :as fm]
            [GraphNamedThings.kmeans :as kmeans]
            [GraphNamedThings.util :as util]
            [loom.graph :as graph]
            [medley.core :as me]))

(defn- get-distinct [entity-records]
  ;FIXME: this should be done before retrieving the records from the database
  (let [distinct-ids (-> entity-records
                               (me/distinct-by #(select-keys % [:title :content :scrape]))
                               (map :docid))]
        (filter #(util/in? (:docid %) distinct-ids) entity-records)))


(defn kmeans-comms [start-epoch end-epoch]
  (let [ent-recs (dbio/get-entity-records start-epoch end-epoch)
        rev-doc-idx (zipmap (iterate inc 0)
                            (distinct (map :docid ent-recs)))
        K 40]
    (let [cluster-results (->> ent-recs
                               (fm/create-feature-matrix)
                               (kmeans/find-centers K))]
      (for [cluster (vals cluster-results)]
        (map #(get rev-doc-idx %) cluster)))))

(defn louvain-comms [start-epoch end-epoch]
  (let [records (dbio/get-entity-records start-epoch end-epoch)]
    (println "Generating new cluster...")
    (->> records
         (get-distinct)
         (graphbuilder/connect-docs)
         (apply graph/weighted-graph)
         (l/iterate-louvain-modularity '()))))