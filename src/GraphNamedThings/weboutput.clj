(ns GraphNamedThings.weboutput
  (:require [GraphNamedThings.buildclusters :as bc]
            [GraphNamedThings.louvain :as l]
            [GraphNamedThings.dbaccess :as dbaccess]
            [medley.core :as me]
            [clj-uuid :as uuid]))

(defn get-score [result modularity]
  (* modularity
     (count
       (distinct
         (map :id_feed result)))))

(defn rank-articles [articles]
  (let [ranked-articles (->> articles
                             (sort-by :comm-weight)
                             (reverse)
                             (map-indexed #(assoc %2 :group-rank %1)))]
    (flatten
      (for [source-group (vals (group-by :source_id ranked-articles))]
        (map-indexed #(assoc %2 :source-rank %1) source-group)))))


(defn add-article-weights [g comm-docs]
  ;TODO: retrieving all of the article data should be somewhere more obvious
  (let [results (dbaccess/get-doc-summary comm-docs)]
    (for [article results]
      (assoc article :comm-weight (reduce + (l/get-community-weight g comm-docs (:id article)))))))


(defn gen-results [start-epoch end-epoch]
  (let [l-results (bc/louvain-comms start-epoch end-epoch)]
    (let [lcomms (:comms l-results)
          modularity (:qs l-results)
          g (:graph l-results)]
        (pmap
          #(let [result (->> (val %)
                            (add-article-weights g)
                            (rank-articles))
                modularity (get modularity (key %))]
            (let [score (get-score result modularity)]
              (hash-map :articles result :score score :id (uuid/v4))))
          lcomms))))

(defn update-results [article-clusters start-epoch end-epoch]
  (let [new-ids (into #{} (map :id (dbaccess/processed-docs-from-time-range start-epoch end-epoch)))
        cur-ids (into #{} (map :id (flatten @article-clusters)))]
    (println "Getting docs between " start-epoch "and " end-epoch)
    (if (not= new-ids cur-ids)
      (doall
        (reset! article-clusters (gen-results start-epoch end-epoch)))
      ((println "No new articles found")
       article-clusters))))
