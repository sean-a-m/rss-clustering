(ns GraphNamedThings.weboutput
  (:require [GraphNamedThings.buildclusters :as bc]
            [GraphNamedThings.louvain :as l]
            [GraphNamedThings.dbio :as dbio]))

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
      (for [source-group (vals (group-by :id_feed ranked-articles))]
        (map-indexed #(assoc %2 :source-rank %1) source-group)))))


(defn add-article-weights [g comm-docs]
  (let [results (dbio/get-doc-out comm-docs)]
    (for [article results]
      (assoc article :comm-weight (reduce + (l/get-community-weight g comm-docs (:id article)))))))


(defn gen-results [start-epoch end-epoch]
  (let [l-results (bc/louvain-comms start-epoch end-epoch)]
    (let [lcomms (:comms l-results)
          modularity (:qs l-results)
          g (:graph l-results)]
      (for [comm lcomms]
        (let [result (->> (val comm)
                          (add-article-weights g)
                          (rank-articles))
              ;result (dbio/get-doc-out (val comm))
              modularity (get modularity (key comm))]
          (let [score (get-score result modularity)]
            (hash-map :articles result :score score)))))))


(defn update-results [app-state start-epoch end-epoch]
  (let [new-ids (into #{} (map :id (dbio/processed-docs-from-time-range start-epoch end-epoch))) ;TODO: Replace with query that only retreives IDs
        cur-ids (into #{} (map :id (flatten @app-state)))]
    (println "Getting docs between " start-epoch "and " end-epoch)
    (if (not= new-ids cur-ids)
      (doall
        (reset! app-state (gen-results start-epoch end-epoch)))
      app-state)))
