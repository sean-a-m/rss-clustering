(ns GraphNamedThings.weboutput
  (:require [GraphNamedThings.buildclusters :as bc]
            [GraphNamedThings.louvain :as l]
            [GraphNamedThings.dbio :as dbio]))

(defn get-score [result modularity]
  (* modularity
     (count
       (distinct
         (map :id_feed result)))))

;(defn gen-result [articles modularity]
(defn build-results [g comm-docs]
  (let [results (dbio/get-doc-out comm-docs)]
    (for [article results]
      (assoc article :comm-weight (reduce + (l/get-community-weight g comm-docs (:id article)))))))


(defn gen-results [start-epoch end-epoch]
  (let [l-results (bc/louvain-comms start-epoch end-epoch)]
    (let [lcomms (:comms l-results)
          modularity (:qs l-results)
          g (:graph l-results)]
      (for [comm lcomms]
        (let [result (build-results g (val comm))
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
