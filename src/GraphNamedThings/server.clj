(ns GraphNamedThings.server
  (:use compojure.core)
  (:require [org.httpkit.server :as http]
            [compojure.core :refer :all]
            [compojure.handler :as handler]
            [ring.util.response :refer [response]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [GraphNamedThings.config :as config]
            [GraphNamedThings.schema :as sc]
            [GraphNamedThings.dbaccess :as db]
            [clj-time.core :as t]
            [clj-time.coerce :as coerce]))

(defn- paginate [perpage page coll]
  (let [perpage (or perpage config/articles-per-page)
        page (or page 0)
        partitioned (partition-all perpage coll)]
    (nth partitioned page nil)))

(defn- assoc-term-scores [cluster]
    (let [terms (flatten
                  (map :array_agg (:articles cluster)))
          dterms (distinct terms)
          tfs (frequencies terms)
          dfs (reduce #(assoc %1 (:entstring %2) (:count %2)) {} (db/get-string-counts dterms))]
      (reduce #(assoc %1 %2 (Math/sqrt (/ (get tfs %2) (get dfs %2)))) {} dterms)))

(defn- above-avg-terms [cluster]
  "TODO: this does not make use of the coreference information found when processing the articles"
    (let [term-group (assoc-term-scores cluster)]
      (let [average-score (/ (reduce + (vals term-group))
                             (count term-group))]
        (keys
          (filter #(< average-score (val %)) term-group)))))

(defn get-related-articles [article-clusters cluster-uuid limit offset]
  (let [cluster (first (filter #(= (:id %) cluster-uuid) @article-clusters))
        search-terms (above-avg-terms cluster)
        epoch-cutoff (-> (t/hours 24) t/ago coerce/to-epoch)]
    ;TODO: time should be based on data from article-clusters and not system time
    (db/get-doc-summary
      (map :docid (db/related-article-ids search-terms epoch-cutoff limit offset)))))

(defn respond [resp]
  {:status 200
   :headers {"Content-Type" "application/json"
             "Access-Control-Allow-Origin" "*"}
   :body resp})

(defn request-documents [article-clusters page perpage]
  (respond
    (->> @article-clusters
         (sort-by :score)
         (reverse)
         (map :articles)
         (paginate perpage page))))

(defn app-routes [article-clusters]
  (compojure.core/routes

    (GET "/" [:as r]
      (let [params (sc/cluster-coercer (:params r))]
        (request-documents article-clusters (:page params) (:perpage params))))

    (GET "/related" [:as r]
      (let [params (sc/related (:params r))]
        (get-related-articles article-clusters
                              (:id params)
                              (or (:page params) 0)
                              (or (:perpage params) config/rel-articles-per-page))))))

(defn run-server [article-clusters]
  (http/run-server
    (-> (handler/site (app-routes article-clusters))
        (wrap-json-body {:keywords? true})
        (wrap-json-response))
    {:port 9002}))
