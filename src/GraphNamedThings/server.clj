(ns GraphNamedThings.server
  (:use compojure.core)
  (:require [org.httpkit.server :as http]
            [compojure.core :refer :all]
            [compojure.handler :as handler]
            [ring.util.response :refer [response]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [GraphNamedThings.config :as config]
            [GraphNamedThings.schema :as sc]))

(defn- paginate [perpage page coll]
  (let [perpage (or perpage config/perpage)
        page (or page 0)
        partitioned (partition-all perpage coll)]
    (nth partitioned page nil)))

(defn respond [resp]
  {:status 200
   :headers {"Content-Type" "application/json"
             "Access-Control-Allow-Origin" "*"}
   :body resp})

(defn request-documents [app-state page perpage]
  (respond
    (->> @app-state
         (sort-by :score)
         (reverse)
         (map :articles)
         (paginate perpage page))))

(defn app-routes [app-state]
  (compojure.core/routes

    (GET "/" [:as r]
      (let [params (sc/cluster-coercer (:params r))]
        (println (:params r))
        (println params)
        (request-documents app-state (:page params) (:perpage params))))))

(defn run-server [app-state]
  (http/run-server
    (-> (handler/site (app-routes app-state))
        (wrap-json-body {:keywords? true})
        (wrap-json-response))
    {:port 9002}))
