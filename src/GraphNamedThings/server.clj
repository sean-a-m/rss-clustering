(ns GraphNamedThings.server
  (:use compojure.core)
  (:require [org.httpkit.server :as http]
            [compojure.core :refer :all]
            [compojure.handler :as handler]
            [ring.util.response :refer [response]]
            [cheshire.core :as ch]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]))


(defn respond [resp]
  {:status 200
   :headers {"Content-Type" "application/json"
             "Access-Control-Allow-Origin" "*"}
   :body resp})

(defn request-documents [app-state]
  (respond
    (ch/generate-string @app-state)))

(defn app-routes [app-state]
  (compojure.core/routes
    (GET "/" []
      (request-documents app-state))))

(defn runserver [app-state]
  (http/run-server
    (handler/site (app-routes app-state)) {:port 9002}))
