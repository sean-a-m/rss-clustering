(ns GraphNamedThings.server
  (:use compojure.core)
  (:require [GraphNamedThings.buildclusters :as bc]
            [org.httpkit.server :as http]
            [compojure.core :refer :all]
            [compojure.handler :as handler]
            [compojure.route :as route]
            [ring.util.response :refer [response]]
            [cheshire.core :as ch]
            [clojure.data.json :as json]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clojure.java.io :as io]))

(defn validated?
  "Validate that both timestamps are present, start < end, etc"
  [reqmap]
  (and (contains? reqmap :startdate)
       (contains? reqmap :enddate)))

(defn parse-datetime
  [str]
  (f/parse (f/formatters :basic-date-time-no-ms) str))

(defn respond [resp]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body resp})

(defn request-documents [app-state]
  (response
    (ch/generate-string @app-state)))
;(json/write-str @app-state)))

(defn app-routes [app-state]
  (compojure.core/routes
    (GET "/" []
      (request-documents app-state))))

(defn runserver [app-state]
  (http/run-server
    (handler/site (app-routes app-state)) {:port 9002}))
