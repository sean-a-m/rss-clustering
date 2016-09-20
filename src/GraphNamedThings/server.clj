(ns GraphNamedThings.server
  (:use compojure.core)
  (:require [GraphNamedThings.buildclusters :as bc]
            [org.httpkit.server :as http]
            [compojure.handler :as handler]
            [compojure.route :as route]
            [clojure.data.json :as json]
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
   :headers {"Content-Type" "text/html"}
   :body resp})

(defn request-interval [nlp-pipe]
  (fn [req]
    (let [reqmap (json/read (io/reader (:body req) :encoding "UTF-8") :key-fn keyword)]
      (if (validated? reqmap)
        (respond (json/write-str
                   (bc/get-clusters nlp-pipe (parse-datetime (:startdate reqmap)) (parse-datetime (:enddate reqmap)))))
        (respond "Bad request")))))

(defn ok [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "OK!"})

(defn echo [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body req})

(defn route-my-pipe [nlp-pipe]
  (defroutes all-routes
     (POST "/" [] (request-interval nlp-pipe))
     (GET "/" [] ok)
     (route/resources "/")))

(defn runserver [nlp-pipe]
  (http/run-server (handler/site (route-my-pipe nlp-pipe)) {:port 9002}))

