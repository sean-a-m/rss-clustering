(defproject GraphNamedThings "tbd"
  :description "tbd"
  :url "N/A"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [edu.stanford.nlp/stanford-corenlp "3.7.0"]
                 [edu.stanford.nlp/stanford-corenlp "3.7.0" :classifier "models-english"]
                 [aysylu/loom "0.5.4"]
                 [org.jsoup/jsoup "1.9.1"]
                 [net.mikera/core.matrix "0.52.2"]
                 [org.clojure/java.jdbc "0.6.1"]
                 [org.postgresql/postgresql "9.4-1206-jdbc41"]
                 [org.clojure/math.combinatorics "0.1.3"]
                 [korma "0.4.3"]
                 [clj-time "0.12.0"]
                 [http-kit "2.1.18"]
                 [ring/ring-json "0.4.0"]
                 [ring/ring-defaults "0.2.1"]
                 [compojure "1.5.1"]
                 [javax.servlet/javax.servlet-api "3.1.0"]
                 [clj-postgresql "0.4.0"]
                 ;[clmcll/clmcll "0.1.0-SNAPSHOT"]
                 [clatrix "0.5.0"]
                 [danlentz/clj-uuid "0.1.6"]
                 [cheshire "5.7.0"]
                 [medley "0.8.4"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [prismatic/schema "1.1.3"]]
  :main ^:skip-aot GraphNamedThings.core
  :target-path "target/%s"
  :jvm-opts ["-Xmx1600m"]
  :global-vars {*unchecked-math* true
                *assert* false}
  :profiles {:uberjar {:aot :all}})
