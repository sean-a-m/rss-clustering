(defproject GraphNamedThings "0.1.0-SNAPSHOT"
  :description "[Eventually] Generate a weighted graph of named entities"
  :url "N/A"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [digest "1.4.4"]
                 [edu.stanford.nlp/stanford-corenlp "3.6.0"]
                 [edu.stanford.nlp/stanford-corenlp "3.6.0" :classifier "models-english"]
                 [com.google.protobuf/protobuf-java "2.6.1"]  ;This should be a dependency for Stanford NLP's simple version but it isn't
                 [aysylu/loom "0.5.4"]]                       ;Not actually being used yet!
  :main ^:skip-aot GraphNamedThings.core
  :target-path "target/%s"
  :jvm-opts ["-Xms4g"]
  :profiles {:uberjar {:aot :all}})
