
(ns GraphNamedThings.core
  (require [GraphNamedThings.annotate :as annotate]
           [GraphNamedThings.util :as util]
           [GraphNamedThings.opdoc :as opdoc]
           [GraphNamedThings.pipe :as pipe]
           [clojure.zip :as zip])
  (:import [edu.stanford.nlp.simple Document Sentence SentenceAlgorithms]))

(defn -main
  [& args]
  (let [words "Bernie Sanders won the U.S. presidential Democratic nominating contest in Wyoming on Saturday, besting rival Hillary Clinton and adding to a string of recent victories as the two candidates gear up for a crucial matchup in New York.  Sanders, a U.S. senator from Vermont, has won seven out of the last eight state-level Democratic nominating contests, chipping away at Clinton's big lead in the number of delegates needed to secure the party's nomination."
        doc (Document. words)
        sent (Sentence. "Bernie Sanders won the U.S. presidential Democratic nominating contest in Wyoming on Saturday, besting rival Hillary Clinton and adding to a string of recent victories as the two candidates gear up for a crucial matchup in New York.  Sanders, a U.S. senator from Vermont, has won seven out of the last eight state-level Democratic nominating contests, chipping away at Clinton's big lead in the number of delegates needed to secure the party's nomination.")
        sent-algo (SentenceAlgorithms. sent)]

    (println
      (-> doc
          (annotate/annotate-doc)
          (opdoc/merge-tokens)
          (opdoc/filter-nonentities)))))

