
(ns GraphNamedThings.core
  (use GraphNamedThings.NERLib)
  (use GraphNamedThings.annotate)
  (use GraphNamedThings.util)
  (use GraphNamedThings.document)
  (use GraphNamedThings.pipe)
  (require [clojure.zip :as zip])
  (:import edu.stanford.nlp.simple.Document)
  (:import edu.stanford.nlp.simple.Sentence)
  (:import edu.stanford.nlp.simple.SentenceAlgorithms)
  (:import edu.stanford.nlp.ie.machinereading.structure.Span))


(defn -main
  [& args]
  (let [words "Bernie Sanders won the U.S. presidential Democratic nominating contest in Wyoming on Saturday, besting rival Hillary Clinton and adding to a string of recent victories as the two candidates gear up for a crucial matchup in New York.  Sanders, a U.S. senator from Vermont, has won seven out of the last eight state-level Democratic nominating contests, chipping away at Clinton's big lead in the number of delegates needed to secure the party's nomination."
        doc (Document. words)
        sent (Sentence. "Bernie Sanders won the U.S. presidential Democratic nominating contest in Wyoming on Saturday, besting rival Hillary Clinton and adding to a string of recent victories as the two candidates gear up for a crucial matchup in New York.  Sanders, a U.S. senator from Vermont, has won seven out of the last eight state-level Democratic nominating contests, chipping away at Clinton's big lead in the number of delegates needed to secure the party's nomination.")
        sent-algo (SentenceAlgorithms. sent)]

    (println
      (-> doc
          (annotate-doc)
          (merge-tokens)
          (filter-nonentities)))))

