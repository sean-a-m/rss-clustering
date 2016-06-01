
(ns GraphNamedThings.core
  (require [GraphNamedThings.annotate :as annotate]
           [GraphNamedThings.inputs :as input]
           [GraphNamedThings.util :as util]
           [GraphNamedThings.opdoc :as opdoc]
           [GraphNamedThings.pipe :as pipe]
           [clojure.zip :as zip]))

(defn -main
  [& args]
  (let [words "Bernie Sanders won the U.S. presidential Democratic nominating contest in Wyoming on Saturday, besting rival Hillary Clinton and adding to a string of recent victories as the two candidates gear up for a crucial matchup in New York.  Sanders, a U.S. senator from Vermont, has won seven out of the last eight state-level Democratic nominating contests, chipping away at Clinton's big lead in the number of delegates needed to secure the party's nomination."]
    (println
      (-> words
        (input/process-documents)
        (opdoc/merge-tokens)
        (opdoc/filter-nonentities)))))


(def words "Bernie Sanders won the U.S. presidential Democratic nominating contest in Wyoming on Saturday, besting rival Hillary Clinton and adding to a string of recent victories as the two candidates gear up for a crucial matchup in New York.  Sanders, a U.S. senator from Vermont, has won seven out of the last eight state-level Democratic nominating contests, chipping away at Clinton's big lead in the number of delegates needed to secure the party's nomination.")
      (-> words
        (input/process-documents)
        (opdoc/merge-tokens)
        (opdoc/filter-nonentities))

(def doc-list (list "Bernie Sanders won the U.S. presidential Democratic nominating contest in Wyoming on Saturday, besting rival Hillary Clinton and adding to a string of recent victories as the two candidates gear up for a crucial matchup in New York.  Sanders, a U.S. senator from Vermont, has won seven out of the last eight state-level Democratic nominating contests, chipping away at Clinton's big lead in the number of delegates needed to secure the party's nomination."
                 "Testimony by Cheryl D. Mills, chief of staff when Hillary Clinton was secretary of state, represented the first sworn public accounting from a member of Mrs. Clinton’s inner circle."))

  (map #(opdoc/extract-entities %)
       (input/process-documents doc-list))
