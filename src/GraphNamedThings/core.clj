

(ns GraphNamedThings.core
  (use GraphNamedThings.NERLib)
  (:import edu.stanford.nlp.simple.Document)
  (:import edu.stanford.nlp.simple.Sentence)
  (:import edu.stanford.nlp.simple.SentenceAlgorithms)
  (:import edu.stanford.nlp.ie.machinereading.structure.Span))


;Testing functions here, the really long/ugly function calls will help to identify required interfaces etc
(defn -main
  [& args]
  (let [words "The political stakes were higher than at any of the previous debates because this was one of the last high-profile, widely seen opportunities for Donald J. Trump’s rivals to sow doubts about his candidacy and to slow his march to the nomination.  Mr. Trump is ahead in public opinion polls in Florida, Illinois and North Carolina; the race appears closer in Ohio, with some polls indicating that John Kasich, the governor there, has a small lead. Missouri also votes next week."
        doc (Document. words)
        sent (Sentence. "The political stakes were higher than at any of the previous debates because this was one of the last high-profile, widely seen opportunities for Donald J. Trump’s rivals to sow doubts about his candidacy and to slow his march to the nomination.  Mr. Trump is ahead in public opinion polls in Florida, Illinois and North Carolina; the race appears closer in Ohio, with some polls indicating that John Kasich, the governor there, has a small lead. Missouri also votes next week.")
        sent-algo (SentenceAlgorithms. sent)
        ner-tags-desired '("PERSON" "LOCATION" "ORGANIZATION")]


    (println (. sent posTags))
    (println (sentence-pos sent "NNP"))
    (println (doc-pos doc "NNP"))
    (println (doc-ner doc))
    (println (map #(.headOfSpan sent-algo %) (to-span (list-ner-spans (doc-ner doc) 1))))                      ;Print the head of each NE span
    (println (to-span (list-ner-spans-filtered (list-ner-spans (doc-ner doc) 1) (doc-ner doc) ner-tags-desired)))   ;List the spans containing individual named entities
    (println  (coref-spans-all doc))    ;Just calling coreference function straight from CoreNLP
    (println  (coref-text-all doc))))    ;Just calling coreference function straight from CoreNLP







