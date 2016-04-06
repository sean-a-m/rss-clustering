

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
        sentAlgo (SentenceAlgorithms. sent)
        NERTagsDesired '("PERSON" "LOCATION" "ORGANIZATION")]


    (println (. sent posTags))
    (println (sentencePOS sent "NNP"))
    (println (docPOS doc "NNP"))
    (println (docNER doc))
    (println (map #(.headOfSpan sentAlgo %) (toSpan (ListNERSpans (docNER doc) 1))))                      ;Print the head of each NE span
    (println (toSpan (ListNERSpansFiltered (ListNERSpans (docNER doc) 1) (docNER doc) NERTagsDesired)))   ;List the spans containing individual named entities
    (println  (corefSpansAll doc))    ;Just calling coreference function straight from CoreNLP
    (println  (corefTextAll doc))))    ;Just calling coreference function straight from CoreNLP







