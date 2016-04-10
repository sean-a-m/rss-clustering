

(ns GraphNamedThings.core
  (use GraphNamedThings.NERLib)
  (use GraphNamedThings.annotate)
  (:import edu.stanford.nlp.simple.Document)
  (:import edu.stanford.nlp.simple.Sentence)
  (:import edu.stanford.nlp.simple.SentenceAlgorithms)
  (:import edu.stanford.nlp.ie.machinereading.structure.Span))


;Testing functions here, the really long/ugly function calls will help to identify required interfaces etc
(defn -main
  [& args]
  (let [words "Bernie Sanders won the U.S. presidential Democratic nominating contest in Wyoming on Saturday, besting rival Hillary Clinton and adding to a string of recent victories as the two candidates gear up for a crucial matchup in New York.  Sanders, a U.S. senator from Vermont, has won seven out of the last eight state-level Democratic nominating contests, chipping away at Clinton's big lead in the number of delegates needed to secure the party's nomination."
        doc (Document. words)
        sent (Sentence. "Bernie Sanders won the U.S. presidential Democratic nominating contest in Wyoming on Saturday, besting rival Hillary Clinton and adding to a string of recent victories as the two candidates gear up for a crucial matchup in New York.  Sanders, a U.S. senator from Vermont, has won seven out of the last eight state-level Democratic nominating contests, chipping away at Clinton's big lead in the number of delegates needed to secure the party's nomination.")
        sent-algo (SentenceAlgorithms. sent)
        ner-tags-desired '("PERSON" "LOCATION" "ORGANIZATION")]


    ;(println (. sent posTags))
    ;(println (sentence-pos sent "NNP"))
    ;(println (doc-pos doc "NNP"))
    ;(println (doc-ner doc))
    ;(println (map #(.headOfSpan sent-algo %) (to-span (list-ner-spans (doc-ner doc) 1))))                      ;Print the head of each NE span
    ;(println (to-span (list-ner-spans-filtered (list-ner-spans (doc-ner doc) 1) (doc-ner doc) ner-tags-desired)))   ;List the spans containing individual named entities
    ;(println  (coref-spans-all doc))    ;Just calling coreference function straight from CoreNLP
    (println (my-mentions doc))
    (println (coref-list doc))
    (println (print-corefs doc))
    (println (group-words
               (annotate-doc doc)))))
    ;(println (generate-coref-hash-list doc))))
    ;(println (coref-map doc))
    ;(println (coref-mentions doc))
    ;(println (.words sent))
    ;(println (.mentions sent))))


    ;(println  (coref-text-all doc))))    ;Just calling coreference function straight from CoreNLP







