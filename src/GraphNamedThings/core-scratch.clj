
;This file is currently just being used as a scratch pad
(ns GraphNamedThings.core
  (use GraphNamedThings.NERLib)
  (use GraphNamedThings.annotate)
  (use GraphNamedThings.document)
  (use GraphNamedThings.util)
  (use GraphNamedThings.testutil)
  (require [clojure.zip :as zip])
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
        sent-algo (SentenceAlgorithms. sent)]

    (process-document doc)))


(defn print-corefs [doc]
  (vals
        (into
          {} (java.util.HashMap.
               (.coref doc)))))

(def words "Bernie Sanders won the U.S. presidential Democratic nominating contest in Wyoming on Saturday, besting rival Hillary Clinton and adding to a string of recent victories as the two candidates gear up for a crucial matchup in New York.  Sanders, a U.S. senator from Vermont, has won seven out of the last eight state-level Democratic nominating contests, chipping away at Clinton's big lead in the number of delegates needed to secure the party's nomination.")

(def doc (Document. words))

(def s (Sentence. words))

(def corefs (coref-list doc))

(first corefs)

(print-corefs doc)



corefs


(def indices (iterate inc 1))



(def ner-tags (partition-by #(not= "O" %) (.nerTags s)))

(def sentence-index (repeat 1))



(take 10 indices)

(take 10 sentence-index)

(take 10 corefs)

corefs

(token-in-mention 1 1 corefs)

(def coref-ids (map #(token-in-mention %1 %2 %3) indices sentence-index (repeat corefs)))

coref-ids


(.words s)
ner-tags





  (def ner-tags-zip (zip/seq-zip ner-tags))

  ner-tags-zip


nested-index

(def parted (partition-by #(not= "O" %) (.nerTags s)))

parted

(nested-index (zip/seq-zip parted))

(def doc-id "doc id")



(annotate-sentence s "id" corefs)

(def annotated (annotate-doc doc))




annotated

(dec (:coref-head (first (flatten annotated))))



(map #(ner-tag-at-head % annotated) annotated)





annotated


(def merged-filtered
(filter-nonentities
  (merge-tokens annotated)))

merged-filtered



  (map seq
       (vals
(group-by :coref-id merged-filtered)))

(second (split-with (partial > 10) [1 2 3 2 1]))


(def processed-doc (process-document doc))


(group-entity-tokens processed-doc)




(next-group annotated)






(.words s)
ner-tags


(hash-string "wakka wakka wakka")

   ; (map #(->token %1 %2 %3 %4 %5 %6 %7 %8)
     ;    (.words s)               ;string
   ;      (flatten ner-tags)                 ;NER tag
   ;      (nested-index (zip/seq-zip ner-tags))   ;NER index
    ;     (repeat doc-id)          ;doc-id
   ;      coref-ids                ;coref-id
    ;     sentence-index           ;sentence index
    ;     indices                  ;start word id
    ;     indices)               ;end word id




(process-document doc)

(def processed (process-document doc))

(write-doc processed)

processed

(read-doc "test/doc")

processed2


(def nest-list '(("O") ("O") (7 6 5) ("O") (1 7 4) ("O" "O" "O") (3 2) ("O") ("O")))


nest-list



(def zip-list (zip/seq-zip nest-list))

zip-list

(nth nest-list 2)

(count (filter seq? nest-list ))


(nested-index zip-list)



(->
(-> zip-list
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    ) zip/up zip/lefts)


(-> zip-list
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/up
    zip/lefts
    )


(zip/path
(-> zip-list
    zip/next
    zip/next
    zip/next
    zip/next))



(hash-string "asdlkjsadfsakMMMMMMlfds")



(defn make-id-list [is s-is ws]
  (map clojure.string/join
    (map list is s-is ws)))

indices

(map hash-string
  (make-id-list indices sentence-index (.words s)))

(map clojure.string/join
  (make-id-list indices sentence-index (.words s)))

(defn make-an-entity-id-from-grouped [grouped-token]
  (hash-string
    (clojure.string/join
      (mapcat :id grouped-token))))

(group-entity-tokens processed-doc)


(def grouped-tokens (group-entity-tokens processed-doc))

grouped-tokens

(mapcat :id (first grouped-tokens))
(make-an-entity-id-from-grouped
  (last grouped-tokens))

(defn the-tokens-from-grouped [grouped-token]
  (map :string grouped-token))

(the-tokens-from-grouped
  (first grouped-tokens))


(extract-entities processed-doc)
