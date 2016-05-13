;WIP to convert scratchpad into unit tests
(ns GraphNamedThings.core-test
  (:require [clojure.test :refer :all]
            [GraphNamedThings.core :refer :all])
  (use GraphNamedThings.annotate)
  (use GraphNamedThings.document)
  (use GraphNamedThings.util)
  (use GraphNamedThings.testutil)
  (require [clojure.zip :as zip])
  (:import edu.stanford.nlp.simple.Document)
  (:import edu.stanford.nlp.simple.Sentence)
  (:import edu.stanford.nlp.simple.SentenceAlgorithms)
  (:import edu.stanford.nlp.ie.machinereading.structure.Span))

(defn print-corefs [doc]
  (vals
        (into
          {} (java.util.HashMap.
               (.coref doc)))))

(def words "Bernie Sanders won the U.S. presidential Democratic nominating contest in Wyoming on Saturday, besting rival Hillary Clinton and adding to a string of recent victories as the two candidates gear up for a crucial matchup in New York.  Sanders, a U.S. senator from Vermont, has won seven out of the last eight state-level Democratic nominating contests, chipping away at Clinton's big lead in the number of delegates needed to secure the party's nomination.")

(def doc (Document. words))

(def s (Sentence. words))

(def corefs (coref-list doc))

(def indices (iterate inc 1))

(def ner-tags (partition-by #(not= "O" %) (.nerTags s)))

(def sentence-index (repeat 1))

(def coref-ids (map #(token-in-mention %1 %2 %3) indices sentence-index (repeat corefs)))

corefs

(token-in-mention 1 1 corefs)

coref-ids

(.words s)

ner-tags


(annotate-sentence s "id" corefs)

(def annotated (annotate-doc doc))

annotated

(map #(ner-tag-at-head % annotated) annotated)

annotated


(def merged-filtered
(filter-nonentities
  (merge-tokens annotated)))

merged-filtered

(group-entity-tokens processed)

(next-group annotated)

(hash-string "wakka wakka wakka")

(def processed (process-document doc))

(write-doc processed)

processed

; (read-doc "test/doc")

(def nest-list '(("O") ("O") (7 6 5) ("O") (1 7 4) ("O" "O" "O") (3 2) ("O") ("O")))

nest-list

(def zip-list (zip/seq-zip nest-list))

zip-list

(nested-index zip-list)

(defn make-id-list [is s-is ws]
  (map clojure.string/join
    (map list is s-is ws)))

indices

(def grouped-tokens (group-entity-tokens processed))

grouped-tokens

(extract-entities processed)
