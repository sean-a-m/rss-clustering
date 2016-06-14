(ns GraphNamedThings.nlputil
  (require [clojure.set :as cset]
           [clojure.core.matrix :as matrix]
           )
    (:import [edu.stanford.nlp.ling CoreAnnotations$SentencesAnnotation CoreAnnotations$TokensAnnotation CoreAnnotations$PartOfSpeechAnnotation CoreAnnotations$TextAnnotation CoreAnnotations$CharacterOffsetBeginAnnotation CoreAnnotations$CharacterOffsetEndAnnotation])
  )

;Wrappers for retrieving various items from corenlp annotation

(def corenlp-sentence edu.stanford.nlp.ling.CoreAnnotations$SentencesAnnotation)
(def corenlp-coref edu.stanford.nlp.hcoref.CorefCoreAnnotations$CorefChainAnnotation)
(def corenlp-token edu.stanford.nlp.ling.CoreAnnotations$TokensAnnotation)
(def corenlp-netag edu.stanford.nlp.ling.CoreAnnotations$NamedEntityTagAnnotation)
(def corenlp-text edu.stanford.nlp.ling.CoreAnnotations$TextAnnotation)
(def corenlp-sent-index edu.stanford.nlp.ling.CoreAnnotations$SentenceIndexAnnotation)
(def corenlp-word-index edu.stanford.nlp.ling.CoreAnnotations$IndexAnnotation)
(def corenlp-char-offset-begin edu.stanford.nlp.ling.CoreAnnotations$CharacterOffsetBeginAnnotation)
(def corenlp-char-offset-end edu.stanford.nlp.ling.CoreAnnotations$CharacterOffsetEndAnnotation)

(defn get-sentences
  [annotated]
  (.get annotated corenlp-sentence))

(defn get-corefs
  [annotated]
  (.get annotated corenlp-coref))

(defn get-tokens
  [annotated]
  (.get annotated corenlp-token))

(defn get-ner-tag
  [token]
  (.get token corenlp-netag))

(defn get-words
  [any]
  (.get any corenlp-text))

(defn get-sentence-index
  [token]
  (inc
    (.get token corenlp-sent-index)))

(defn get-word-index
  [token]
    (.get token corenlp-word-index))

(defn get-token-start-offset
  [token]
  (.get token corenlp-char-offset-begin))

(defn get-token-end-offset
  [token]
  (.get token corenlp-char-offset-end))

(defn ner-list [processed]
  (map get-ner-tag
    (get-tokens processed)))

(defn word-index-list [processed]
  (map get-word-index
       (get-tokens processed)))


(defn sentence-index-list [processed]
  (map get-sentence-index
       (get-tokens processed)))

(defn corenlp-annotate-doc
  "Run corenlp annotator on one item"
  [doc-text corenlp-obj]
  (. corenlp-obj process doc-text))


(defn cosine-sim
  "Cosine similarity of two vectors.  Takes words, not numerical vectors!
  TODO: make the bottom part less ugly
  TODO: generalize to vectors and move word->vector logic somewhere else"
  [w1 w2]
  (let [f1 (frequencies w1)
        f2 (frequencies w2)
        intersect (cset/intersection (into #{} (keys f1)) (into #{} (keys f2)))
        wf1 (select-keys f1 intersect)
        wf2 (select-keys f2 intersect)
        v1 (matrix/array (vals wf1))
        v2 (matrix/array (vals wf2))]
    (let [numr (matrix/dot v1 v2)
          denm (* (matrix/length (matrix/array (vals f1))) (matrix/length (matrix/array (vals f2))))]
      (/ numr denm))))

(defn jaccard
  "Jaccard coefficient.  Accepts many types of sequences"
  [w1 w2]
  (let [v1 (into #{} w1)
        v2 (into #{} w2)]
    (/
      (count (cset/intersection v1 v2))
      (count (cset/union v1 v2)))))




