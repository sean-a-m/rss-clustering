(ns GraphNamedThings.nlputil
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
