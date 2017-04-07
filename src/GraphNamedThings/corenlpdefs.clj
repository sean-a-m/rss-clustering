(ns GraphNamedThings.corenlpdefs
  (:import (edu.stanford.nlp.ling CoreAnnotations$SentencesAnnotation
                                  CoreAnnotations$TokensAnnotation
                                  CoreAnnotations$TextAnnotation
                                  CoreAnnotations$CharacterOffsetBeginAnnotation
                                  CoreAnnotations$CharacterOffsetEndAnnotation
                                  CoreAnnotations$SentenceIndexAnnotation
                                  CoreAnnotations$IndexAnnotation
                                  CoreAnnotations$NamedEntityTagAnnotation)
           (edu.stanford.nlp.hcoref CorefCoreAnnotations$CorefChainAnnotation)))

(defn get-sentences
  [annotated]
  (.get annotated CoreAnnotations$SentencesAnnotation))

(defn get-corefs
  [annotated]
  (.get annotated CorefCoreAnnotations$CorefChainAnnotation))

(defn get-tokens
  [annotated]
  (.get annotated CoreAnnotations$TokensAnnotation))

(defn get-ner-tag
  [token]
  (.get token CoreAnnotations$NamedEntityTagAnnotation))

(defn get-words
  [any]
  (.get any CoreAnnotations$TextAnnotation))

(defn get-sentence-index
  [token]
  (inc
    (.get token CoreAnnotations$SentenceIndexAnnotation)))

(defn get-word-index
  [token]
    (.get token CoreAnnotations$IndexAnnotation))

(defn get-token-start-offset
  [token]
  (.get token CoreAnnotations$CharacterOffsetBeginAnnotation))

(defn get-token-end-offset
  [token]
  (.get token CoreAnnotations$CharacterOffsetEndAnnotation))

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
