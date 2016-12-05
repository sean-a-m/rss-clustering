(ns GraphNamedThings.inputs
  (:require [GraphNamedThings.util :as util]
           [GraphNamedThings.nlputil :as nlputil]
           [GraphNamedThings.corenlpdefs :as nlpdefs]
            [clj-uuid :as uuid]
            [GraphNamedThings.config :as config])
  (:import [edu.stanford.nlp pipeline.StanfordCoreNLP pipeline.Annotation]
           [edu.stanford.nlp.ling CoreAnnotations$SentencesAnnotation CoreAnnotations$TokensAnnotation CoreAnnotations$PartOfSpeechAnnotation CoreAnnotations$TextAnnotation CoreAnnotations$CharacterOffsetBeginAnnotation CoreAnnotations$CharacterOffsetEndAnnotation]
           [edu.stanford.nlp.hcoref CorefCoreAnnotations$CorefChainAnnotation]))

;defines an entity derived from processing a document
(defrecord entity [strings ner-tag])

(defn ner-ids-from-tokens
  "Return a list of id's, where each unique ID corresponds to one entity, from a list of tokens
  This is just based on grouping adjacent tokens with identical entity tags (PERSON, PLACE, etc)"
  [tokens]
  (let [core-ner-list (map nlpdefs/get-ner-tag tokens)]
    (flatten
      (map-indexed #(repeat (count %2) %1)
                   (partition-by #(not= "O" %) core-ner-list)))))

(defn contained-in-chain-id
  "Returns the chain id if the coreference chain contains the mention span"
  [entity-list chain]
  (let [coref-mentions (.getMentionsInTextualOrder chain)
        ent-start (nlpdefs/get-word-index (first entity-list))
        ent-end (nlpdefs/get-word-index (last entity-list))
        ent-sent (nlpdefs/get-sentence-index (first entity-list))]
    (if (seq
          (->> coref-mentions
               (filter #(= ent-sent (.sentNum %)))
               (filter #(>= ent-end (.headIndex %)))
               (filter #(<= ent-start (.headIndex %)))))
      (.getChainID chain)
      nil)))

(defn coref-ids-in-entity-list
  "Returns all coref chain ID's the entity span is a member of"
  [entity-list corefs]
  (let [coref-ids
        ;remove nil items from the list, since they only represent an the result of a list of entities that was not contained in a coreference chain (not relevant)
        (remove nil?
                (map #(contained-in-chain-id entity-list %) corefs))]
    ;Give items without a coreference their own ID
    (if (seq coref-ids)
      coref-ids
      (uuid/v1))))

(defn- get-token-group-string
  [tokens]
  (map nlpdefs/get-words tokens))

(defn- vector-strings
  "Turn a vector of tokens into a string."
  ;TODO: Assuming tokens are separated by a space only makes sense for adjacent tokens with identical named entity tags..maybe
  [obj-vector]
  (clojure.string/lower-case
    (clojure.string/join " " (get-token-group-string obj-vector))))

(defn collect-entities
  "Collect tokens into sequences of matching entities"
  [tokens]
  (->> tokens
       (partition-by #(not= "O" (nlpdefs/get-ner-tag %)))
       (remove #(= "O" (nlpdefs/get-ner-tag (first %))))))

(defn- collect-corefs
  "Group token spans by matching coreferences"
  [corefs collected-entities]
  (vals
    (group-by #(coref-ids-in-entity-list % corefs) collected-entities)))

(defn- create-entity-from-vector
  "Create an entity record from a list of vectors of corenlp tokens"
  [ent-vector]
  {:strings (map vector-strings ent-vector)
   :tag (nlpdefs/get-ner-tag (first (first ent-vector))) ;All of the tokens in the list should be the same; this takes the first token inside the first vector in the list
   :id (uuid/v1)})

(defn get-entities-from-document-vector
  "Retrieve entities from the vector of tokens and coreference list from a document annotation"
  [tokens corefs]
  (->> tokens
      (collect-entities)
      (collect-corefs corefs)
      (map create-entity-from-vector)))

(defn get-document-entities
  "Retrieve the list of entity records belonging to one document annotation"
  [annotation]
  (let [tokens (nlpdefs/get-tokens annotation)
        corefs (vals (nlpdefs/get-corefs annotation))]  ;get-corefs returns a hash map of java corefchain objects
    (get-entities-from-document-vector tokens corefs)))

(defn get-document-entities-new
  "Retrieve the list of entity records belonging to one document annotation"
  [annotation]
  (let [tokens (nlpdefs/get-tokens annotation)
        corefs (vals (nlpdefs/get-corefs annotation))]  ;get-corefs returns a hash map of java corefchain objects
    (get-entities-from-document-vector tokens corefs)))

(defn get-entity-lists
  "Process a list of documents, returning a list of entity records"
  [doc-text pipe]
  (let [annotations (nlputil/text-list-to-annotations doc-text)]
    (do
      (. pipe annotate annotations)
      (let [annotations-shortened (nlputil/text-list-to-annotations
                                    (map #(take config/max-sentences (nlpdefs/get-sentences %)) annotations))]
	(println doc-text)
        (map get-document-entities annotations)))))

(defn get-entity-list
  "Process a document, returning a list of entity records"
  [doc-text pipe]
  (let [annotation (nlputil/text-list-to-annotation doc-text)]
    (do
      (. pipe annotate annotation)
      (let [annotation-shortened (nlputil/text-list-to-annotation
                                    (take config/max-sentences (nlpdefs/get-sentences annotation)))]
        (println doc-text)
        (get-document-entities-new annotation)))))


