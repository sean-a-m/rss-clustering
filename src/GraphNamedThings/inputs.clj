(ns GraphNamedThings.inputs
  (require [clojure.zip :as zip]
           [GraphNamedThings.util :as util]
           [GraphNamedThings.corenlpdefs :as nlpdefs])
  (:import [edu.stanford.nlp pipeline.StanfordCoreNLP pipeline.Annotation]
           [edu.stanford.nlp.ling CoreAnnotations$SentencesAnnotation CoreAnnotations$TokensAnnotation CoreAnnotations$PartOfSpeechAnnotation CoreAnnotations$TextAnnotation CoreAnnotations$CharacterOffsetBeginAnnotation CoreAnnotations$CharacterOffsetEndAnnotation]
           [edu.stanford.nlp.hcoref CorefCoreAnnotations$CorefChainAnnotation]))

;defines an entity derived from processing a document
(defrecord entity [strings ner-tag])

;TODO: Decide if this should be removed
(defrecord doc-record [timestamp uid title source tokens text])

(defn ner-ids-from-tokens
  "Return a list of id's, where each unique ID corresponds to one entity, from a list of tokens
  This is just based on grouping adjacent tokens with identical entity tags (PERSON, PLACE, etc)"
  [tokens]
  (let [core-ner-list (map nlpdefs/get-ner-tag tokens)]
    (->> core-ner-list
         (partition-by #(not= "O" %))
         (zip/seq-zip)
         (util/nested-index))))

(defn token-ner-id-pairs-to-tokens
  "Take a list of pairs of token objects and NER ID's and return just the list of tokens
  TODO: This doesn't make sense, individual lists exist before the pair of lists exists"
  [token-ner-id-pairs]
  (map first token-ner-id-pairs))

(defn contained-in-mention?
  "Is the start and end of span contained within this coref mention?
  For whatever reason CoreNLP coreference indices are zero indexed but token indices aren't"
  [start end sent mention]
  (println "start: " start "end: " end "mention: " mention)
  (and
    (<= end (dec (.endIndex mention)))
    (>= start (dec (.startIndex mention)))))

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
  (let [entity-span-start (nlpdefs/get-word-index (first entity-list))
        entity-span-end (nlpdefs/get-word-index (last entity-list))
        entity-sentence (nlpdefs/get-sentence-index (first entity-list))]
    (let [coref-ids
      ;remove nil items from the list, since they only represent an the result of a list of entities that was not contained in a coreference chain (not relevant)
      (remove nil?
        (map #(contained-in-chain-id entity-list %) corefs))]
      ;Give items without a coreference their own ID
      (if (seq coref-ids)
        coref-ids
        (util/uuid!)))))



(defn token-groups
  "Return all tokens records in a document given text and a CoreNLP pipeline object
  TODO: There may be something in CoreNLP that is better suited for referencing a span than a list of tokens
  TODO: should probably fold instead of group-by on NER IDs
  TODO: This function is way too big, at least split off the part defining the list of entities
  TODO: Something shouldn't be part of multiple coreferences.  Need to fix grouping for that"
  [doc-text pipe]
  (let [nlp-processed (nlpdefs/corenlp-annotate-doc doc-text pipe)
        core-tokens (nlpdefs/get-tokens nlp-processed)
        core-corefs (util/core-coref-list
                      (nlpdefs/get-corefs nlp-processed))
        ner-ids (ner-ids-from-tokens core-tokens)]
    (let [entity-list
    ;get just the list of tokens from a vector list
    (map token-ner-id-pairs-to-tokens
      ;return set of each entity
      (vals (group-by second
        ;filter out anything that isn't an entity
        (filter #(not= "O" (nlpdefs/get-ner-tag (first %)))
                ;vector of token objects and NER ID #'s
                (map vector core-tokens ner-ids)))))]
      ;only take tokens
      (map (fn [v] (map #(first %) v))
      (vals
      ;group entity tokens by coref id
        (group-by #(first (second %))
        ;create entity-tokens - coref id's tuples
          (map vector
               entity-list
              (map #(coref-ids-in-entity-list % core-corefs) entity-list))))))))


(defn entity-string-from-list
  "Returns the string corresponding to the span defined by a list of tokens
  TODO: Replace with library function that I never found"
  [ent-list doc-text]
  (let [span-start (nlpdefs/get-token-start-offset (first ent-list))
        span-end (nlpdefs/get-token-end-offset (last ent-list))]
    (subs doc-text span-start span-end)))

(defn entity-ner-tag-from-group
  "Returns NER tag from a group of tokens
  TODO: make this smarter about what to do if tags conflict (or throw an error)"
  [ent-group]
  (nlpdefs/get-ner-tag
    (first
      (first
        ent-group))))

(defn entity-strings-from-group
  "Returns group of entity strings contained within a coreferentiated group"
  [ent-group doc-text]
  (->entity
    (map (fn [e] (entity-string-from-list e doc-text)) ent-group)
    (entity-ner-tag-from-group ent-group)))


(defn token-entities
  "Process a document, returning a list of entity records"
  [doc-text pipe]
  (map #(entity-strings-from-group % doc-text)
       (token-groups doc-text pipe)))

