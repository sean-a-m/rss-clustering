(ns GraphNamedThings.inputs
  (require [GraphNamedThings.util :as util]
           [GraphNamedThings.nlputil :as nlputil]
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
    (flatten
      (map-indexed #(repeat (count %2) %1)
                   (partition-by #(not= "O" %) core-ner-list)))))

(defn token-ner-id-pairs-to-tokens
  "Take a list of pairs of token objects and NER ID's and return just the list of tokens
  TODO: This doesn't make sense, individual lists exist before the pair of lists exists"
  [token-ner-id-pairs]
  (map first token-ner-id-pairs))

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
        (util/uuid!))))

(defn token-group-from-vector
  [token-vector]
  (map :token token-vector))

;TODO: separate group and filters
(defn group-and-filter-tokens
  "Group tokens by span and filter out non-entities"
  [nlp-tokens]
  (let [ner-ids (ner-ids-from-tokens nlp-tokens)]
    (->> nlp-tokens
         (map #(hash-map :span-id %1 :token %2) ner-ids) ;create a list of tokens and corresponding span id's
         (filter #(not= "O" (nlpdefs/get-ner-tag (:token %))))  ;remove tokens with no entity tag
         (group-by :span-id)  ;group token objects by the span ids
         (vals) ;take only the values (index isn't important)
         (map token-group-from-vector))))  ;get collections of the tokens from each span (remaining in the correct order)

(defn create-entities-from-tokens
  [nlp-tokens corefs]
  (->> nlp-tokens
       (group-and-filter-tokens)
       (map #(hash-map :coref-id (first (coref-ids-in-entity-list % corefs)) :tokens %))   ;TODO: just taking the first id here isn't accurate
       (group-by :coref-id)
       (vals)))



;will rename and remove normal token-groups once refactoring is complete
(defn token-groups
  "Return all tokens records in a document given text and a CoreNLP pipeline object.
  This generates the following structure:
  -List of all entities
    -List of all spans representing an entity
      -List of all tokens in a span
        -Token (type CoreLabel)
  TODO: There may be something in CoreNLP that is better suited for referencing a span than a list of tokens
  TODO: should probably fold instead of group-by on NER IDs
  TODO: This function is way too big, at least split off the part defining the list of entities
  TODO: Something shouldn't be part of multiple coreferences.  Need to fix grouping for that"
  [nlp-processed]
  (let [core-tokens (nlpdefs/get-tokens nlp-processed)
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

;TODO: Change doc-text to take text from annotation object to simplify arguments in later functions in this file
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

(defn token-entities-from-document
  "Return a list of entity records from a single document"
  [doc-text annotated]
  (map #(entity-strings-from-group % doc-text)
       (token-groups annotated)))

; "Process a list of documents, returning a list of entity records"
(defn token-entities
  [doc-text pipe]
  (let [annotations (nlputil/text-list-to-annotations doc-text)]
    (do
      (. pipe annotate annotations)
      (map #(token-entities-from-document (nlpdefs/get-words %) %) annotations))))

