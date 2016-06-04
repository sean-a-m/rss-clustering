(ns GraphNamedThings.inputs
  (require [clojure.zip :as zip]
           [GraphNamedThings.util :as util])
  (:import [edu.stanford.nlp pipeline.StanfordCoreNLP pipeline.Annotation])
  (:import [edu.stanford.nlp.ling CoreAnnotations$SentencesAnnotation CoreAnnotations$TokensAnnotation CoreAnnotations$PartOfSpeechAnnotation CoreAnnotations$TextAnnotation CoreAnnotations$CharacterOffsetBeginAnnotation CoreAnnotations$CharacterOffsetEndAnnotation])
  (:import [edu.stanford.nlp.hcoref CorefCoreAnnotations$CorefChainAnnotation]))


;(defrecord document [text tokens sentences coref-chains])
;(defrecord sentence [text tokens sentence-index])
;(defrecord token [text index sentence-index pos ne-tag])


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


;Defines one coref mention, consisting of the ID, sentence number, and span (begin end) tuple (one-indexed)
(defrecord coref-record [id idx-s idx-start idx-end idx-head])

(defrecord doc-record [timestamp uid title source tokens text])

(defrecord token [string ner-tag ner-id coref-id sent-index start-index end-index id coref-head])

;defines an entity derived from processing a document
(defrecord t-entity [strings ner-tag pos-tag])


(defn token-in-mention
  [word-index sent-index mentions]
    (first
      (->> mentions
           (filter #(= sent-index (:idx-s %)))
           (filter #(<= word-index (:idx-end %)))
           (filter #(>= word-index (:idx-start %))))))

(defn coref-span
  "CorefChain.CorefMention -> (start end) pair"
  [coref-mention]
  (list
    (.startIndex coref-mention)
    (.endIndex coref-mention)))

(defn coref-record-from-chain
  "CorefChain -> coref-chain record.
  Filter out anything with only one item in a coreference chain since it isn't interesting as a corefence"
  [coref-chain]
  (let [coref-mentions (.getMentionsInTextualOrder coref-chain)]
    (if (< 1 (count coref-mentions))
      (map #(->coref-record %1 %2 %3 %4 %5)
           (repeat (.getChainID coref-chain))
           (map (fn [c] (.sentNum c)) coref-mentions)
           (map (fn [c] (.startIndex c)) coref-mentions)
           (map (fn [c] (dec (.endIndex c))) coref-mentions)
           (map (fn [c] (dec (.headIndex c))) coref-mentions))
      nil)))

(defn coref-chains-contained
  "List of the ID's of all coreference chains that contain a given token"
  []
  "3")


(defn coref-list
  "Document -> list of coref-record records"
  [coref-anno]
  (mapcat
    #(coref-record-from-chain %)
      (vals
        (into
          {} (java.util.HashMap.
               coref-anno)))))


(defn core-coref-list
  "Take a coreference hashmap and transform to a list of values
  TODO: this can probably be generalized to transforming a hashmap to a list"
  [coref-anno]
      (vals
        (into
          {} (java.util.HashMap.
               coref-anno))))

(defn ner-list [processed]
  (map get-ner-tag
    (get-tokens processed)))

(defn word-index-list [processed]
  (map get-word-index
       (get-tokens processed)))


(defn sentence-index-list [processed]
  (map get-sentence-index
       (get-tokens processed)))


;predicate that returns true if token record is a named entity or is part of a coreference
(def ner-or-coref? #(or
                      (not= (:ner-tag %) "O")
                      (not= (:coref-id %) nil)))

(defn ner-ids-from-tokens
  "Return a list of id's, where each unique ID corresponds to one entity, from a list of tokens
  This is just based on grouping adjacent tokens with identical entity tags (PERSON, PLACE, etc)"
  [tokens]
  (let [core-ner-list (map get-ner-tag tokens)]
    (->> core-ner-list
         (partition-by #(not= "O" %))
         (zip/seq-zip)
         (nested-index))))




(defn annotate-doc
  "Create token records from CoreNLP annotation object.
  TODO: This should be split into many more separate functions"
  [processed]
  (let [sentence-index (sentence-index-list processed)
        corefs-from-doc (coref-list (get-corefs processed))
        nes (ner-list processed)
        word-indices (word-index-list processed)
        coref-ids (map #(:id (token-in-mention %1 %2 %3)) word-indices sentence-index (repeat corefs-from-doc))
        coref-heads (map #((fnil inc -1) (:idx-head (token-in-mention %1 %2 %3))) word-indices sentence-index (repeat corefs-from-doc))
        ner-tags (partition-by #(not= "O" %) nes)
        hash-construct (map clojure.string/join
                            (map list (get-words processed) word-indices sentence-index))
        words (map get-words
               (get-tokens processed))]
    (map #(->token %1 %2 %3 %4 %5 %6 %7 %8 %9)
         words               ;string
         (flatten ner-tags)                 ;NER tag
         (nested-index (zip/seq-zip ner-tags))   ;NER index
         coref-ids                ;coref-id
         sentence-index           ;sentence index
         word-indices                  ;start word id
         word-indices               ;end word id
         (map hash-string hash-construct)  ;uuid
         coref-heads)))  ;coref head


(defn corenlp-annotate-doc
  "Run corenlp annotator on one item"
  [doc-text corenlp-obj]
  (. corenlp-obj process doc-text))


(defn process-documents
  "Takes a list of document texts and returns a list of token record lists per-document"
  [docs]
  (let [props
        (doto (java.util.Properties.)
          (.put "annotators" "tokenize, ssplit, pos, lemma, ner, parse, dcoref"))
        pipes (new StanfordCoreNLP props)
        processed (map #(corenlp-annotate-doc % pipes) docs)]
    (map #(annotate-doc %) processed)))

(defn token-ner-id-pairs-to-tokens
  "Take a list of pairs of token objects and NER ID's and return just the list of tokens"
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
        ent-start (get-word-index (first entity-list))
        ent-end (get-word-index (last entity-list))
        ent-sent (get-sentence-index (first entity-list))]
        (if (seq
          (->> coref-mentions
               (filter #(= ent-sent (.sentNum %)))
               (filter #(>= ent-end (.headIndex %)))
               (filter #(<= ent-start (.headIndex %)))))
           (.getChainID chain)
           nil)))


(defn contained-in-chain-id2
  "Returns the chain id if the coreference chain contains the mention span"
  [start end sent chain]
  (let [coref-mentions (.getMentionsInTextualOrder chain)]
    (map #(list "Chain: " (.getChainID chain) "Chain #'s:" (.sentNum %) (.startIndex %) (.endIndex %) "Mention #'s: " sent start end) coref-mentions)))


(defn coref-ids-in-entity-list
  "Returns all coref chain ID's the entity span is a member of"
  [entity-list corefs]
  (let [entity-span-start (get-word-index
                            (first entity-list))
        entity-span-end (get-word-index
                            (last entity-list))
        entity-sentence (get-sentence-index (first entity-list))]
    (let [coref-ids
      ;remove nil items from the list
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
  (let [nlp-processed (corenlp-annotate-doc doc-text pipe)
        core-tokens (get-tokens nlp-processed)
        core-corefs (core-coref-list
                      (get-corefs nlp-processed))
        ner-ids (ner-ids-from-tokens core-tokens)]
    (let [entity-list
    ;get just the list of tokens from a vector list
    (map token-ner-id-pairs-to-tokens
      ;return set of each entity
      (vals (group-by second
        ;filter out anything that isn't an entity
        (filter #(not= "O" (get-ner-tag (first %)))
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
  (let [span-start (get-token-start-offset (first ent-list))
        span-end (get-token-end-offset (last ent-list))]
    (subs doc-text span-start span-end)))


(defn entity-strings-from-group
  "Returns group of entity strings contained within a coreferentiated group"
  [ent-group doc-text]
  (map #(entity-string-from-list % doc-text) ent-group))

(defn token-entities
  "Process a document, returning a list of entity records"
  [doc-text pipe]
  (map #(entity-strings-from-group % doc-text)
       (token-groups doc-text pipe)))


(def props  (doto (java.util.Properties.)
          (.put "annotators" "tokenize, ssplit, pos, lemma, ner, parse, dcoref")))
(def pipes (new StanfordCoreNLP props))
(def words (list "The Democratic platform process is finally underway, and the main issue is this: Did the campaign of Bernie Sanders really alter the Democratic Party? The answer is not yet entirely clear, but on many key issues so far the Hillary Clinton campaign has been unwilling to commit to delivering specifics about fundamental change in America, which have been at the heart of Sanders' campaign."
                 "Testimony by Cheryl D. Mills, chief of staff when Hillary Clinton was secretary of state, represented the first sworn public accounting from a member of Mrs. Clinton’s inner circle."))

(def words2 "Insert words here")

(def processed
  (map #(corenlp-annotate-doc % pipes) words))

processed

(def processed2 (corenlp-annotate-doc words2 pipes))

(seq ())

(util/uuid!)

(token-entities words2 pipes)

(token-groups2 words2 pipes)

(core-coref-list
  (get-corefs processed2))

(token-ner-id-pairs-to-tokens
[[1 2] [3 4]])

(clojure.set/union #{1 2 3 4})




;(map #(annotate-doc %) processed)

;(def test-corefs
;(coref-list
; (get-corefs processed)))

;processed


  (get-corefs processed2)

(get-words (first processed))

(ner-list (first processed))

(first words)

(entity-string-from-list
  (take 2
  (rest
    (get-tokens (first processed))))
  (first words))
;(annotate-doc (first processed))


;(process-documents words)

;(-> words
;    (process-documents)
;    (opdoc/merge-tokens)
;    (opdoc/filter-nonentities))
