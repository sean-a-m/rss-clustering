;TODO: Functions required to aggregate coreferences

(ns GraphNamedThings.annotate
  (use GraphNamedThings.util)
  (require [clojure.zip :as zip])
  (:import edu.stanford.nlp.simple.Document)
  (:import edu.stanford.nlp.simple.Sentence)
  (:import edu.stanford.nlp.simple.SentenceAlgorithms)
  (:import edu.stanford.nlp.ie.machinereading.structure.Span))

;Annotated document, containing each token and it's corresponding named entity tag, and word and sentence indices
(defrecord annotation [token tag idx-w idx-s])
;Defines one coref mention, consisting of the ID, sentence number, and span (begin end) tuple (one-indexed)
(defrecord coref-record [id idx-s idx-start idx-end idx-head])

(defrecord entity [uid rep-mention ner-tag entities])

(defrecord doc-record [timestamp uid title source tokens text])

(defrecord token [string ner-tag ner-id doc-id coref-id sent-index start-index end-index])

(defrecord processed-doc [entities document])



;annotation, coref-mentions -> coref ID that contains annotation
(defn annotation-in-mention [a ms]
  (:id
    (first
      (->> ms
           (filter #(= (:idx-s a) (:idx-s %)))
           (filter #(<= (:idx-w a) (:idx-end %)))
           (filter #(>= (:idx-w a) (:idx-start %)))))))

(defn token-in-mention [word-index sent-index mentions]
  (:id
    (first
      (->> mentions
           (filter #(= sent-index (:idx-s %)))
           (filter #(<= word-index (:idx-end %)))
           (filter #(>= word-index (:idx-start %)))))))


;CorefChain.CorefMention -> (start end) pair
(defn coref-span [coref-mention]
  (list
    (.startIndex coref-mention)
    (.endIndex coref-mention)))

;CorefChain -> coref-chain record
(defn coref-record-from-chain [coref-chain]
  (let [coref-mentions (.getMentionsInTextualOrder coref-chain)]
    (map #(->coref-record %1 %2 %3 %4 %5)
         (repeat (.getChainID coref-chain))
         (map (fn [c] (.sentNum c)) coref-mentions)
         (map (fn [c] (.startIndex c)) coref-mentions)
         (map (fn [c] (dec (.endIndex c))) coref-mentions)
         (map (fn [c] (dec (.headIndex c))) coref-mentions))))

;Document -> list of coref-record records
(defn coref-list [doc]
  (mapcat
    #(coref-record-from-chain %)
      (vals
        (into
          {} (java.util.HashMap.
               (.coref doc))))))

(defn print-corefs [doc]
  (vals
        (into
          {} (java.util.HashMap.
               (.coref doc)))))

(def ner? #(not= (:ner-tag %) "O"))

;Retrieve next group of entities
(defn next-group [t]
  (split-with ner?
              (drop-while (complement ner?) t)))

(defn joined-strings [tokens]
  (let [strings (map #(:string %) tokens)]
    (clojure.string/join " " strings)))

;merge adjacent tokens with identical NER tags
(defn merge-tokens [tokens]
  (let [[token-group tokens-rest] (next-group tokens)
        all (first token-group)]
    (cond
      (empty? token-group) nil
      :else
        (cons
          (->token
            (joined-strings token-group)
            (:ner-tag all)
            (:ner-id all)
            (:doc-id all)
            (:coref-id all)
            (:sent-index all)
            (:start-index (first token-group))
            (:end-index (last token-group)))
          (merge-tokens tokens-rest)))))

;Return a list of tokens that have an NER tag
(defn filter-nonentities [tokens]
  (filter #(not= "O" (:ner-tag %)) tokens))

(defn annotate-sentence [s doc-id corefs-from-doc]
  (let [indices (iterate inc 1)
        sentence-index (repeat (inc (.sentenceIndex s)))
        coref-ids (map #(token-in-mention %1 %2 %3) indices sentence-index (repeat corefs-from-doc))
        ner-tags (partition-by #(not= "O" %) (.nerTags s))]
    (map #(->token %1 %2 %3 %4 %5 %6 %7 %8)
         (.words s)               ;string
         (flatten ner-tags)                 ;NER tag
         (nested-index (zip/seq-zip ner-tags))   ;NER index
         (repeat doc-id)          ;doc-id
         coref-ids                ;coref-id
         sentence-index           ;sentence index
         indices                  ;start word id
         indices)))               ;end word id

(defn annotate-doc [d]
  (let [corefs (coref-list d)]
    (mapcat #(annotate-sentence %1 %2 %3)
            (.sentences d)
            (repeat "test!")
            (repeat corefs))))


;should generate a  unique id for a document
;right now it's random but maybe it should always be the same for a given document
(defn uuid-from-string [s]
  (uuid))

;get the timestamp in the correct format from a document
;it's a placeholder for now
(defn get-timestamp [doc]
  "not a timestamp!")

;get the title from a document
;it's a placeholder
(defn get-title [doc]
  "not a title!")

(defn get-doc-source [doc]
  "not a source!")

(defn entities-from-doc [doc]
  (annotate-sentence doc))

(defn get-text [doc]
  (.text doc))


(defn process-entities [doc]
)

;Returns a document record, including a title
(defn process-document [doc]
  (let [doc-title (get-title doc)
        doc-timestamp (get-timestamp doc)
        doc-id (uuid-from-string doc)
        entities (entities-from-doc doc)
        text (get-text doc)]
    (->doc-record doc-title doc-timestamp doc-id get-doc-source entities text)))




