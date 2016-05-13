
(ns GraphNamedThings.annotate
  (use GraphNamedThings.util)
  (require [clojure.zip :as zip])
  (:import [edu.stanford.nlp.simple Document Sentence SentenceAlgorithms]
           [edu.stanford.nlp.ie.machinereading.structure Span]))


;Defines one coref mention, consisting of the ID, sentence number, and span (begin end) tuple (one-indexed)
(defrecord coref-record [id idx-s idx-start idx-end idx-head])

(defrecord doc-record [timestamp uid title source tokens text])

(defrecord token [string ner-tag ner-id doc-id coref-id sent-index start-index end-index id coref-head])


(defn token-in-mention [word-index sent-index mentions]
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
  "CorefChain -> coref-chain record"
  [coref-chain]
  (let [coref-mentions (.getMentionsInTextualOrder coref-chain)]
    (map #(->coref-record %1 %2 %3 %4 %5)
         (repeat (.getChainID coref-chain))
         (map (fn [c] (.sentNum c)) coref-mentions)
         (map (fn [c] (.startIndex c)) coref-mentions)
         (map (fn [c] (dec (.endIndex c))) coref-mentions)
         (map (fn [c] (dec (.headIndex c))) coref-mentions))))


(defn coref-list [doc]
  "Document -> list of coref-record records"
  (mapcat
    #(coref-record-from-chain %)
      (vals
        (into
          {} (java.util.HashMap.
               (.coref doc))))))

(def ner-or-coref? #(or
                      (not= (:ner-tag %) "O")
                      (not= (:coref-id %) nil)))



(defn annotate-sentence
  [s doc-id corefs-from-doc]
  (let [indices (iterate inc 1)
        sentence-index (repeat (inc (.sentenceIndex s)))
        coref-ids (map #(:id (token-in-mention %1 %2 %3)) indices sentence-index (repeat corefs-from-doc))
        coref-heads (map #((fnil inc -1) (:idx-head (token-in-mention %1 %2 %3))) indices sentence-index (repeat corefs-from-doc))
        ner-tags (partition-by #(not= "O" %) (.nerTags s))
        hash-construct (map clojure.string/join
                            (map list (.words s) indices sentence-index))]
    (map #(->token %1 %2 %3 %4 %5 %6 %7 %8 %9 %10)
         (.words s)               ;string
         (flatten ner-tags)                 ;NER tag
         (nested-index (zip/seq-zip ner-tags))   ;NER index
         (repeat doc-id)          ;doc-id
         coref-ids                ;coref-id
         sentence-index           ;sentence index
         indices                  ;start word id
         indices               ;end word id
         (map hash-string hash-construct)  ;uuid
         coref-heads)))  ;coref head

(defn annotate-doc [doc]
  (let [corefs (coref-list doc)]
    (mapcat #(annotate-sentence %1 %2 %3)
            (.sentences doc)
            (repeat "test!")
            (repeat corefs))))


(defn hash-from-doc [doc]
  (hash-string
    (.text doc)))


(defn get-timestamp
  "get the timestamp in the correct format from a document, it's a placeholder for now"
  [doc]
  "not a timestamp!")

(defn get-title
  "get the title from a document, it's a placeholder"
  [doc]
  "not a title!")

(defn get-doc-source [doc]
  "not a source!")

(defn tokens-from-doc [doc]
  (annotate-doc doc))

(defn get-text [doc]
  (.text doc))


(defn process-entities [doc]
)


(defn process-document
  "Returns a document record from a Document object"
  [doc]
  (let [doc-title (get-title doc)
        doc-timestamp (get-timestamp doc)
        doc-id (hash-from-doc doc)
        tokens (tokens-from-doc doc)
        text (get-text doc)]
    (->doc-record
      doc-title
      doc-timestamp
      doc-id
      get-doc-source
      tokens
      text)))




