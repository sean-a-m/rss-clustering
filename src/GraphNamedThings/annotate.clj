
(ns GraphNamedThings.annotate
  (use GraphNamedThings.util)
  (require [clojure.zip :as zip])
  (:import edu.stanford.nlp.simple.Document)
  (:import edu.stanford.nlp.simple.Sentence)
  (:import edu.stanford.nlp.simple.SentenceAlgorithms)
  (:import edu.stanford.nlp.ie.machinereading.structure.Span))


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

(def ner-or-coref? #(or
                      (not= (:ner-tag %) "O")
                      (not= (:coref-id %) nil)))



(defn annotate-sentence [s doc-id corefs-from-doc]
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

(defn annotate-doc [d]
  (let [corefs (coref-list d)]
    (mapcat #(annotate-sentence %1 %2 %3)
            (.sentences d)
            (repeat "test!")
            (repeat corefs))))


;calls a hash function with the document text, which isn't totally unique
(defn hash-from-doc [doc]
  (hash-string
    (.text doc)))

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

(defn tokens-from-doc [doc]
  (annotate-doc doc))

(defn get-text [doc]
  (.text doc))


(defn process-entities [doc]
)

;Returns a document record, including a title
(defn process-document [doc]
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




