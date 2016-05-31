(ns GraphNamedThings.inputs
  (use GraphNamedThings.util)
  (require [clojure.zip :as zip])
  (:import [edu.stanford.nlp pipeline.StanfordCoreNLP pipeline.Annotation])
  (:import [edu.stanford.nlp.ling CoreAnnotations$SentencesAnnotation CoreAnnotations$TokensAnnotation CoreAnnotations$PartOfSpeechAnnotation CoreAnnotations$TextAnnotation])
  (:import [edu.stanford.nlp.hcoref CorefCoreAnnotations$CorefChainAnnotation]))




(def corenlp-sentence edu.stanford.nlp.ling.CoreAnnotations$SentencesAnnotation)
(def corenlp-coref edu.stanford.nlp.hcoref.CorefCoreAnnotations$CorefChainAnnotation)
(def corenlp-token edu.stanford.nlp.ling.CoreAnnotations$TokensAnnotation)
(def corenlp-netag edu.stanford.nlp.ling.CoreAnnotations$NamedEntityTagAnnotation)
(def corenlp-text edu.stanford.nlp.ling.CoreAnnotations$TextAnnotation)
(def corenlp-sent-index edu.stanford.nlp.ling.CoreAnnotations$SentenceIndexAnnotation)
(def corenlp-word-index edu.stanford.nlp.ling.CoreAnnotations$IndexAnnotation)


;(defrecord document [text tokens sentences coref-chains])
;(defrecord sentence [text tokens sentence-index])
;(defrecord token [text index sentence-index pos ne-tag])


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


(defn coref-list [coref-anno]
  "Document -> list of coref-record records"
  (mapcat
    #(coref-record-from-chain %)
      (vals
        (into
          {} (java.util.HashMap.
               coref-anno)))))

(defn ner-list [processed]
  (map get-ner-tag
    (get-tokens processed)))

(defn word-index-list [processed]
  (map get-word-index
       (get-tokens processed)))


(defn sentence-index-list [processed]
  (map get-sentence-index
       (get-tokens processed)))



(def ner-or-coref? #(or
                      (not= (:ner-tag %) "O")
                      (not= (:coref-id %) nil)))



(defn annotate-doc
  [processed doc-id corefs-from-doc nes]
  (let [sentence-index (sentence-index-list processed)
        word-indices (word-index-list processed)
        coref-ids (map #(:id (token-in-mention %1 %2 %3)) word-indices sentence-index (repeat corefs-from-doc))
        coref-heads (map #((fnil inc -1) (:idx-head (token-in-mention %1 %2 %3))) word-indices sentence-index (repeat corefs-from-doc))
        ner-tags (partition-by #(not= "O" %) nes)
        hash-construct (map clojure.string/join
                            (map list (get-words processed) word-indices sentence-index))
        words (map get-words
               (get-tokens processed))]
    (map #(->token %1 %2 %3 %4 %5 %6 %7 %8 %9 %10)
         words               ;string
         (flatten ner-tags)                 ;NER tag
         (nested-index (zip/seq-zip ner-tags))   ;NER index
         (repeat doc-id)          ;doc-id
         coref-ids                ;coref-id
         sentence-index           ;sentence index
         word-indices                  ;start word id
         word-indices               ;end word id
         (map hash-string hash-construct)  ;uuid
         coref-heads)))  ;coref head








(defn process-documents
  [docs]
  (let [props
        (doto (java.util.Properties.)
          (.put "annotators" "tokenize, ssplit, pos, lemma, ner, parse, dcoref"))
        pipes (new StanfordCoreNLP props)
        processed (. pipes process docs)]
    (annotate-doc
      processed
      "id"
      (coref-list
        (get-corefs processed))
      (ner-list processed))))



(def props  (doto (java.util.Properties.)
          (.put "annotators" "tokenize, ssplit, pos, lemma, ner, parse, dcoref")))
(def pipes (new StanfordCoreNLP props))
(def words "Bernie Sanders won the U.S. presidential Democratic nominating contest in Wyoming on Saturday, besting rival Hillary Clinton and adding to a string of recent victories as the two candidates gear up for a crucial matchup in New York.  Sanders, a U.S. senator from Vermont, has won seven out of the last eight state-level Democratic nominating contests, chipping away at Clinton's big lead in the number of delegates needed to secure the party's nomination.")
(def processed (. pipes process words))

(coref-list
  (get-corefs processed))

;(-> words
;    (process-documents)
;    (opdoc/merge-tokens)
;    (opdoc/filter-nonentities))
