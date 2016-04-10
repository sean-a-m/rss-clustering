;TODO: Functions required to aggregate coreferences

(ns GraphNamedThings.annotate
  (use GraphNamedThings.util)
  (:import edu.stanford.nlp.simple.Document)
  (:import edu.stanford.nlp.simple.Sentence)
  (:import edu.stanford.nlp.simple.SentenceAlgorithms)
  (:import edu.stanford.nlp.ie.machinereading.structure.Span))

;Annotated document, containing each token and it's corresponding named entity tag, and word and sentence indices
(defrecord annotation [token tag idx-w idx-s])
;Defines one coref mention, consisting of the ID, sentence number, and span (begin end) tuple (one-indexed)
(defrecord coref-record [id idx-s idx-start idx-end])

;annotation, coref-metnions -> coref ID that contains annotation
(defn annotation-in-mention [a ms]
  (:id
    (first
      (->> ms
           (filter #(= (:idx-s a) (:idx-s %)))
           (filter #(<= (:idx-w a) (:idx-end %)))
           (filter #(>= (:idx-w a) (:idx-start %)))))))


;CorefChain.CorefMention -> (start end) pair
(defn coref-span [coref-mention]
  (list
    (.startIndex coref-mention)
    (.endIndex coref-mention)))

;CorefChain -> coref-chain record
(defn coref-record-from-chain [coref-chain]
  (let [coref-mentions (.getMentionsInTextualOrder coref-chain)]
    (map #(->coref-record %1 %2 %3 %4)
         (repeat (.getChainID coref-chain))
         (map (fn [c] (.sentNum c)) coref-mentions)
         (map (fn [c] (.startIndex c)) coref-mentions)
         (map (fn [c] (dec (.endIndex c))) coref-mentions))))

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

;Sentence object -> list of annotation records
;create a list of annotation records.  CoreNLP tokens are 1-indexed, but sentences are 0-indexed
(defn annotate-sentence [s]
    (map #(->annotation %1 %2 %3 %4)
         (.words s)
         (.nerTags s)
         (iterate inc 1)
         (repeat (inc (.sentenceIndex s)))))

;Document object -> list of annotation records
;Generate concatenated list of annotations created from all sentences
(defn annotate-doc [d]
  (mapcat #(annotate-sentence %)
          (.sentences d)))

(defn group-words [l]
  (partition-by :tag l))

(defn my-mentions [doc]
  (let [as (annotate-doc doc)
        ms (coref-list doc)]
    (map #(annotation-in-mention % ms) as)))
