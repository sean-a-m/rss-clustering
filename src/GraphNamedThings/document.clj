(ns GraphNamedThings.document
  (use GraphNamedThings.util)
  (use GraphNamedThings.annotate)
  (:import edu.stanford.nlp.simple.Document)
  (:import edu.stanford.nlp.simple.Sentence)
  (:import edu.stanford.nlp.simple.SentenceAlgorithms)
  (:import edu.stanford.nlp.ie.machinereading.structure.Span))


(defrecord entity [id rep-mention ner-tag tokens token-ids])

;Retrieve next group of entities
(defn next-group [t]
  (split-with ner-or-coref?
              (drop-while (complement ner-or-coref?) t)))

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
            (:end-index (last token-group))
            (map :id token-group))
          (merge-tokens tokens-rest)))))

;Return a list of tokens that have an NER tag
(defn filter-nonentities [tokens]
  (filter #(or
             (not= "O" (:ner-tag %))
             (not= nil (:coref-id %)))
          tokens))


;Get the list of groups of tokens corresponding to entities found within a document given a document record
(defn group-entity-tokens [doc-rec]
  (let [coref? #(not= (:coref-id %) nil)
        merged-filtered-tokens (-> (:tokens doc-rec)
                                   (merge-tokens)
                                   (filter-nonentities))]
    (concat
      (map seq
        (vals
          (group-by :coref-id
            (filter coref? merged-filtered-tokens))))
      ;each nil coref is a separate group
      (partition 1
          (filter (complement coref?) merged-filtered-tokens)))))


(defn get-rep-mention [grouped-token]
  "The most representative mention!")

(defn make-id-from-grouped [grouped-token]
  (hash-string
    (clojure.string/join
      (mapcat :id grouped-token))))

(defn get-tag-from-grouped [grouped-token]
  (:ner-tag (first grouped-token)))


(defn tokens-from-grouped [grouped-token]
  (map :string grouped-token))

(defn ids-from-grouped [grouped-token]
  (map :id grouped-token))

(defn extract-entities [doc-rec]
  (let [tokens-grouped (group-entity-tokens doc-rec)]
    (map #(->entity %1 %2 %3 %4 %5)
         (map make-id-from-grouped tokens-grouped)
         (map get-rep-mention tokens-grouped)
         (map get-tag-from-grouped tokens-grouped)
         (map tokens-from-grouped tokens-grouped)
         (map ids-from-grouped tokens-grouped))))




