;Contains anything that is performed on a token record
(ns GraphNamedThings.opdoc
  (:require  [GraphNamedThings.util :as util])
  (:use [GraphNamedThings.inputs])
  (:import [edu.stanford.nlp.ie.machinereading.structure.Span]
           [GraphNamedThings.inputs.token]))


(defrecord entity [id rep-mention ner-tag tokens token-ids])


(defn next-group
  "Returns the next partition of tokens in a list that have either
  an interesting NER tag or are part of a coreference span.  Only intended
  for a list of tokens in it's original order"
  [tokens]
  (split-with ner-or-coref?
              (drop-while (complement ner-or-coref?) tokens)))

(defn joined-strings
  "Joins the token strings in a list.  Assumes the tokens are in their original
  order.  This needs to be changed to extract the actual substring from the
  original document instead of just guessing that each token is separated by
  a space (not always true)"
  [tokens]
  (let [strings (map #(:string %) tokens)]
    (clojure.string/join " " strings)))

(defn ner-tag-at-head
  "Returns the NER tag of the head word of the coreference span that the
  token is a part of, or the token's tag if it isn't part of a coref span"
  [token tokens]
  (if (zero? (:coref-head token))
    ;if the coref-head index is 0 then there was no coreference, so the token's NER tag is it's own
    (:ner-tag token)
    ;else, the token's NER tag is the NER tag of the token's coreference's head word
    (:ner-tag
      (first
        (filter
          #(and
             (= (:sent-index %) (:sent-index token))
             (= (:start-index %) (:coref-head token))) tokens)))))

(defn merge-tokens
  "Merge adjacent tokens with identical NER tags"
  [tokens]
  (let [[token-group tokens-rest] (next-group tokens)
        all (first token-group)]
    (cond
      (empty? token-group) nil
      :else
        (cons
          (->token
            (joined-strings token-group)
            (ner-tag-at-head all tokens)
            (:ner-id all)
            (:coref-id all)
            (:sent-index all)
            (:start-index (first token-group))
            (:end-index (last token-group))
            (map :id token-group)
            (:coref-head all))
          (merge-tokens tokens-rest)))))


(defn filter-nonentities [tokens]
  "Return a list of tokens that have an interesting NER tag or are part of
  a coreference (refer to a named entity)"
  (filter #(or
             (not= "O" (:ner-tag %))
             (not= nil (:coref-id %)))
          tokens))

(defn group-entity-tokens
  "Get the list of groups of tokens corresponding to entities found within a document given a document record"
  [tokens]
  (let [coref? #(not= (:coref-id %) nil)
        merged-filtered-tokens (-> tokens
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


(defn get-rep-mention
  "Intended to return the most representative mention of an entity.
  Implemented as a method of one of the CoreNLP coref classes"
  [grouped-token]
  "This could some day be the most representative mention!")

(defn make-id-from-grouped
  [grouped-token]
  (util/hash-string
    (clojure.string/join
      (mapcat :id grouped-token))))

(defn get-tag-from-grouped
  [grouped-token]
  (:ner-tag (first grouped-token)))


(defn tokens-from-grouped
  [grouped-token]
  (map :string grouped-token))

(defn ids-from-grouped
  [grouped-token]
  (map :id grouped-token))

;(defn extract-entities [doc-rec]
;  (let [tokens-grouped (group-entity-tokens doc-rec)]
;    (map #(->entity %1 %2 %3 %4 %5)
;         (map make-id-from-grouped tokens-grouped)
;         (map get-rep-mention tokens-grouped)
;         (map get-tag-from-grouped tokens-grouped)
;         (map tokens-from-grouped tokens-grouped)
;         (map ids-from-grouped tokens-grouped))))

(defn extract-entities [tokens]
  "Merge and filter token records to create a list of entity records grouped together"
  (let [tokens-grouped (group-entity-tokens tokens)]
    (map #(->entity %1 %2 %3 %4 %5)
         (map make-id-from-grouped tokens-grouped)
         (map get-rep-mention tokens-grouped)
         (map get-tag-from-grouped tokens-grouped)
         (map tokens-from-grouped tokens-grouped)
         (map ids-from-grouped tokens-grouped))))


(def my-words (list "Bernie Sanders won the U.S. presidential Democratic nominating contest in Wyoming on Saturday, besting rival Hillary Clinton and adding to a string of recent victories as the two candidates gear up for a crucial matchup in New York.  Sanders, a U.S. senator from Vermont, has won seven out of the last eight state-level Democratic nominating contests, chipping away at Clinton's big lead in the number of delegates needed to secure the party's nomination."
                 "Testimony by Cheryl D. Mills, chief of staff when Hillary Clinton was secretary of state, represented the first sworn public accounting from a member of Mrs. Clinton’s inner circle."))

(def processed-docs-i
(process-documents my-words))

(extract-entities (first processed-docs-i))
