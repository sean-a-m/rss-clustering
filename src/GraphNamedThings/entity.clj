(ns GraphNamedThings.entity
  (require [GraphNamedThings.inputs :as inputs]
           [GraphNamedThings.util :as util]
           [GraphNamedThings.nlputil :as nlputil]
           [clojure.string :as str])
   (:import [edu.stanford.nlp pipeline.StanfordCoreNLP pipeline.Annotation]))


(defrecord document [timestamp title entities])

(defn word-set
  "Return set of all individual words within one entity"
  [ent-rec]
  (set
    (mapcat #(str/split % #"\s") (:strings ent-rec))))


(defn ent-similarity
  "Return a list of entity keys and similarity values for a list ent-list compared against an entity record ent-rec"
  [ent-rec ent-list]
    (map #(vector % (nlputil/jaccard
                       (word-set ent-rec)
                       (word-set %)))
         ent-list))

(defn best-coref
  "Returns the id of the best coreference for the entity in the existing map (create new uuid if none exists)
  TODO: consider whether a new ID should just be the record hash again
  TODO: consider adding check for matching NER tag"
  [ent-rec ent-map ent-index]
  ;first filter for matching entity candidates by selecting anything with a matching word
  (let [candidates (into #{}
                     (flatten
                       (vals
                         (select-keys ent-index (word-set ent-rec)))))
        ;minimum similarity before rejecting matches
        min-threshold 0.25]
    ;If there's at least one candidate then find the candidate with the best similarity coefficient, otherwise return a new id
    (if (seq candidates)
      (let [best-ent-candidate
            (apply max-key second
               (ent-similarity ent-rec candidates))]
        ;if the best candidate passes a minimum similarity threshold, return that entity, otherwise, return a new id
        (if (> (second best-ent-candidate) min-threshold)
          (first best-ent-candidate)
          (util/uuid!)))
      (util/uuid!))))


(defn add-entity-to-map
  "Add entity record to map of entities, either generating a new uuid for it or using an existing uuid (for all entities corresponding
  to the same coreference)"
  [ent-rec ent-map ent-index]
  (assoc ent-map
    ent-rec
    (best-coref ent-rec ent-map ent-index)))

(defn index-word-entries
  "Add all words in an entity record to an index pointing from word to list of entity records containing that word"
  [ent-rec ent-index]
  (let [f (fn [x] (conj x ent-rec))]
    (reduce #(update %1 %2 f) ent-index (word-set ent-rec))))

(defn index-entities
  "Add all words in a set of entities (from a document, for example) to a hashmap where the set of words contained in all entities is the set of keys and the set of entities containing those words are the values"
  [ent-recs ent-index]
  (reduce #(index-word-entries %2 %1) ent-index ent-recs))



