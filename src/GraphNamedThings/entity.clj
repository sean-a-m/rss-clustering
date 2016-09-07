(ns GraphNamedThings.entity
  (require [GraphNamedThings.inputs :as inputs]
           [GraphNamedThings.util :as util]
           [GraphNamedThings.nlputil :as nlputil]
           [GraphNamedThings.corenlpdefs :as nlpdefs]
           [clojure.string :as str]
           [clojure.set :as cset]))


(defn word-set
  "Return set of all individual words within one entity"
  [ent-rec]
  (set
    (mapcat #(str/split % #"\s") (:strings ent-rec))))

(defn ent-similarity
  "Return a list of entity keys and similarity values for a list ent-list compared against an entity record ent-rec"
  [ent-rec ent-list]
    (map #(vector % (nlputil/longest-matching (word-set ent-rec)
                                              (word-set %)))
         ent-list))

(defn best-coref
  "Returns the id of the best coreference for the entity in the existing map (create new uuid if none exists)
  TODO: consider whether a new ID should just be the record hash again
  TODO: consider adding check for matching NER tag"
  [ent-rec candidates]
  ;first filter for matching entity candidates by selecting anything with a matching word
  (let [;minimum similarity before rejecting matches, this is zero right now because it's sorting on the matching word with the highest length
        ;and if there are no words the longest-matching function will return 0
        min-threshold 0]
    ;If there's at least one candidate then find the candidate with the best similarity coefficient, otherwise return a new id
    (if (seq candidates)
      (let [best-ent-candidate
            (apply max-key second
                   (ent-similarity ent-rec candidates))]
        ;if the best candidate passes a minimum similarity threshold, return that entity, otherwise, return itself
        (if (> (second best-ent-candidate) min-threshold)
          (first best-ent-candidate)
          ent-rec))
      ent-rec)))

(defn index-word-entries
  "Add all words in an entity record to an index pointing from word to list of entity records containing that word"
  [ent-rec ent-index]
  (let [f (fn [x] (conj x ent-rec))]
    (reduce #(update %1 %2 f) ent-index (word-set ent-rec))))

(defn index-entities
  "Add all words in a set of entities (from a document, for example) to a hashmap where the set of words contained in all entities is the set of keys and the set of entities containing those words are the values"
  [ent-recs ent-index]
  (reduce #(index-word-entries %2 %1) ent-index ent-recs))

(defn get-coref-candidates
  "Return all possible coreference candidates"
  [ent-index ent-rec ent-recs]
  (let [filtered-ent-recs (filter #(= (:ner-tag %) (:ner-tag ent-rec)) ent-recs)]  ;filter out anything that doesn't have a matching tag
    (cset/intersection
      (into #{} filtered-ent-recs)
      (into #{}
            (flatten
              (vals
                (select-keys ent-index (word-set ent-rec))))))))

(defn merge-entity
  "Merge coreferent entities, returning a map pointing from each entity to a coreference or an id"
  [ent-index ent-map ent-recs]
  (let [candidates (get-coref-candidates ent-index (first ent-recs) (rest ent-recs))]
    (assoc ent-map
      (first ent-recs)
      (best-coref (first ent-recs) candidates))))

(defn get-entity-merge-map
  "Return a map of entity records returning to coreferent records"
  [ent-map ent-recs]
  (let [ent-index (index-entities ent-recs {})]
    (reduce (partial merge-entity ent-index) ent-map (util/tails ent-recs))))

;TODO: record sounds like record
(defn get-entity-id
  "Iterate through the index of entities until hitting the one where the key and value are identical (doesn't point to
  any other entity"
  [ent-map record]
  (let [new-record (get ent-map record)]
    (if (= new-record record)
        record
        (recur ent-map new-record))))






































