(ns GraphNamedThings.docpr
  (require [GraphNamedThings.inputs :as inputs]
           [GraphNamedThings.util :as util]
           [clojure.string :as str])
   (:import [edu.stanford.nlp pipeline.StanfordCoreNLP pipeline.Annotation]))


(defrecord document [timestamp title entities])

(defn best-coref
  "Returns the id of the best coreference for the entity in the existing map (create new uuid if none exists)
  TODO: consider whether a new ID should just be the record hash again"
  [ent-rec ent-map]
  (util/uuid!))


(defn add-entity-to-map
  "Add entity record to map of entities, either generating a new uuid for it or using an existing uuid (for all entities corresponding
  to the same coreference)"
  [ent-rec ent-map]
  (assoc ent-map
    ent-rec
    (best-coref ent-rec ent-map)))

(defn word-set
  "Return set of all individual words within one entity"
  [ent-rec]
  (set
    (mapcat #(str/split % #"\s") (:strings ent-rec))))

(defn index-word-entries
  "Add all words in an entity record to an index pointing from word to list of entity records containing that word"
  [ent-rec ent-index]
  (let [f (fn [x] (conj x ent-rec))]
    (reduce #(update %1 (keyword %2) f) ent-index (word-set ent-rec))))

(defn index-entities
  "Add all words in a set of entities (from a document, for example) to a hashmap where the set of words contained in all entities is the set of keys and the set of entities containing those words are the values"
  [ent-recs ent-index]
  (reduce #(index-word-entries %2 %1) ent-index ent-recs))








  (def props  (doto (java.util.Properties.)
          (.put "annotators" "tokenize, ssplit, pos, lemma, ner, parse, dcoref")))
(def pipes (new StanfordCoreNLP props))
(def words2 "The Democratic platform process is finally underway, and the main issue is this: Did the campaign of Bernie Sanders really alter the Democratic Party? The answer is not yet entirely clear, but on many key issues so far the Hillary Clinton campaign has been unwilling to commit to delivering specifics about fundamental change in America, which have been at the heart of Sanders' campaign.")

(inputs/token-entities words2 pipes)

(add-entity-to-map (inputs/token-entities words2 pipes) {})


(second (inputs/token-entities words2 pipes))

(index-word-entries (second (inputs/token-entities words2 pipes)) {(keyword "Bernie") (list (last (inputs/token-entities words2 pipes)))})
(conj (list (last (inputs/token-entities words2 pipes))) (second (inputs/token-entities words2 pipes)))
(last (inputs/token-entities words2 pipes))

(word-set
(second (inputs/token-entities words2 pipes)))

(inputs/token-entities words2 pipes)

(index-entities (inputs/token-entities words2 pipes) {})

(index-entities (inputs/token-entities words2 pipes)
  (index-entities (inputs/token-entities words2 pipes) {}))

