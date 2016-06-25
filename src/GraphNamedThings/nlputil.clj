(ns GraphNamedThings.nlputil
  (require [clojure.set :as cset]
           [clojure.core.matrix :as matrix]))

(defn cosine-sim
  "Cosine similarity of two vectors.  Takes words, not numerical vectors!
  TODO: make the bottom part less ugly
  TODO: generalize to vectors and move word->vector logic somewhere else"
  [w1 w2]
  (let [f1 (frequencies w1)
        f2 (frequencies w2)
        intersect (cset/intersection (into #{} (keys f1)) (into #{} (keys f2)))
        wf1 (select-keys f1 intersect)
        wf2 (select-keys f2 intersect)
        v1 (matrix/array (vals wf1))
        v2 (matrix/array (vals wf2))]
    (let [numr (matrix/dot v1 v2)
          denm (* (matrix/length (matrix/array (vals f1))) (matrix/length (matrix/array (vals f2))))]
      (/ numr denm))))

(defn jaccard
  "Jaccard coefficient.  Accepts many types of sequences"
  [w1 w2]
  (let [v1 (into #{} w1)
        v2 (into #{} w2)]
    (double
    (/
      (count (cset/intersection v1 v2))
      (count (cset/union v1 v2))))))

(defn longest-matching
  "Return the character count of the longest matching strings between two sets of words"
  [s1 s2]
  (count
    (last
      (sort-by count
        (cset/intersection s1 s2)))))

(defn contained-in-mention?
  "Is the start and end of span contained within this coref mention?
  For whatever reason CoreNLP coreference indices are zero indexed but token indices aren't"
  [start end sent mention]
  (println "start: " start "end: " end "mention: " mention)
  (and
    (<= end (dec (.endIndex mention)))
    (>= start (dec (.startIndex mention)))
    (= sent (.sentIndex mention))))




