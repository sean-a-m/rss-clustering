(ns GraphNamedThings.feature-matrix
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as linear]
            [loom.alg]))

(m/set-current-implementation :clatrix)

(defn create-vector-index
  [ent-recs]
  "Create a list of terms and corresponding indices"
    (zipmap
      (distinct (map :entstring ent-recs))
      (iterate inc 0)))

(defn create-doc-index
  [ent-recs]
  (zipmap
    (distinct (map :docid ent-recs))
    (iterate inc 0)))

(defn calculate-svd
  [ent-recs]
  (let [vector-idx (create-vector-index ent-recs)
        doc-idx (create-doc-index ent-recs)]
    (linear/svd
       (loop [mat (m/zero-matrix (count vector-idx) (count doc-idx)) cur-ent-recs ent-recs]
        (if (not-empty cur-ent-recs)
          (let [cur-ent (first cur-ent-recs)]
            (m/mset! mat (get vector-idx (:entstring cur-ent)) (get doc-idx (:docid cur-ent)) 1)
            (recur
              mat
              (rest cur-ent-recs)))
          mat)))))

(defn recalc-svd-matrix [k mat]
  (let [top-k (concat (take k (:S mat)) (repeat (- (count (:S mat)) k) 0))]
    (m/mmul (:U mat) (m/diagonal-matrix top-k) (:V* mat))))

(defn create-feature-matrix [ent-recs]
  (recalc-svd-matrix (/ (count ent-recs) 2)
    (calculate-svd ent-recs)))






