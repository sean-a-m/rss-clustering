(ns GraphNamedThings.test.inputs_test
  (:require [clojure.set :as cset]
            [clojure.core.matrix :as matrix]
            [GraphNamedThings.nlputil :refer :all]
            [GraphNamedThings.test.testdata :refer :all]
            [clojure.test :refer :all]))

(deftest cosine-sim-correct
  (let [v1 '("glutathione" "homocystine" "transhydrogenase")
        v2 '("glutathione" "coa" "glutathione" "transhydrogenase")]\
    ;close enough
    (is (< 0.7 (cosine-sim v1 v2) 3))
    (is (> 0.72 (cosine-sim v1 v2) 3))))

(deftest jaccard-correct
  (let [v1 '("A" "B" "C" "D" "E")
        v2 '("I" "H" "G" "F" "E" "D")]
    (is (< 0.22 (jaccard v1 v2)))
    (is (> 0.23 (jaccard v1 v2)))))

(deftest jaccard-correct-set
  (let [v1 '("A" "B" "C" "D" "D" "D" "D" "A" "A" "E")
        v2 '("I" "H" "G" "F" "E" "D")]
    (is (< 0.22 (jaccard v1 v2)))
    (is (> 0.23 (jaccard v1 v2)))))


