(ns GraphNamedThings.test.graphalg_test
  (:require [GraphNamedThings.graphalg :refer :all]
            [loom.graph]
            [loom.alg]
            [clojure.core.matrix :as m]
            [clojure.test :refer :all]))

; https://www.cs.ucsb.edu/~xyan/classes/CS595D-2009winter/MCL_Presentation2.pdf pg 10

;A graph that should have one cluster at :1
(def g
  (loom.graph/weighted-graph
    [:1 :2 2]
    [:1 :3 1]
    [:1 :4 3]
    [:2 :4 2]))

;A graph that has two obvious clusters, {:1 :2 :3 :4} and {:5 :6 :7}
(def g2
  (loom.graph/graph
    [:1 :2]
     [:1 :3]
     [:1 :4]
     [:2 :3]
     [:2 :4]
     [:2 :5]
     [:3 :4]
     [:5 :6]
     [:5 :7]
     [:6 :7]))

(def mat
  (m/array
    [[0 2 1 3]
     [2 0 0 2]
     [1 0 0 0]
     [3 2 0 0]]))

(def mat2
  (m/array
    [[1/4 1/3 1/2 1/3]
     [1/4 1/3 0 1/3]
     [1/4 0 1/2 0]
     [1/4 1/3 0 1/3]]))

(def g3
  (loom.graph/graph
    [:1 :2]
    [:1 :3]
    [:2 :3]
    [:3 :4]
    [:4 :5]
    [:4 :6]
    [:5 :6]))

(def g4
  (loom.graph/graph
    [:a :b]
    [:a :c]
    [:b :c]
    [:c :d]
    [:d :e]
    [:d :f]
    [:e :f]))

(def mat3
  (m/array
    [[1 1 1 0 0 0]
     [1 1 1 0 0 0]
     [1 1 1 1 0 0]
     [0 0 1 1 1 1]
     [0 0 0 1 1 1]
     [0 0 0 1 1 1]]))

;tolerance for float comparisons
(def ε 0.05)

;(deftest graph-to-adj-mat-test
;  (graph-to-adj-mat g2))

(deftest column-norm-test
  (is (m/equals
        (column-normalize-matrix mat)
        (m/array [[0N 1/2 1 3/5]
                  [1/3 0N 0 2/5]
                  [1/6 0N 0 0N]
                  [1/2 1/2 0 0N]])
        ε)))

(deftest add-self-loops-test
  (is (m/equals
        (add-self-loops mat)
        (m/array [[1.0 2.0 1.0 3.0]
                  [2.0 1.0 0.0 2.0]
                  [1.0 0.0 1.0 0.0]
                  [3.0 2.0 0.0 1.0]])
        ε)))


(deftest inflate-column-test
  (is (m/equals
        (inflate-column [0 1/2 0 1/6 1/3] 2)
        (m/array [0 9/14 0.0 1/14 4/14])
        ε)))

(let [mat2-expanded (expand 2 mat2)
      mat2-inflated (inflate 2 mat2-expanded)]

  (deftest expand-mat-test
    (is (m/equals
          mat2-expanded
          (m/array [[0.35 0.31 0.38 0.31]
                    [0.23 0.31 0.13 0.31]
                    [0.19 0.08 0.38 0.08]
                    [0.23 0.31 0.13 0.31]])
        ε)))

  (deftest inflate-mat-test
    (is (m/equals
          mat2-inflated
          (m/array [[0.47 0.33 0.45 0.33]
                    [0.20 0.33 0.05 0.33]
                    [0.13 0.02 0.45 0.02]
                    [0.20 0.33 0.05 0.33]])
          ε))))

(deftest mcl-iterate-test
  (is (m/equals
        (mcl-iterate 10 (column-normalize-matrix mat3))
        (m/array
          [[0 0 0 0 0 0]
           [0 0 0 0 0 0]
           [1 1 1 0 0 0]
           [0 0 0 1 1 1]
           [0 0 0 0 0 0]
           [0 0 0 0 0 0]])
        ε)))

(deftest mcl-connected-test
  (is (=
        (mcl-connected (mcl-iterate 10 (column-normalize-matrix mat3)))
        '((0 1 2) (3 4 5)))))

(deftest cluster-test
  (is (=
        (set (cluster g4))
        #{#{:d :e :f} #{:a :b :c}})))
