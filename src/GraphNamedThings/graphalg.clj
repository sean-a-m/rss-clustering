(ns GraphNamedThings.graphalg
  (:require [loom.graph]
            [clojure.core.matrix :as m]))


(defn add-self-loops
  "Adds self-looping edges to the adjacency matrix corresponding to each node on the graph.
  TODO: Maybe this shouldn't be it's own function since it should never be used after normalizing the matrix.
  Or possibly integrate this into the graph construction step"
  [mat]
  ;TODO: Check for non-square
  (m/add
    (m/identity-matrix
      ;Can take either value in shape since matrix is always 2D, square
      (first
        (m/shape
          mat)))
    mat))

(defn normalize-column
  [col]
  (m/scale
    col
    (/ 1 (m/esum col))))

(defn inflate-column
  [col exp]
  (normalize-column
    (m/pow col exp)))

(defn column-normalize-matrix
  [mat]
  (m/transpose
    (map normalize-column mat)))

(defn expand
  "Expand step of MCL algorithm (matrix exponentiation)"
  [e mat]
  (reduce m/mmul (repeat e mat)))

(defn inflate
  "Inflate step of the MCL algorithm"
  [e mat]
  (m/transpose
    (map #(inflate-column % e) (m/columns mat))))

(defn mcl-iterate
  "Iterate Markov Cluster Algorithm until solution converges or iteration limit is reached"
  [mat limit]
  ;TODO: consider the problem of cycles that don't converge
  ;TODO: consider using mutable arrays
  (let [ε 0.025
        new-mat (->> mat
                    (expand 2)
                    (inflate 2))]
    (if (or (zero? limit)
            (m/equals mat new-mat ε))
      new-mat
      (recur new-mat (dec limit)))))

(defn nonzero-indices
  "Return indices of non-zero elements in a row.  Shouldn't be used with floats"
  [row]
  (keep-indexed
    #(if-not (zero? %2) %1)
    row))


(defn mcl-connected
  "Return indices of connected element sets"
  [mat]
  (let [ε 0.001
        attractors (m/gt
                     (m/main-diagonal
                       mat)
                     ε)
        attractor-indices (nonzero-indices attractors)
        attractor-rows (map (partial m/get-row mat) attractor-indices)]
    (map #(nonzero-indices (m/gt % ε)) attractor-rows)))


