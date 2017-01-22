(ns GraphNamedThings.kmeans
  (:require [clojure.core.matrix :as m]
            [loom.alg]))

(m/set-current-implementation :clatrix)


(defn cosine-sim [m1 m2]
  (let [m1m2 (* (m/length m1) (m/length m2))]
  (if (= 0.0 m1m2)
    0  ;TODO: better handling of 0 vectors (0 vectors aren't even legitimate data!)
    (/ (m/dot m1 m2) m1m2))))

(defn get-best-cluster [mu x]
    (apply max-key #(cosine-sim (val x) (val %)) mu))

(defn update-clusters [mu clusters x]
  (let [best-cluster (get-best-cluster mu x)]
    (update clusters (key best-cluster) conj (key x))))

(defn cluster-points [mu X]
  (reduce (partial update-clusters mu) {} X))

(defn new-center [X cluster]
  (m/div (apply m/add (map (partial get X) (val cluster)))
         (count (val cluster))))

(defn new-centers [X clusters]
  (map (partial new-center X) clusters))

(defn format-mat [mat]
  (zipmap (iterate inc 0) (m/slices mat 1)))

(defn find-centers
 [K mat]
  (let [X (format-mat mat)
        mu-idxs (take K (repeatedly #(rand-int (count X))))
        mu (zipmap mu-idxs (map (partial get X) mu-idxs))]
    (loop [oldmu mu itr-left 15]
      (let [clusters (cluster-points oldmu X)
            newmu (zipmap mu-idxs (new-centers X clusters))]
        (if (and (not= (vals newmu) (vals oldmu))
                 (> itr-left 0))
          (recur newmu (dec itr-left))
          clusters)))))









