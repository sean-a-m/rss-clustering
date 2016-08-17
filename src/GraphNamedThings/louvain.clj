(ns GraphNamedThings.louvain
  (:require [loom.graph]
            [loom.alg]
            [clojure.core.matrix :as m]
            [clojure.set :as cset]
            [GraphNamedThings.util :as util]
            [clojure.math.combinatorics :as combo]
            [clojure.core.matrix.dataset :as ds]))


(defn init-community
  "Returns the set of communities where each community contains one graph node"
  [g]
  (apply merge
    (map #(hash-map % (list %)) (:nodeset g))))

(defn maplistreverse
  [m]
  (apply merge
  (map (fn [k] (zipmap (get m k) (repeat k))) (keys m))))

(defn distinct-edges-set
  "Returns the distinct edges of the graph as sets"
  [g]
  (loom.alg/distinct-edges g))

(defn inside-edges
  ""
  [c g]
  (distinct-edges-set (loom.graph/subgraph g c)))

(defn get-keyval-node
  "Get the key value pair for one node k in an adjacency list"
  [adj-list k]
  (zipmap
    (map #(into #{} (vector k (key %))) (get adj-list k))
    (vals (get adj-list k))))

(defn community-edges
  "Get all edges connected to the community of nodes c.  c must be a set"
  [c g]
  (let [all-nodes (select-keys (:adj g) c)]
    (apply merge
      (map (partial get-keyval-node all-nodes) c))))

(defn connected-nodes
  "Nodes connected to community c in g"
  [c g]
  (into #{}
    (mapcat keys
            (vals
              (select-keys (:adj g) c)))))

(defn inside-edges-sum
  "Sum of the edge weights from each node inside c to other node inside c"
  [c g]
  (reduce +
          (map
            #(get
              (get (:adj g) (first %))
              (second %))
            (inside-edges c g))))

(defn outside-connections-sum [c g]
  "Sum of the edges connecting from nodes outside c to nodes inside c"
  (reduce +
    (map second
     (remove #(every? (into #{} c) (first %)) (community-edges c g)))))

(defn ki
  "Sum of weights of links to node i"
  [g i]
  (reduce +
    (vals
      (get (:adj g) i))))

(defn ki-in
  "Sum of weights of links from node i to community c"
  [g c i]
  (reduce +
    (vals
      (select-keys (get (:adj g) i) c))))

(defn sum-connections-between
  [g c1 c2]
  (reduce +
    (map
      #(reduce +
        (vals
          (select-keys (get (:adj g) %) c2))) c1)))

(defn sum-all-weights
  "The sum of all weights in an undirected weighted graph"
  [g]
  (let [get-in-adj (partial get-in (:adj g))]
    (reduce +
      (map get-in-adj
           (distinct-edges-set g)))))

(defn dQQ
  "Change in modularity from moving i into C"
  [Ein kiin Etot ki-l m]
  (-
    (-
      (/ (+ Ein kiin) (* 2 m))
      (Math/pow (/ (+ Etot ki-l) (* 2 m)) 2))
    (-
      (/ Ein (* 2 m))
      (Math/pow (/ Etot (* 2 m)) 2)
      (Math/pow (/ ki-l (* 2 m)) 2))))

(defn dQ-of-i-c
  "Calculate delta Q for moving node i into C"
  [g i c]
  (let [Ein (inside-edges-sum c g)
        Etot (outside-connections-sum c g)
        kiin (ki-in g c i)
        ki-l (ki g i)
        m (sum-all-weights g)]
    (dQQ Ein kiin Etot ki-l m)))

(defn remove-node
  "Return list of communities with node n removed from community c in communities cs"
  [c n cs]
  (let [old-c-vals (get cs c)]
    (if (> (count old-c-vals) 1)
      (update cs c #(remove (partial = n) %))
      (dissoc cs c))))

(defn add-node
  "add node n to comm c in cs"
  [c n cs]
  (update cs c #(cons n %)))

(defn update-comms
  "update and return list of communities for community that i is moved into
  i - node
  cs - list of communities
  new-c - new community to move i to"
  [i cs old-c new-c]
  (add-node new-c i
    (remove-node old-c i cs)))


(defn max-dQ-rc
  "Returns the community resulting in the maximum gain in modularity
  g - graph
  cs - map of communities to contained nodes
  nodes - map of nodes to containing communities
  i - node"
  [g cs i]
  (let [nodes (maplistreverse cs)
        c-cur (get nodes i) ;current community containing i
        c-cur-nodes (get cs c-cur)
        n-conn (connected-nodes (list i) g)
        c-candidates (into #{} (vals (select-keys nodes n-conn)))
        dQ-remove (if (> (count (get cs c-cur)) 1)   ;dQ of removing i from current community
                    (dQ-of-i-c g i c-cur-nodes)
                    0)]
    (let [dQ-vals (pmap #(dQ-of-i-c g i (get cs %)) c-candidates)
          dQs (zipmap c-candidates dQ-vals)
          dQ-max (apply max-key val dQs)]
      (if (> (- (val dQ-max) dQ-remove) 0)
        (update-comms i cs c-cur (key dQ-max))
        cs))))

(defn modularize
  "First pass"
  [g cs]
  (reduce (partial max-dQ-rc g) cs (:nodeset g)))

(defn new-edge
  "Create edge vector for n1 and connection n2 is community of"
  [g cs node-map n1 n2]
  (let [n2-c (get node-map n2) ;community n2 belongs to
        n1-nodes (get cs n1)
        n2-nodes (get cs n2-c)]
    (vector n1 n2-c (sum-connections-between g n1-nodes n2-nodes))))

(defn get-comm-node-sets
  "All sets of connecting nodes for community c"
  [g cs node-map c]
  (let [community-nodes (get cs c)
        connected-communities (into #{} (map (partial get node-map) (connected-nodes community-nodes g)))]
    (remove #(not= 2 (count %)) ;remove
      (into #{} (map (partial sorted-set c) connected-communities)))))

(defn new-weighted-edge-vectors
  [g cs node-map connecting-nodes]
  (map #(vector (first %) (last %) (sum-connections-between g (get cs (first %)) (get cs (second %)))) connecting-nodes))

(defn new-network
  "Returns a new graph using the communities cs as the nodes"
  [g cs]
  (let [node-map (maplistreverse cs)
        g' (loom.graph/weighted-graph)
        connecting-nodes (into #{} (mapcat (partial get-comm-node-sets g cs node-map) (keys cs)))
        new-edges (new-weighted-edge-vectors g cs node-map connecting-nodes)]
    (reduce loom.graph/add-edges g' new-edges)))

(defn iterate-louvain
  [g]
  (let [g' (->> (init-community g)
                (modularize g)
                (new-network g))]
       g'))










