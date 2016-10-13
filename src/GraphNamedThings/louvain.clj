(ns GraphNamedThings.louvain
  (:require [loom.graph]
            [loom.alg]
            [clojure.set :as cset]))

(defn init-community
  "Returns the set of communities where each community contains one graph node"
  [g]
  (apply merge
         (map #(hash-map % (list %)) (:nodeset g))))

(defn- maplistreverse
  "Reverse a map.  Not good if k repeats"
  [m]
  (apply merge
         (map (fn [k] (zipmap (get m k) (repeat k))) (keys m))))

(defn- assoc-things
  [m existing]
  (assoc m (key existing) (val existing)))

(defn- preexisting-comm-assignments 
  [g & cs]
  "Given communities cs and the graph g, place any nodes in g not in cs in their own community, and leave   the others in their preexisting communities.  
  If a node belongs to multiple preexisting communities, leave it in the first one"
    (loop [comms cs cs' {}]
      (let [comm-index (maplistreverse (first comms))
            existing-nodes (select-keys comm-index (:nodeset g))]
            (reduce assoc-things cs' existing-nodes))
      (if (seq? (rest cs))
        (recur (rest cs) cs')
        cs')))

(defn init-comms-from-preexisting
  [g & cs]
  (let [nodes (:nodeset g)
        preexisting (preexisting-comm-assignments g cs)
        unassigned (cset/difference (into #{} nodes) (into #{} (keys preexisting)))]
    (merge preexisting (zipmap unassigned unassigned))))

(defn- maplistreverse
  "Reverse a map.  Not good if k repeats"
  [m]
  (apply merge
         (map (fn [k] (zipmap (get m k) (repeat k))) (keys m))))

(defn outside-connections-sum
  [c g]
  "Sum of the edges connecting from nodes outside c to nodes inside c"
  (reduce +
          (map second (mapcat (fn [cn]
                                (remove #(some (into #{} c) %) (get (:adj g) cn))) c))))
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

(defn total-link-weight
  "The sum of all weights in an undirected weighted graph"
  [g]
  (let [get-in-adj (partial get-in (:adj g))]
    (reduce +
            (map get-in-adj
                 (loom.alg/distinct-edges g)))))

(defn comm-calc
  [c g]
  (hash-map :components c :c-tot (outside-connections-sum c g)))

(defn community-precalc
  [c-init-list g]
  (apply merge
         (pmap #(hash-map (key %) (comm-calc (val %) g)) c-init-list)))

(defn community-recalc
  [g cs i c1 c2]
  (let [c1-components (get (get cs c1) :components)
        c1-components-new (remove #(= i %) c1-components)
        c2-components (get (get cs c2) :components)
        c2-components-new (cons i c2-components)
        cs' (if (> 2 (count c1-components))
              (dissoc! cs c1)
              (assoc! cs c1 (comm-calc c1-components-new g)))]
    (assoc! cs' c2 (comm-calc (distinct c2-components-new) g))))

(defn select-keys-trans
  [m ks]
  (apply merge (map #(hash-map % (get m %)) ks)))

(defn dQ
  [g m ki-i i c]
  (let [c-edge-weights (:c-tot c)
        connections-to (ki-in g (:components c) i)]
    (- connections-to
       (/ (* ki-i c-edge-weights)
          (* 2 m)))))

(defn max-dQ
  [g m cs-node-vector i]
  (let [ki-i (ki g i)
        [cs node-index] cs-node-vector
        cur-node (get node-index i)
        neighboring-nodes (keys (get (:adj g) i))
        candidate-comms (distinct (map (partial get node-index) neighboring-nodes))
        c-candidate-c (select-keys-trans cs candidate-comms)
        dQs (apply merge (map #(hash-map (key %) (dQ g m ki-i i (val %))) c-candidate-c))
        dQ-max (apply max-key val dQs)]
    (if (> (val dQ-max) 0)
      [(community-recalc g cs i cur-node (key dQ-max)) (assoc! node-index i (key dQ-max))]
      [cs node-index])))


(defn max-graph-modularity
  [g m cs-node-vector]
  (let [[cs node-index] cs-node-vector
        csi (transient cs)
        node-index-i (transient node-index)]
    (loop [cs-node-vector [csi node-index-i] nodes (keys cs)]
      (if (empty? nodes)
        (vector (persistent! (first cs-node-vector)) (persistent! (second cs-node-vector)))
        (recur (max-dQ g m cs-node-vector (first nodes)) (rest nodes))))))

(let [pre-cs '()]
;TODO: verify that maximum iteration limit is actually neccessary
(defn iterate-louvain-modularity
  "Iterate until community vector no longer changes or max-iteration limit is reached"
  [g & pre-cs]
  (let [;cs (community-precalc (init-community g) g)
        cs (community-precalc (preexisting-comm-assignments g pre-cs) g)
        node-index (maplistreverse (init-community g))
        m (total-link-weight g)
        f-max-modularity (partial max-graph-modularity g m)
        cs-node-vector [cs node-index]
        limit 10]
    (loop [csnodevector cs-node-vector iterations 0]
      (let [new-vector (f-max-modularity csnodevector)]
        (if (and (not= (first new-vector) (first csnodevector))
                 (< limit iterations))
          (recur new-vector (inc iterations))
          new-vector))))))

