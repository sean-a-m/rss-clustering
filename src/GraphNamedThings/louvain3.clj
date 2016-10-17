(ns GraphNamedThings.louvain3
  (:require [loom.graph]
            [loom.alg]
            [clojure.set :as cset]))

(defn select-keys-trans
  [m ks]
  (apply merge (map #(hash-map % (get m %)) ks)))

(defn assoc-reverse
  "Map elements in components to point to their respective keys in order to create a map from each node to it's community key"
  [m entry]
  (reduce #(assoc %1 %2 (key entry)) m (:components (val entry))))

(defn create-node-index
  "Create node index from community structure"
  [comms]
  (reduce assoc-reverse {} comms))

(defn assoc-things3
  [m entry]
    (update-in m (list (val entry)) (fnil #(conj % (key entry)) '())))

(defn preexisting-comm-assignments
  [g c]
  "Given communities cs and the graph g, place any nodes in g not in cs in their own community, and leave the others in their preexisting communities.
  If a node belongs to multiple preexisting communities, leave it in the first one"
      (let [node-comm-map (zipmap (map :doc_id c) (map #(* 1 (:group_id %)) c))]
            (select-keys node-comm-map (into () (:nodeset g)))))

(defn init-comms-from-preexisting
  [g cs]
  (let [nodes (:nodeset g)
        preexisting-node-comm-map (preexisting-comm-assignments g cs)
        unassigned (cset/difference (into #{} nodes) (into #{} (keys preexisting-node-comm-map)))]
    (apply merge (reduce assoc-things3 {} preexisting-node-comm-map) (zipmap unassigned (map list unassigned))))) ;second element should always be a list

(defn remove-internal-edges
  "Remove the edges in es that connect to nodes in c, returning the others"
  [c es]
  (remove #(contains? (into #{} c) (key %)) es))

(defn outside-connections-sum
  [c g]
  (->> c
      (select-keys (:adj g))  ;select graph nodes from adjacency map that are in the list of community components c
      (vals)  ;get the lists of connected nodes
       (mapcat (partial remove-internal-edges c))
       (vals) ;get the list of weights
       (reduce +)))

(defn get-connected-comms
  [i g node-index]
  (->> i
       ;(:components (val (get cs comm-key)))
       (get (:adj g))  ;get the edges connecting to i
       (keys)  ;get the list of nodes connected to i
       (select-keys-trans node-index) ;get the communities the connecting nodes are part of
       (vals)
       (distinct)))
    ;   (mapcat (partial remove-internal-edges components))
    ;   (keys) ;get the list of outside connecting nodes
    ;   (select-keys-trans node-index)
    ;   (vals)
    ;   (distinct))))

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

(defn dQ
  [g m ki-i i c]
  (let [c-edge-weights (:c-tot c)
        connections-to (ki-in g (:components c) i)]
    (- connections-to
       (/ (* ki-i c-edge-weights)
          (* 2 m)))))

(defn dQ2
  [g m ki-i i c]
  (let [c-edge-weights (:c-tot c)
        connections-to (ki-in g (:components c) i)]
    (- (/ connections-to m)
       (/ (* ki-i c-edge-weights 2)
          (* m m)))))

(defn dQ3
  [g m ki-i i c]
  (let [c-edge-weights (:c-tot c)
        connections-to (ki-in g (:components c) i)]
    (- (/ connections-to m)
       (/ (* c-edge-weights ki-i)
          (* m m)))))


(defn max-dQ
  [g m cs-node-vector i]
  (let [ki-i (ki g i)
        [cs node-index] cs-node-vector
        cur-comm (get node-index i)
        ;neighboring-nodes (keys (get (:adj g) i))
        ;candidate-comms (distinct (map (partial get node-index) neighboring-nodes))
        candidate-comms (cons cur-comm (get-connected-comms i g node-index))
        c-candidate-c (select-keys-trans cs candidate-comms)
        dQs (if (empty? candidate-comms)
              {0 0}
              ;(apply merge (map #(hash-map (key %) (dQ g m ki-i i (val %))) c-candidate-c)))
              ;(zipmap (keys c-candidate-c) (map #(- 1 (dQ g m ki-i i %)) (vals c-candidate-c))))
              (zipmap (keys c-candidate-c) (map #(dQ3 g m ki-i i %) (vals c-candidate-c))))
        dQ-max (apply max-key val dQs)]
    (println "Moving " i "from " cur-comm "to " (key dQ-max) "with dQ of " (float (val dQ-max)))
    (if (and (> (val dQ-max) 0)
             (not= (key dQ-max) cur-comm))
      [(community-recalc g cs i cur-comm (key dQ-max)) (assoc! node-index i (key dQ-max))]
      [cs node-index])))


(defn max-graph-modularity
  [g m cs-node-vector]
  (let [[cs node-index] cs-node-vector
        csi (transient cs)
        node-index-i (transient node-index)]
    (loop [cs-node-vector [csi node-index-i] nodes (keys node-index)]
      (if (empty? nodes)
        (vector (persistent! (first cs-node-vector)) (persistent! (second cs-node-vector)))
        (recur (max-dQ g m cs-node-vector (first nodes)) (rest nodes))))))


;TODO: verify that maximum iteration limit is actually neccessary
(defn iterate-louvain-modularity
  "Iterate until community vector no longer changes or max-iteration limit is reached"
  [g pre-cs]
  (let [;cs (community-precalc (init-community g) g)
        cs (community-precalc (init-comms-from-preexisting g pre-cs) g)
        node-index (create-node-index cs)
        m (total-link-weight g)
        f-max-modularity (partial max-graph-modularity g m)
        cs-node-vector [cs node-index]
        limit 4]
    (loop [csnodevector cs-node-vector iterations 0]
      (let [new-vector (f-max-modularity csnodevector)]
        (if (and (not= (second new-vector) (first csnodevector))
                 (> limit iterations))
          (recur new-vector (inc iterations))
          new-vector)))))

