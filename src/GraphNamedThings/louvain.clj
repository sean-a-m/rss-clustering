(ns GraphNamedThings.louvain
  (:require [loom.graph]
            [loom.alg]
            [clojure.set :as cset]))

(set! *warn-on-reflection* true)

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

(defn assoc-things4
  [m kv]
  (assoc m (first kv) (second kv)))

(defn preexisting-comm-assignments
  [g c]
  "Given communities cs and the graph g, place any nodes in g not in cs in their own community, and leave the others in their preexisting communities.
  If a node belongs to multiple preexisting communities, leave it in the first one"
      (let [node-comm-map (zipmap (map :doc_id c) (map #(:group_id %) c))]
            (select-keys node-comm-map (into #{} (:nodeset g)))))

(defn init-comms-from-preexisting
  [g cs]
  (let [nodes (:nodeset g)
        preexisting-node-comm-map (preexisting-comm-assignments g cs)
        unassigned (cset/difference (into #{} nodes) (into #{} (keys preexisting-node-comm-map)))]
    (apply merge (reduce assoc-things3 {} preexisting-node-comm-map) (zipmap unassigned (map list unassigned))))) ;second element should always be a list

(defn init-comms-from-preexisting2
  [g cs]
  (let [nodes (:nodeset g)
        preexisting-node-comm-map (preexisting-comm-assignments g cs)
        to-update (select-keys preexisting-node-comm-map (cset/intersection (into #{} nodes) (into #{} (keys preexisting-node-comm-map))))
        init-node-set (zipmap nodes (map list nodes))]
   ; (print to-update)
    (reduce-kv assoc-things4 init-node-set to-update)))

  (defn init-node-index-from-preexisting
    [g cs]
    (let [nodes (:nodeset g)
          preexisting-node-comm-map (preexisting-comm-assignments g cs)
          init-node-set (zipmap nodes nodes)]
      (reduce assoc-things4 init-node-set preexisting-node-comm-map)))

(defn create-comms-from-node-index
  [node-index]
    (reduce assoc-things3 {} node-index))

(defn remove-internal-edges
  "Remove the edges in es that connect to nodes in c, returning the others"
  [c es]
  (remove #(contains? (into #{} c) (key %)) es))

(defn select-internal-edges
  "Remove the edges in es that connect to nodes in c, returning the others"
  [c es]
  (filter #(contains? (into #{} c) (key %)) es))

(defn add-connections [^doubles c]
  (reduce + c))

(defn outside-connections-sum
  ^double [c g]
  (->> c
       (select-keys (:adj g))  ;select graph nodes from adjacency map that are in the list of community components c
       (vals)  ;get the lists of connected nodes
       (mapcat (partial remove-internal-edges c))
       (vals) ;get the list of weights
       (vec)
       (apply vector-of :double)
       (add-connections)))

(defn inside-connections-sum
  [g c]
  (->> c
       (select-keys (:adj g))  ;select graph nodes from adjacency map that are in the list of community components c
       (vals)  ;get the lists of connected nodes
       (mapcat (partial select-internal-edges c))
       (vals) ;get the list of weights
       (add-connections)))

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

(defn ki-calc
  [^doubles n]
  (reduce + n))

(defn ki
  "Sum of weights of links to node i"
  ^double [g i]
    (ki-calc
            (vals
              (get (:adj g) i))))

(defn ki-in
  "Sum of weights of links from node i to community c"
  [g c i]
  (ki-calc
          (vals
            (select-keys (get (:adj g) i) c))))

(defn build-ki-table
  [g is]
  (zipmap is (map #(ki-calc (vals (get (:adj g) %))) is)))

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

(defn comm-calc2
  [c g]

  (hash-map :components c :c-tot (inside-connections-sum g c)))

(defn community-precalc
  [c-init-list g]
  (apply merge
         (map #(hash-map (key %) (comm-calc2 (val %) g)) c-init-list)))

(defn community-recalc
  [g cs i c1 c2]
  ;(println "changing " i "from " c1 "to " c2)
  (let [c1-components (get (get cs c1) :components)
        c1-components-new (remove #{i} c1-components)
        c2-components (get (get cs c2) :components)
        c2-components-new (cons i c2-components)]
        (if (> 2 (count c1-components))
              (dissoc! cs c1)
              (assoc! cs c1 (comm-calc c1-components-new g)))
     (assoc! cs c2 (comm-calc (distinct c2-components-new) g))))

(defn community-recalc2
  [g cs i c1 c2]

  ;(println "changing " i "from " c1 "to " c2)
  (let [c1-components (get (get cs c1) :components)
        c1-components-new (remove #{i} c1-components)
        c2-components (get (get cs c2) :components)
        c2-components-new (cons i c2-components)]
        (if (> 2 (count c1-components))
              (dissoc! cs c1)
              (assoc! cs c1 (comm-calc2 c1-components-new g)))
    (assoc! cs c2 (comm-calc2 c2-components-new g))))

(defn dQ
  [g m ki-i i c]
  (let [c-edge-weights (:c-tot c)
        connections-to (ki-in g (:components c) i)]
    (- connections-to
       (/ (* ki-i c-edge-weights)
          (* 2 m)))))

(defn dQ-alt
  [^double m ^double ki-i ^double connections-to ^double c-edge-weights]
  ;(println "Running dQ")
    (- (/ connections-to m)
  ;(- 1
       (/ (* ki-i c-edge-weights)
          (* m m))))

(defn dQ2
  [g m ki-i i c]
  ;(println "Running dQ")
  (let [^double c-edge-weights (:c-tot c)
        ^double connections-to (ki-in g (:components c) i)]
    (dQ-alt m ki-i connections-to c-edge-weights)))
  ;  (- (/ connections-to m)
  ;     (/ (* ki-i c-edge-weights)
  ;        (* m m)))))

(defn dQ3
  [g m ki-i i curr_com test_comm]
  (let [iconn (:c-tot test_comm)
        sigtot (if (= curr_com test_comm)
                 (- iconn ki-i)
                 iconn)]
    (- (ki-in g (:components test_comm) i)
       (/ (* ki-i sigtot)
          m))))


(defn max-dQ
  [g m ki-i cs-node-vector i]
  (let [;ki-i (ki g i)
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
             ; (zipmap (keys c-candidate-c) (map #(dQ2 g m ki-i i %) (vals c-candidate-c))))
              (zipmap (keys c-candidate-c) (map #(dQ3 g m ki-i i cur-comm %) (vals c-candidate-c))))
        dQ-max (apply max-key val dQs)]

    ;(println "Moving " i "from " cur-comm "to " (key dQ-max) "with dQ of " (float (val dQ-max)))
    (if (and (> (val dQ-max) 0)
             (not= (key dQ-max) cur-comm))
      [(community-recalc2 g cs i cur-comm (key dQ-max)) (assoc! node-index i (key dQ-max))]
      [cs node-index])))


(defn max-graph-modularity
  [g m node-index ki-table cs-node-vector]
    (loop [cs-node-vector cs-node-vector nodes (keys node-index)]
      (if (empty? nodes)
        cs-node-vector
        (recur (max-dQ g m (get ki-table (first nodes)) cs-node-vector (first nodes)) (rest nodes)))))


;TODO: verify that maximum iteration limit is actually neccessary
(defn iterate-louvain-modularity
  "Iterate until community vector no longer changes or max-iteration limit is reached"
  [pre-cs g]
  (let [;cs (community-precalc (init-community g) g)
        ;cs (community-precalc (init-comms-from-preexisting2 g pre-cs) g)
        ;node-index (create-node-index cs)
        node-index (init-node-index-from-preexisting g pre-cs)
        cs (community-precalc (reduce assoc-things3 {} node-index) g)
        m (total-link-weight g)
        ki-table (build-ki-table g (keys node-index))
        f-max-modularity (partial max-graph-modularity g m node-index ki-table)
        cs-node-vector [(transient cs) (transient node-index)]
        limit 8]
    (loop [csnodevector cs-node-vector iterations 0]
      (let [new-vector (f-max-modularity csnodevector)]
        (if (and (not= (second new-vector) (second csnodevector))
                 (> limit iterations))
          (recur new-vector (inc iterations))
          (vector (persistent! (first new-vector)) (persistent! (second new-vector))))))))
