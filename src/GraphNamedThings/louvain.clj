(ns GraphNamedThings.louvain
  (:require [loom.graph]
            [loom.alg]
            [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :as math]
            [GraphNamedThings.config :as config]))


(defn preexisting-comm-assignments
  [g c]
  "Given communities cs and the graph g, place any nodes in g not in cs in their own community, and leave the others in their preexisting communities.
  If a node belongs to multiple preexisting communities, leave it in the first one"
  (let [node-comm-map (zipmap (map :doc_id c) (map #(:group_id %) c))]
    (select-keys node-comm-map (into #{} (:nodeset g)))))

(defn init-node-index-from-preexisting
  "Create an map of nodes to communities, using the graph g and any preexisting node index cs
  cs will be an empty list if there is no preexisting index"
  [g cs]
  (let [nodes (:nodeset g)
        preexisting-node-comm-map (preexisting-comm-assignments g cs)
        init-node-set (zipmap nodes nodes)
        assoc-keyvalue (fn [m kv] (assoc m (first kv) (second kv)))]
    (reduce assoc-keyvalue init-node-set preexisting-node-comm-map)))

(defn remove-internal-edges
  "Remove the edges in es that connect to nodes in c, returning the others"
  [c es]
  (remove #(contains? (into #{} c) (key %)) es))

(defn assoc-reverse
  "Map elements in components to point to their respective keys in order to create a map from each node to it's community key"
  [m entry]
  (reduce #(assoc %1 %2 (key entry)) m (:components (val entry))))

(defn outside-connections-sum
  ^double [c g]
  (let [add-connections (fn [^doubles c] (reduce + c))]
    (->> c
         (select-keys (:adj g))  ;select graph nodes from adjacency map that are in the list of community components c
         (vals)  ;get the lists of connected nodes
         (mapcat (partial remove-internal-edges c))
         (vals) ;get the list of weights
         (vec)
         (apply vector-of :double)
         (add-connections))))

(defn get-edge-weight
  [g ev]
  (get-in (:adj g) ev))

(defn inside-connections-sum
  [g c]
  (let [edge-combos (combo/combinations c 2)
        xf (comp
             (map (partial get-edge-weight g))
             (remove nil?))]
    (transduce xf + edge-combos)))

(defn comm-modularity [g m c]
  (let [ein (inside-connections-sum g c)
        e m
        eout (outside-connections-sum c g)]
    (- (/ ein e)
       (math/expt (/ (+ (* 2 ein) eout) (* 2 e)) 2))))

(defn comm-modularities [g m cs]
  (zipmap
    (keys cs)
    (map (partial comm-modularity g m) (vals cs))))

(defn get-community-weight [g c i]
  ;TODO: change to sum inside this function if possible
  "Edge weight between node i and community c"
  (-> (:adj g)
      (get i)
      (select-keys c)
      (vals)))

(defn adjust-change-connection
  "Calculate the change in the internal edge weight of a community c in graph g when adding or removing the node i"
  [g c i]
  (reduce +
          (get-community-weight g c i)))

(defn get-connected-comms
  [i g node-index]
  (->> i
       (get (:adj g))  ;get the edges connecting to i
       (keys)  ;get the list of nodes connected to i
       (select-keys node-index) ;get the communities the connecting nodes are part of
       (vals)
       (distinct)))

(defn ki
  "Sum of weights of links to node i"
  ^double [g i]
  (reduce +
          (vals
            (get (:adj g) i))))

(defn ki-in
  "Sum of weights of links from node i to community c"
  [g c i]
  (reduce +
          (vals
            (select-keys (get (:adj g) i) c))))

(defn build-ki-table
  [g is]
  (zipmap is (map #(reduce + (vals (get (:adj g) %))) is)))

(defn total-link-weight
  "The sum of all weights in an undirected weighted graph"
  [g]
  (let [get-in-adj (partial get-in (:adj g))]
    (reduce +
            (map get-in-adj
                 (loom.alg/distinct-edges g)))))

(defn s-precalc
  [cs g]
  (zipmap (keys cs) (map (partial inside-connections-sum g) (vals cs))))

(defn update-add
  [m k v]
  (update m k (fnil #(conj % v) '())))

(defn community-recalc
  [g i c1 c2 state]
  (let [c1-adjust (adjust-change-connection g (get (:comms @state) c1) i)
        c2-adjust (adjust-change-connection g (get (:comms @state) c2) i)]
    (swap! state update-in [:comms c2] conj i)
    (swap! state update-in [:i-weights c2] + c2-adjust)
    (if (> 2 (count (get (:comms @state) c1)))
      (swap! state update :comms dissoc c1)
      (do
        (swap! state update-in [:comms c1] (fn [v m] (remove m v)) #{i})
        (swap! state update-in [:i-weights c1] - c1-adjust)))))

(defn dQ
  [g m ki-i i curr_com s-tot test_comm]
  (let [sigtot (if (= curr_com test_comm)
                 (- s-tot ki-i)
                 s-tot)]
    (- (ki-in g (val test_comm) i)
       (/ (* ki-i sigtot)
          m))))

(defn update-data-structures
  [g i cur-comm dQ-max state]
  (community-recalc g i cur-comm dQ-max state)
  (swap! state assoc-in[:nodes i] dQ-max))

(defn find-dQ-max
  [g m ki-i i cur-comm s-table comm-candidates]
  (loop [max-item cur-comm
         max-val 0
         comm-candidates comm-candidates]
    (if (or (empty? comm-candidates)
            (nil? comm-candidates))
      max-item
      (let [new-max-val (dQ g m ki-i i cur-comm (get s-table (key (first comm-candidates))) (first comm-candidates))]
        (if (< max-val new-max-val)
          (recur (key (first comm-candidates)) new-max-val (rest comm-candidates))
          (recur max-item max-val (rest comm-candidates)))))))

(defn max-dQ
  [g m ki-i state i]
  (let [;[cs node-index s-table] cs-node-vector
        cur-comm (get (:nodes @state) i)
        candidate-comms (cons cur-comm (get-connected-comms i g (:nodes @state)))
        c-candidate-c (select-keys (:comms @state) candidate-comms)]
    (let [dQ-max (find-dQ-max g m ki-i i cur-comm (:i-weights @state) c-candidate-c)]
      (if (not= dQ-max cur-comm)
        (do
          (swap! state assoc :modified? true)
          (update-data-structures g i cur-comm dQ-max state))))))

(defn max-graph-modularity
  [g m node-index ki-table cs-node-vector]
  (do
    (swap! cs-node-vector assoc :modified? false)
    (doseq [node (keys node-index)]
      (max-dQ g m (get ki-table node) cs-node-vector node))))

(defn update-in-keyvalue
  [m entry]
  (update-in m (list (val entry)) (fnil #(conj % (key entry)) '())))

(defn iterate-louvain-modularity
  "Iterate until community vector no longer changes or max-iteration limit is reached"
  [pre-cs g]
  (let [node-index (init-node-index-from-preexisting g pre-cs)
        cs (reduce update-in-keyvalue {} node-index)
        s-table (s-precalc cs g)
        m (total-link-weight g)
        ki-table (build-ki-table g (keys node-index))
        f-max-modularity (partial max-graph-modularity g m node-index ki-table)
        state (atom {:comms cs :nodes node-index :i-weights s-table :modified? true})
        limit config/iteration-limit]
    (loop [iterations 0]
      (if (and (:modified? @state)
               (> limit iterations))
        (do
          (println "Iterating graph...")
          (f-max-modularity state)
          (recur (inc iterations)))
        (-> @state
            (assoc :graph g)
            (assoc :qs (comm-modularities g m (:comms @state))))))))