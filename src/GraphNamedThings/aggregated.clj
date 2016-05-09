(ns GraphNamedThings.aggregated)

;if range defined by pair r contained within any pair in list l then return that pair, otherwise, return empty list
;TODO: error if  r2>r1 is passed to function
(defn span-in-list [r l]
  (let [r1 (first r)
        r2 (second r)]
    (filter #(>= r1 (first %))
            (filter #(<= r2 (second %)) l))))

;return set of pairs l that contain items r
(defn spans-in-list [rs l]
  (mapcat #(span-in-list % l) rs))

;Returns a list containing coreferences grouped together in their own lists, and other
;TODO: Generalize this or do something that doesn't require manually adding another level of lists to the items that do not have other coreference items
(defn aggregated-entities [coref-list ent-list]
  (let [coref-set (into #{} coref-list)
        ent-set (into #{} ent-list)
        aggregated-corefs (map #(spans-in-list % ent-set) coref-set)]
    (concat
      aggregated-corefs
      (map #(list %)
        (clojure.set/difference
          ent-set
          (apply concat aggregated-corefs))))))


(def span-list '((25 28) (45 49) (54 55) (56 57) (58 60) (66 69) (73 75) (85 86)))

(def coref-list '(((25 28) (46 48) (85 92) (5 89) (67 68)) ((58 59))))


(clojure.set/intersection #{'(1 2) '(3 4)} #{'(3 4)})

(clojure.set/intersection

(set span-list)
(set coref-list))

(spans-in-list (first coref-list) span-list)

(aggregated-entities coref-list span-list)
;def la = in first set
      ;lb = in second set
      ;lc = list not in first or second set
      ;l = concat la lb lc
