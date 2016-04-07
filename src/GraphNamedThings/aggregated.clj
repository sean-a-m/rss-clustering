(ns GraphNamedThings.aggregated)

;if range defined by pair r contained within any pair in list l then return that pair, otherwise, return empty list
;TODO: do something to prevent problems if r where r2>r1 is passed to function
(defn range-in-list [r l]
  (let [r1 (first r)
        r2 (second r)]
    (filter #(>= r1 (first %))
            (filter #(<= r2 (second %)) l))))

;return set of pairs l that contain items r
(defn ranges-in-list [rs l]
  (mapcat #(range-in-list % l) rs))



(def span-list (list '(25 28) '(45 49) '(54 55) '(56 57) '(58 60) '(66 69) '(73 75) '(85 86)))

(def coref-list (list '(25 28) '(46 48) '(85 92) '(5 89) '(67 68)))


(clojure.set/intersection #{'(1 2) '(3 4)} #{'(3 4)})

(clojure.set/intersection

(set span-list)
(set coref-list))

(ranges-in-list coref-list span-list)

;def la = in first set
      ;lb = in second set
      ;lc = list not in first or second set
      ;l = concat la lb lc
