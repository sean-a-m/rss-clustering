;TODO: Functions required to aggregate coreferences

(ns GraphNamedThings.NERLib
  (:import edu.stanford.nlp.simple.Document)
  (:import edu.stanford.nlp.simple.Sentence)
  (:import edu.stanford.nlp.simple.SentenceAlgorithms)
  (:import edu.stanford.nlp.ie.machinereading.structure.Span))

(defn in?
  [elm coll]
  (some #(= elm %) coll))


;Sentence, POS tag -> list of words
(defn sentencePOS [s pos]
  (flatten
    (map rest
      (get
        (group-by first
          (map vector (.posTags s) (.words s))) pos))))


;Sentence -> NER list
(defn sentenceNER [s]
  (let [notNERs "O"
        words (.words s)
        tags (.nerTags s)]
        (partition-by #(= notNERs %)
          tags)))

;Sentence -> NER list, alternate implementation
(defn sentenceNER2 [s]
  (let [notNERs "O"
        words (.words s)
        tags (.nerTags s)]
    (map-indexed (fn [idx w] (nth w 0))
      words)))


;Document, POS tag -> list of words corresponding to POS tag supplied
(defn docPOS [d pos]
  (flatten
    (map #(sentencePOS % pos)
         (seq (.sentences d)))))

;Document -> NER list
(defn docNER [d]
    (mapcat #(sentenceNER %)
         (.sentences d)))

;NER Group list, start index ->
(defn NERSpanPair [l i]
  (list i (+ (dec i) (count l))))

;NER List -> list of spans
(defn ListNERSpans [l i]
  (cond
    (empty? l) ()
    :else (cons
            (NERSpanPair (first l) i) (ListNERSpans (rest l) (+ i (count (first l)))))))  ;TODO: Requiring the caller to pass the number "1" as a function argument is silly

;list of spans, NER list, tag to remove -> filtered list of spans
(defn ListNERSpansFiltered [ls ln tag]
  (let [spans (map #(first %) ls)
        NERs (map #(first %) ln)]
    (keep-indexed #(if (in? (nth NERs %1) tag) %2) ls)))


;List of span pairs -> List of spans
(defn NERSpans [l]
  (map #(Span. (dec (first %)) (second %)) l))  ;the first number in the pair is 0 indexed, and the second number is exclusive, so decrementing the first number solves both of those problems





