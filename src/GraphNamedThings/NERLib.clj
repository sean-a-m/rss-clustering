

(ns GraphNamedThings.NERLib
  (use GraphNamedThings.util)
  (:import edu.stanford.nlp.simple.Document)
  (:import edu.stanford.nlp.simple.Sentence)
  (:import edu.stanford.nlp.simple.SentenceAlgorithms)
  (:import edu.stanford.nlp.ie.machinereading.structure.Span))


;Sentence, POS tag -> list of words
(defn sentence-pos [s pos]
  (flatten
    (map rest
      (get
        (group-by first
          (map vector (.posTags s) (.words s)))
        pos))))


;Sentence -> NER list
(defn sentence-ner [s]
  (let [not-ners "O"
        words (.words s)
        tags (.nerTags s)]
        (partition-by #(= not-ners %)
          tags)))

;Sentence -> NER list, alternate implementation
(defn sentence-ner2 [s]
  (let [not-ners "O"
        words (.words s)
        tags (.nerTags s)]
    (map-indexed
      (fn [idx w]
        (nth w 0))
      words)))


;Document, POS tag -> list of words corresponding to POS tag supplied
(defn doc-pos [d pos]
  (flatten
    (map
      #(sentence-pos % pos)
         (seq (.sentences d)))))

;Document -> NER list
(defn doc-ner [d]
    (mapcat
      #(sentence-ner %)
      (.sentences d)))

;NER Group list, start index ->
(defn ner-span-pair [l i]
  (list i (+ (dec i) (count l))))

;NER List -> list of spans
(defn list-ner-spans [l i]
  (cond
    (empty? l) ()
    :else (cons
            (ner-span-pair (first l) i)
            (list-ner-spans
              (rest l)
              (+ i
                 (count
                   (first l)))))))  ;TODO: Requiring the caller to pass the number "1" as a function argument is silly

;list of spans, NER list, tag to remove -> filtered list of spans
(defn list-ner-spans-filtered [ls ln tag]
  (let [spans (map #(first %) ls)
        ners (map #(first %) ln)]
    (keep-indexed #(if (in? (nth ners %1) tag) %2) ls)))


;List of span integer pairs -> List of span objects
(defn to-span [l]
  (map #(Span.
          (dec (first %))
          (second %))
       l))  ;the first number in the pair is 0 indexed, and the second number is exclusive, so decrementing the first number solves both of those problems

;Document object -> coreference map object (type Map<Integer,CorefChain>) converted to list of object CorefChain
(defn coref-map [doc]
  (vals
    (into
      {} (java.util.HashMap.
           (.coref doc)))))

;List of lists of coref mentions
(defn coref-mentions [doc]
  (map #(.getMentionsInTextualOrder %)
       (coref-map doc)))

(defn coref-text [l]
  (map #(.mentionSpan %) l))

;Mention object, Document object -> Word offset (in order to convert span number to be based on total number of words instead of the offset from the start of a given sentence)
(defn mention-offset [m doc]
  (let [sent-num (.sentNum m)
        pred-sentences (take (dec sent-num) (.sentences doc))]
    (reduce +
            (map #(.length %)
                 pred-sentences))))


(defn coref-spans [l doc]
  (let [sent-num #(.sentNum %)]
    (map #(list
            (+
              (.startIndex %)
              (mention-offset % doc))
            (+
              (.endIndex %)
              (mention-offset % doc)))
         l)))

(defn coref-spans-all [doc]
  (map #(coref-spans % doc)
       (coref-mentions doc)))

(defn coref-text-all [doc]
  (map #(coref-text %)
       (coref-mentions doc)))



