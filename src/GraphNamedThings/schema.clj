(ns GraphNamedThings.schema
  (:require [schema.core :as s]
            [schema.coerce :as coerce]))

(def clusters
  {(s/optional-key :page) s/Int
   (s/optional-key :perpage) s/Int})

(def cluster-coercer
  (coerce/coercer clusters coerce/string-coercion-matcher))

(def related
  (let [related-schema
        {(s/required-key :id) s/Uuid
         (s/optional-key :page) s/Int
         (s/optional-key :perpage) s/Int}]
    (coerce/coercer related-schema coerce/string-coercion-matcher)))
