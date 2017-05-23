(ns GraphNamedThings.schema
  (:require [schema.core :as s]
            [schema.coerce :as coerce]))

(def clusters
  {(s/optional-key :page) s/Int
   (s/optional-key :perpage) s/Int})

(def cluster-coercer
  (coerce/coercer clusters coerce/string-coercion-matcher))