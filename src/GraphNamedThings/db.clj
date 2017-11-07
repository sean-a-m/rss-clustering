(ns GraphNamedThings.db
  (:require [hugsql.core :as hugsql]))

(hugsql/def-db-fns "GraphNamedThings/sql.sql")
(hugsql/def-sqlvec-fns "GraphNamedThings/sql.sql")


