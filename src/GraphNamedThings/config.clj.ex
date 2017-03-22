;Anything specific to a particular server or database instance
(ns GraphNamedThings.config)


"Path to database"
(def output-db-path "//localhost:5432/db")
(def dbname "db")
(def dbhost "localhost:5432")

"pg authentication"
(def psql-user "user")
(def psql-pass "pass")

;Feed ids to use
(def selected-feed-ids-ttrss [30 52 53 54 56 61 62 65 66 67 93 94 102])

(def selected-feed-ids  [3 4 7 9 13 14 15 16 17 18 97 98 99 100 104 105 106])

;version number used for determining when to overwrite database entries
(def version 0.01)

;limit the number of sentences to process
(def max-sentences 10)


