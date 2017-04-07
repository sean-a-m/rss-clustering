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
(def selected-feed-ids  [3 4 7 9 13 14 15 16 17 18 97 98 99 100 104 105 106])

;how long to wait between recalculating results in milliseconds
(def update-delay 360000)

;how many threads corenlp library can use when parsing documents
(def corenlp-threads 2)

;how many documents corenlp library should process at once
(def corenlp-batch-size 1)

;iteration limit for clustering
(def iteration-limit 15)