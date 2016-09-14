;Anything specific to a particular server or database instance
(ns GraphNamedThings.config)


"Path to database"
(def ttrss-db-path "[path]")

(def frss-db-path "[path]")

(def output-db-path "[path]")

"pg authentication"
(def psql-user "[user]")
(def psql-pass "[pass]")

;Feed ids to use
(def selected-feed-ids-ttrss [30 52 53 54 56 61 62 65 66 67 93 94 102])

(def selected-feed-ids [3 4 7 9 10 11 12 13 14 15 16 17 18 97 98 99 100])

;version number used for determining when to overwrite database entries
(def version 0.01)

;size of document processing batches
(def batch-size 100)




