;Data processing functions specific to a type of database or document source

(ns GraphNamedThings.dbio
  (:require [clojure-csv.core :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clj-time.core :as t]
            [clj-time.coerce :as coerce]
            [korma.db :refer :all]
            [clojure.data.json :as json]
            [taoensso.nippy :as nippy]
            [korma.core :refer :all]
            [clj-uuid :as uuid]
            [GraphNamedThings.config :as config])
  (:import org.jsoup.Jsoup))

;defines one group of related document, where group-id can be an arbitrary id and doc-ids is a collection of document ids
(defrecord doccluster [group-id doc-ids])


(def ttrss-db
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     config/ttrss-db-path
   })

(def frss-db
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     config/frss-db-path
   })

(def psqldb
  {:classname   "org.postgresql.Driver"
   :subprotocol "postgresql"
   :subname     config/output-db-path
   :user        config/psql-user
   :password    config/psql-pass
   :stringtype "unspecified"
   })

(defdb mydb psqldb)

;(declare namedentities strings processlog)

(defentity rss_entries
           (database psqldb)
           (entity-fields :id :title :link :date_entered :content :feed_id))

(defentity entry
           (database psqldb)
           (entity-fields :id :title :link :date :content :id_feed))

(defentity entry-ids
           (database psqldb)
           (entity-fields :id)
           (table :entry))

(defentity processed
           (database psqldb)
           (entity-fields :id :startdate :enddate))

(defentity processlog
           (database mydb)
           (entity-fields :id :success :ver))

(defentity processlog-ids
           (database mydb)
           (table :processlog)
           (entity-fields :id))

(defentity namedentities
           (database mydb)
 ;          (has-many strings)
           (entity-fields :id :tag :docid))

(defentity strings
           (database mydb)
           (belongs-to namedentities)
           (entity-fields :id :entstring :count))

(defentity docrelations
           (database psqldb)
           (entity-fields :processed_id :group_id :doc_id))

(defentity entitytest (database psqldb) (entity-fields :k :v))

(defentity entitystrings (database psqldb) (entity-fields :id :val))

(defentity entities (database psqldb) (entity-fields :id :val))



(defn parse-html-fragment
  "Assumes the text is always part of the document body"
  [html-fragment]
  (let [jsoup-doc  (. Jsoup parseBodyFragment html-fragment)]
    (.text
      (.body jsoup-doc))))

(defn select-id-list-old
  "Select processed documents by ID"
  [id-list]
  (select entitytest
          (where {:k [in id-list]})))

(defn select-id-list
  "Select processed documents by ID"
  [id-list]
  (select entities
          (where {:id [in id-list]})))

(defn select-entities-by-doc-id
  [id-list]
  (select entitystrings
          (where {(raw "val->>'doc-id'") [in id-list]})))

(defn select-by-strings
  [string-list]
  (let [quoted-list (map #(clojure.string/join (list "'" % "'")) string-list)
        array-str (clojure.string/join "," quoted-list)]
    (exec-raw psqldb [(clojure.string/join (list "SELECT * FROM entitystrings WHERE (val->'strings')::jsonb #&# array[" array-str "]"))] :results)))

(defn docs-by-id
  "Select input documents by ID"
  [id-list]
  (select entry
          (where {:id [in id-list]
                  :id_feed [in config/selected-feed-ids]})))

(defn select-newest-unprocessed-newest [batch-size]
  (select entry-ids
      (where (and (not (in :id (subselect processlog-ids)))
              (in :id_feed config/selected-feed-ids)))
                 ; {:id [in (list 3 4 7)]}))
          (limit batch-size)
          (order :id :DESC)))

(defn select-newest-unprocessed! [batch-size]
  "Select the most recent documents that haven't been processed to related entities yet.  "
  (exec-raw psqldb ["SELECT id, title, link, date, content, id_feed FROM entry WHERE id NOT IN (SELECT k FROM entities) AND id_feed IN (3, 4, 7, 9, 13, 14, 15, 16, 17, 18, 72, 79, 86, 97, 98, 99, 100) ORDER BY date DESC LIMIT ?" [batch-size]] :results))

(defn select-newest-unprocessed!-old [batch-size]
  "Select the most recent documents that haven't been processed to related entities yet.  "
  (exec-raw psqldb ["SELECT id, title, link, date, content, id_feed FROM entry WHERE id NOT IN (SELECT k FROM entitytest) AND id_feed IN (3, 4, 7, 9, 13, 14, 15, 16, 17, 18, 72, 79, 86, 97, 98, 99, 100) ORDER BY date DESC LIMIT ?" [batch-size]] :results))


(defn select-newest-unprocessed!2 [batch-size]
  (select entry
          (where (and (not {:id [in (map :k (select entitytest))]})
                      {:id [in config/selected-feed-ids]}))))

(defn docs-from-time-range-raw
  [start-time end-time]
  (exec-raw frss-db ["SELECT * FROM entry WHERE date BETWEEN ? AND ?" [start-time end-time]] :results))

(defn docs-from-time-range
  "Time is epoch in  frss table"
  [start-time end-time]
  (select entry
          (where {:date [between [start-time end-time]]
                  :id_feed [in config/selected-feed-ids]})))

(defn processed-docs-from-time-range
  "Time is epoch in  frss table"
  [start-time end-time]
  (select entry
          (where {:date [between [start-time end-time]]
                  :id_feed [in config/selected-feed-ids]
                  :id [in (subselect processlog-ids)]})))


(defn log-result
  [doc-id success? version]
  (insert processlog
          (values [{:id doc-id :success success? :ver version}])))

(defn doc-content-with-title
  "Returns title + content (parsed from html) for a given document entry as returned by docs-by-id"
  [doc-entry]
  (str/join ". " (list
                         (:title doc-entry)
                         (parse-html-fragment (str/join (list " " (:content doc-entry))))))) ;adding a space is a hack to stop jsoup from crashing and should be handle smarter somehow

(defn doc-content
  "Returns title + content (parsed from html) for a given document entry as returned by docs-by-id"
  [doc-entry]
    (parse-html-fragment (:content doc-entry)))

(defn doc-content-by-id
  "List of document ID's (contained in database) -> list of document contents "
  [id-list]
  (into {} (map #(vector (:id %) (doc-content %)) (docs-by-id id-list))))

(defn select-cluster-candidates
  "Return all processed table entries that have a time interval between start date and end date"
  [start-date end-date]
  (let [sql-start (coerce/to-sql-time start-date)
        sql-end (coerce/to-sql-time end-date)]
    (select processed
            (where (and (>= sql-start :startdate)
                        (<= sql-end :enddate))))))

(defn move-serialized-to-json! []
  (let [selectedentities (select entitytest)]
    (doall
    (map #(insert entities
                  (values [{:id (:k %) :val (json/write-str (nippy/thaw (:v %)))}])) selectedentities))))

(defn select-overlapping-clusters
  "Select all clusters that overlap with start-end interval"
  [start-date end-date]
  (let [sql-start (coerce/to-sql-time start-date)
        sql-end (coerce/to-sql-time end-date)]
    (select processed
            (where (or {:enddate [>= sql-start]
                        :startdate [<= sql-end]})))))

(defn select-best-cluster
  "Given start and end timestamps, select the best document group.  Right now the best one is whichever is over the smallest invterval"
  [start-date end-date]
  (let [candidates (select-cluster-candidates start-date end-date)]
    (:id (first
          (sort-by #(t/in-seconds  ;convert to seconds because clj-time/interval is not comparable
                       (t/interval (coerce/from-sql-time (:startdate %)) (coerce/from-sql-time (:enddate %))))
                   candidates)))))

(defn- assoc-things
  [m entry]
  (update-in m (list (:group_id entry)) (fnil #(conj % (:doc_id entry)) '())))  ;I'm using update-in so I don't have to add a prefix to all the korma/updates.....

(defn select-best-cluster-set
  "Returns a list of existing community id to document id mappings"
  [start-date end-date]
  (let [candidates (select-overlapping-clusters start-date end-date)
        processed-ids (map :id candidates)]
    (let [results (select docrelations (where {:processed_id [in processed-ids]}))]
      ;(reduce assoc-things {} results))))
      (map #(select-keys % '(:group_id :doc_id)) results))))



(defn write-new-cluster-entry
  "Write entry for this document cluster into the index, with a starting time stamp and postgres interval, and return the id"
  [start-date end-date]
  (let [start-entry (coerce/to-sql-time start-date)
        end-entry (coerce/to-sql-time end-date)]
    (insert processed
            (values [{:startdate start-entry :enddate end-entry}]))))

(defn create-entries-from-cluster
  "Write a single group of document ids to the database"
  [id cluster]
  (map #(hash-map :processed_id id :group_id (:group-id cluster) :doc_id %) (:doc-ids cluster)))


(defn write-clusters
  [clusters start-date end-date]
  "Write the resulting set of clusters to the database"
  (let [id (:id (write-new-cluster-entry start-date end-date))

        cluster-entries (mapcat (partial create-entries-from-cluster id) clusters)]
      (insert docrelations (values (doall cluster-entries)))))

(defn group-to-cluster-entry
  [group]
  (->doccluster
    (key group)
    (map :doc_id (val group))))

(defn read-doc-cluster
  "Read db entries into doc-cluster record"
  [id]
  (let [doc-relations (select docrelations
                              (where (= :processed_id id)))
        doc-relations-grouped (group-by :group_id doc-relations)]
    (map group-to-cluster-entry doc-relations-grouped)))


(defn select-entities-by-docid [docids]
  (select namedentities
          (fields :strings.entstring)
          (where {:docid [in docids]})
          (join strings (= :strings.id :id))))



(defn write-entities [entity-records string-lists]
  (transaction
    (if (seq entity-records)
    (insert namedentities
           (values entity-records)))
    (if (seq string-lists)
    (insert strings
            (values string-lists)))))

