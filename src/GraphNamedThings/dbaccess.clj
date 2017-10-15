(ns GraphNamedThings.dbaccess
  (:require [korma.db :refer :all]
            [korma.core :refer :all]
            [clj-postgresql.core :as pg]
            [clojure.java.jdbc :as jdbc]
            [GraphNamedThings.config :as config]
            [medley.core :as me])
  (:import org.jsoup.Jsoup))



(def psqldb (pg/pool :host config/dbhost :user config/psql-user :dbname config/dbname :password config/psql-pass))

(defentity entry
           (database psqldb)
           (entity-fields :id :title :link :date :content :id_feed :accessed :scrape :scrape_success))

(defentity entry-ids
           (database psqldb)
           (entity-fields :id)
           (table :entry))

(defentity namedentities
           (database psqldb)
           (entity-fields :id :tag :docid))

(defentity strings
           (database psqldb)
           (belongs-to namedentities)
           (entity-fields :id :entstring :count))

(defn parse-html-fragment
  "Assumes the text is always part of the document body"
  [html-fragment]
  (let [jsoup-doc  (. Jsoup parseBodyFragment html-fragment)]
    (.text
      (.body jsoup-doc))))

(defn docs-by-id
  "Select input documents by ID"
  [id-list]
  (select entry
          (where {:id [in id-list]
                  :id_feed [in config/selected-feed-ids]})))

(defn select-newest-unprocessed [batch-size]
  (select entry-ids
      (where {:text_processed false
              :accessed true
              :id_feed [in config/selected-feed-ids]})
          (limit batch-size)
          (order :id :DESC)))

(defn processed-docs-from-time-range
  "Time is epoch in  frss table"
  [start-time end-time]
    (select entry
            (where {:date [between [start-time end-time]]
                    :id_feed [in config/selected-feed-ids]
                    :text_processed true
                    :process_success true})))

(defn log-result
  [doc-id success?]
  (update entry
          (set-fields {:process_success success?
                       :text_processed true})
          (where (= :id doc-id))))

(defn doc-content
  "Returns content (parsed from html) for a given document entry as returned by docs-by-id"
  [doc-entry]
  (if (= (:scrape_success doc-entry) true)
    (:scrape doc-entry)
    (parse-html-fragment (:content doc-entry))))

(defn doc-content-by-id
  "List of document ID's (contained in database) -> list of document contents "
  [id-list]
  (into {} (map #(vector (:id %) (doc-content %)) (docs-by-id id-list))))

(defn get-entity-records [start end]
  ;FIXME: entry.content and entry.scrape are in this query to help filter documents by unique content, but this should be done before retreiving entity records
  "Return all named entities ocurring between start and end date"
  (exec-raw psqldb ["SELECT strings.entstring, strings.id, namedentities.docid, namedentities.tag, entry.content, entry.scrape
                      FROM strings, namedentities, entry
                        WHERE strings.id = namedentities.id
                          AND namedentities.docid = entry.id
                        AND namedentities.tag NOT IN ('DATE','NUMBER','ORDINAL','DURATION','TIME','PERCENT','MONEY','SET','NULL')
                        AND strings.entstring NOT IN ('guardian','reuters','associated press','ap','getty','getty images')
                        AND namedentities.docid IN (SELECT id
                                                      FROM entry
                                                        WHERE date BETWEEN ? AND ?)" [start end]] :results))

(defn write-entities [entity-records string-lists]
  (transaction
    (if (seq entity-records)
      (insert namedentities
             (values entity-records)))
    (if (seq string-lists)
      (insert strings
              (values string-lists)))))

(defn get-doc-summary [doc-ids]
  (let [prepared-stuff (clojure.string/join ", " (take (count doc-ids) (repeat "?::bigint")))
        query (str "WITH document_strings AS (
                      SELECT namedentities.docid, array_agg(strings.entstring)
                        FROM namedentities, strings
                          WHERE namedentities.id = strings.id AND namedentities.docid IN (" prepared-stuff ")
                          AND NOT namedentities.tag IN ('DATE', 'NUMBER', 'ORDINAL', 'DURATION', 'TIME', 'PERCENT', 'MONEY')
                          GROUP BY namedentities.docid)
                    SELECT entry.id, entry.title, entry.link, entry.date, entry.id_feed, source_feeds.id AS source_id, sources.source_name, document_strings.array_agg
                      FROM entry, document_strings, source_feeds, sources
                      WHERE document_strings.docid=entry.id
                        AND entry.id_feed = source_feeds.id_feed
                        AND source_feeds.id = sources.id")
        stmnt (apply vector query doc-ids)]
    (jdbc/query psqldb stmnt)))

(defn get-string-counts [strings]
  "Get the number of ocurrences for each string in the list strings over all documents in the database"
  (let [prepared-stuff (clojure.string/join ", " (take (count strings) (repeat "?::text")))
        query (str "SELECT entstring, count(*)
                      FROM strings WHERE entstring IN (" prepared-stuff ")
                      GROUP BY entstring")
        stmnt (apply vector query strings)]
    (jdbc/query psqldb stmnt)))

(defn related-article-ids [terms time-before limit offset]
  "TODO: Take into account term coreferences"
  (let [prepared-stuff (clojure.string/join ", " (take (count terms) (repeat "?::text")))
        query (str  "WITH sorted_docs AS (
                      SELECT namedentities.docid, COUNT(*) FROM namedentities, strings
                        WHERE strings.entstring IN (" prepared-stuff ")
                        AND strings.id = namedentities.id
                      GROUP BY namedentities.docid
                      ORDER BY count DESC)
                    SELECT sorted_docs.docid FROM sorted_docs, entry
                      WHERE sorted_docs.docid = entry.id
                      AND entry.date < ?::bigint
                    LIMIT ?::int OFFSET ?::int")
        stmnt (apply vector query (flatten (conj (list time-before limit offset) terms)))] ;FIXME: ugly
    (jdbc/query psqldb stmnt)))


