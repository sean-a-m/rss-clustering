-- :name docs-by-id
select * from entry
where id in (:v*:id-list)
and id_feed in (:v*:feed-ids)

-- :name select-newest-unprocessed
select id from entry
where text_processed = false
and accessed = true
and id_feed in (:v*:feed-ids)
order by id desc
limit :batch-size

-- :name processed-docs-from-time-range
select * from entry
where date between :start-time and :end-time
and id_feed in (:v*:feed-ids)
and text_processed = true
and process_success = true

-- :name log-result :! :n
update entry
set process_success = :success?, text_processed = true
where id = :doc-id

-- :name get-entity-records
select strings.entstring, strings.id, namedentities.docid, namedentities.tag, entry.content, entry.scrape
from strings, namedentities, entry
where strings.id = namedentities.id
and namedentities.docid = entry.id
and namedentities.tag not in ('DATE','NUMBER','ORDINAL','DURATION','TIME','PERCENT','MONEY','SET','NULL')
and namedentities.docid in (select id from entry
                                where date between :start and :end)

-- :name get-doc-summary
with document_strings as (
                      select namedentities.docid, array_agg(strings.entstring)
                        from namedentities, strings
                          where namedentities.id = strings.id AND namedentities.docid IN (:v*:doc-ids)
                          and not namedentities.tag in ('DATE', 'NUMBER', 'ORDINAL', 'DURATION', 'TIME', 'PERCENT', 'MONEY')
                          group by namedentities.docid)
                    select entry.id, entry.title, entry.link, entry.date, entry.id_feed, source_feeds.id AS source_id, sources.source_name, document_strings.array_agg
                      from entry, document_strings, source_feeds, sources
                      where document_strings.docid = entry.id
                        AND entry.id_feed = source_feeds.id_feed
                        AND source_feeds.id = sources.id

-- :name get-string-counts
select entstring, count(*)
from strings where entstring in (:v*:strings)
group by entstring

-- :name related-article-ids
with sorted_docs as (
                      select namedentities.docid, count(*) from namedentities, strings
                        where strings.entstring in (:v*:strings)
                        and strings.id = namedentities.id
                      group by namedentities.docid
                      order by count desc)
                    select sorted_docs.docid from sorted_docs, entry
                      where sorted_docs.docid = entry.id
                      and entry.date < :time-before::bigint
                    limit :limit::int offset :offset::int

-- :name write-entity-records
insert into namedentities (:id, tag, docid)
values :tuple*:entity-records

-- :name write-entity-record
insert into namedentities (id, tag, docid)
values (:id, :tag, :docid)

-- :name write-strings
insert into strings (id, entstring, count)
values :tuple*:string-lists

-- :name write-string
insert into strings (id, entstring, count)
values (:id, :entstring, :count)