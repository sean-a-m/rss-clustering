CREATE TABLE namedentities(
	id UUID PRIMARY KEY NOT NULL,
	tag TEXT NOT NULL,
	docid BIGINT NOT NULL);

CREATE TABLE strings(
	id UUID NOT NULL,
	entstring TEXT NOT NULL,
	count SMALLINT NOT NULL,
	PRIMARY KEY (id, entstring));

CREATE UNIQUE INDEX idxr ON strings (entstring, id);
CREATE INDEX string_idx ON strings (lower(entstring)); 

CREATE INDEX tag_idx ON namedentities (lower(tag)); 
CREATE INDEX docid_idx ON namedentities (docid);

CREATE TABLE entry
(
  id bigint NOT NULL,
  title character varying(255) NOT NULL,
  author character varying(255),
  content text,
  link character varying(1023) NOT NULL,
  date integer,
  id_feed smallint,
  tags character varying(1023),
  rowid bigint,
  accessed boolean NOT NULL DEFAULT false,
  scrape TEXT,
  text_processed BOOLEAN NOT NULL DEFAULT false,
  process_success BOOLEAN,
  CONSTRAINT entry_pkey PRIMARY KEY (id),
  CONSTRAINT aconstraint UNIQUE (rowid)
)

CREATE INDEX entry_date_idx ON entry (date);

ALTER TABLE strings ADD FOREIGN KEY (id) REFERENCES namedentities ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE namedentities ADD FOREIGN KEY (docid) REFERENCES entry (id) ON DELETE CASCADE ON UPDATE CASCADE;