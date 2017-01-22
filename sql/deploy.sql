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

CREATE TABLE processlog(
	id BIGINT PRIMARY KEY NOT NULL,
	success BOOLEAN NOT NULL,
	ver SMALLINT NOT NULL);

CREATE INDEX tag_idx ON namedentities (lower(tag)); 