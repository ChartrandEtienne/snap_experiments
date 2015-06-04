CREATE TABLE comic (
	id SERIAL PRIMARY KEY,
	url_prefix TEXT
);

CREATE TABLE follows (
	id SERIAL PRIMARY KEY,
	usr_id integer REFERENCES login (id),
	comic_id integer REFERENCES comic (id)
);
