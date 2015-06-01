CREATE TABLE login (
	id SERIAL PRIMARY KEY,
	login TEXT
);

CREATE TABLE post (
	id SERIAL PRIMARY KEY,
	url TEXT,
	title TEXT,
	usr_id integer REFERENCES usr (id)
);
