CREATE TABLE visit (
	id SERIAL PRIMARY KEY,
	url TEXT,
	datetime TIMESTAMP DEFAULT NOW(),
	usr_id integer REFERENCES login (id)
);

