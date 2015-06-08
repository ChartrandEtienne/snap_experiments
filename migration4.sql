ALTER TABLE login ADD COLUMN password character varying(100) NOT NULL DEFAULT '';

ALTER TABLE login ALTER COLUMN password DROP DEFAULT;
