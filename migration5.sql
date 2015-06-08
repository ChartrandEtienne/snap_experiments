ALTER TABLE login ADD COLUMN apikey character varying(100) NOT NULL DEFAULT md5(random()::text);

