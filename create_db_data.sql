CREATE TABLE chat_data (
    chat_id BIGINT  PRIMARY KEY
                    UNIQUE,
    name    TEXT,
    type_teren     TEXT,
	date_tren   INTEGER,
	treiner TEXT,
	time_tren TEXT,
	sheet_id TEXT,
	tg_username TEXT
);
