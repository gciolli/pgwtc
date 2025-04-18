EXTENSION = pgwtc ly2pg

DATA = pgwtc--0.1dev.sql pgwtc-notes.csv pgwtc-metadata.csv	\
pgwtc-clavis-metadata.csv ly2pg--0.1dev.sql ly2pg-tokens.txt	\
ly2pg-wtc-voces.sh ly2pg-wtc-load.sql

DOCS = pgwtc-README.md ly2pg-README.md

PG_CONFIG = pg_config
PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
