PGWTC_VERSION=0.1

EXTENSION = pgwtc ly2pg

DATA = pgwtc--$(PGWTC_VERSION).sql pgwtc-notes.csv pgwtc-metadata.csv	\
ly2pg-claves.csv ly2pg--$(PGWTC_VERSION).sql ly2pg-tokens.txt		\
ly2pg-wtc-voces.sh ly2pg-wtc-load.sql

DOCS = pgwtc-README.md ly2pg-README.md

PG_CONFIG = pg_config
PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
