EXTENSION = pgwtc
DATA = pgwtc--0.1dev.sql pgwtc-notes.csv pgwtc-metadata.csv pgwtc-clavis-metadata.csv

PG_CONFIG = pg_config
PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
