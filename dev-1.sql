\set ON_ERROR_STOP 1
\timing on

DROP EXTENSION IF EXISTS ly2pg CASCADE;
CREATE EXTENSION pgwtc CASCADE;
SET search_path = pgwtc, ly2pg, public;

SELECT initio @ tempo AS pos
, depth, vox, note, ticks
FROM subject_occurrences
WHERE src = 'BWV858'
ORDER BY initio, vox;
