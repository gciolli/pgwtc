\set ON_ERROR_STOP 1
\timing on

DROP EXTENSION IF EXISTS ly2pg CASCADE;
CREATE EXTENSION pgwtc CASCADE;
SET search_path = pgwtc, ly2pg, public;

SELECT src
, initio @ tempo AS pos
, vox
, depth
, lilypond(note)
, ticks
FROM subject_occurrences
ORDER BY src, initio, vox;
