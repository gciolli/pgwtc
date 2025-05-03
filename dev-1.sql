\set ON_ERROR_STOP 1
\timing on

DROP EXTENSION IF EXISTS ly2pg CASCADE;
CREATE EXTENSION pgwtc CASCADE;
--SET search_path = pgwtc, ly2pg, public;

SELECT src, count(*)
FROM pgwtc.subject_occurrences
GROUP BY ROLLUP (src);
