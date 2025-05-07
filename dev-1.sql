\set ON_ERROR_STOP 1
\timing on

DROP EXTENSION IF EXISTS ly2pg CASCADE;
CREATE EXTENSION pgwtc CASCADE;
--SET search_path = pgwtc, ly2pg, public;

SELECT pattern_id, count(*)
FROM pgwtc.subject_occurrences_pretty
GROUP BY pattern_id
ORDER BY pattern_id;

SELECT *
FROM pgwtc.subject_occurrences_pretty
WHERE src = 'BWV857' \gx
