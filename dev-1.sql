\set ON_ERROR_STOP 1
\timing on

DROP EXTENSION IF EXISTS ly2pg CASCADE;
CREATE EXTENSION pgwtc CASCADE;
--SET search_path = pgwtc, ly2pg, public;

SELECT *
FROM pgwtc.subject_occurrences_pretty
WHERE src = 'BWV846';

\q

SELECT src, count(*)
FROM pgwtc.subject_occurrences_pretty
GROUP BY src
ORDER BY src;

\q

SELECT *
FROM pgwtc.subject_patterns_pretty
WHERE src = 'BWV853';

SELECT *
FROM pgwtc.notes_pretty
WHERE src = 'BWV861'
ORDER BY vox, ord;
