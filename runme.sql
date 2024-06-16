\set ON_ERROR_STOP 1
\timing on

BEGIN;

\ir tools.sql

\q

-- 3. Analysis

WITH a AS (
  SELECT *
  , #- frase(pitch,t,d) OVER w AS f
  FROM notes NATURAL JOIN metadata
  WHERE bwv = 871
  WINDOW w AS (
    PARTITION BY bwv, voice
    ORDER BY t
    RANGE BETWEEN 384*4 PRECEDING AND CURRENT ROW
  )
)
SELECT *
FROM a
WHERE voice = 2
ORDER BY t;

ROLLBACK;
