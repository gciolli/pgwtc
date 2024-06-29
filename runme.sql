\set ON_ERROR_STOP 1
\timing on

BEGIN;

\ir lib/tools.sql

-- 3. Analysis

WITH a AS (
  SELECT *
  , #- frase(pitch,t,d) OVER w AS frase
  FROM wtc
  WHERE BWV = 871
  WINDOW w AS (
    PARTITION BY BWV, voice
    ORDER BY t
    RANGE BETWEEN 4 * 384 PRECEDING AND CURRENT ROW
  )
)
SELECT bar, pos, frase
FROM a
WHERE voice = 2
ORDER BY t;

ROLLBACK;
