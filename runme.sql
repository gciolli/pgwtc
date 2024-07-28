\set ON_ERROR_STOP 1
\timing on

BEGIN;

\ir lib/tools.sql

-- 3. Analysis

WITH a AS (
  SELECT *
  , frase(pitch,t,d) OVER w
  FROM wtc
  WHERE BWV = 871
  WINDOW w AS (
    PARTITION BY BWV, voice
    ORDER BY t
    RANGE BETWEEN CURRENT ROW AND 4 * 384 FOLLOWING
  )
), b AS (
  SELECT a1.bar, a1.pos, a1.frase, a2.bar, a2.pos, a2.frase
  FROM a AS a1
  CROSS JOIN a AS a2
  WHERE a1.voice = 1
    AND a2.voice = 2
--    AND a1.frase <-> a2.frase > 0
  ORDER BY a1.t, a2.t
)
SELECT bar, pos, #- frase
FROM a
WHERE voice = 2
ORDER BY t;

ROLLBACK;
