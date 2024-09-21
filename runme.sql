\set ON_ERROR_STOP 1
\timing on

BEGIN;

\ir lib/tools.sql
\ir lib/load.sql

SELECT *
FROM wtc
WHERE BWV = 870
AND voice = 4
ORDER BY t LIMIT 10;
\q

-- 3. Analysis

SELECT locutio(pitch,t,d ORDER BY t)
  @ clavis
  @ 'C'
  # tempo
FROM wtc
WHERE BWV = 870 -- AND voice = 4
GROUP BY bwv, voice, clavis, tempo
ORDER BY bwv, voice;

\q

WITH a AS (
  SELECT *
  , locutio(pitch,t,d) OVER w
  FROM wtc
  WHERE BWV = 871
  WINDOW w AS (
    PARTITION BY BWV, voice
    ORDER BY t
    RANGE BETWEEN CURRENT ROW AND 4 * 384 FOLLOWING
  )
--?), b AS (
--?  SELECT a1.bar, a1.pos, a1.locutio, a2.bar, a2.pos, a2.locutio
--?  FROM a AS a1
--?  CROSS JOIN a AS a2
--?  WHERE a1.voice = 1
--?    AND a2.voice = 2
--?--    AND a1.locutio <-> a2.locutio > 0
--?  ORDER BY a1.t, a2.t
)
SELECT bar, pos, #- locutio
FROM a
WHERE voice = 2
ORDER BY t;

ROLLBACK;
