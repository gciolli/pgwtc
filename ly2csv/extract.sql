BEGIN;

CREATE TEMP TABLE events
( bwv int
, t bigint
, c int
, p int
, v int
);

\copy events FROM PROGRAM 'bash midi2pg.sh tmp/*.midi'

SELECT count(*) FROM events;

-- After loading MIDI events, we need to convert them into actual
-- notes. A note in MIDI corresponds to a pair of events: first, the
-- key is pressed (velocity > 0) and then it is released (velocity =
-- 0).
--
-- We use a window function to pair events that have the same values
-- of (bwv, c, p), so that (t1, v1) is the event immediately following
-- (t, v). Then we filter the pairs that correspond to a note, that
-- is, where v > 0 and v1 = 0. At this point we just compute the
-- duration of the note as d = t1 - t.

CREATE VIEW notes AS
WITH a AS (
  SELECT bwv, c, p, t, v
  , lead (t, 1) OVER w AS t1
  , lead (v, 1) OVER w AS v1
  FROM events
  WINDOW w AS (
    PARTITION BY bwv, c, p
    ORDER BY t, v
  )
)
SELECT bwv, c, p, t, t1 - t AS d
FROM a
WHERE v > 0 AND v1 = 0
ORDER BY bwv, t, c;

\copy (TABLE notes) TO 'tmp/notes.csv' CSV HEADER

ROLLBACK;
