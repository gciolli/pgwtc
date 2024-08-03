--
-- 1. Data load
--

CREATE TEMP TABLE notes
( bwv int
, voice int
, pitch int
, t int
, d int NOT NULL
, CONSTRAINT notes_pk PRIMARY KEY (bwv, voice, t, pitch)
);

\copy notes FROM 'notes.csv' CSV HEADER

--
-- 2. Correct anacrusis
--

-- Anacrusis (the \partial command in Lilypond) occurs in 5 of the 48
-- fugues, and is not preserved in the MIDI output. So we manually
-- restore the original note positions by moving the notes back for
-- the right amount of ticks.

UPDATE notes SET t = t - 192 WHERE bwv IN (856, 881, 893);
UPDATE notes SET t = t - 384 WHERE bwv = 879;
UPDATE notes SET t = t - 768 WHERE bwv = 882;

--
-- 3. Remove grace notes
--

-- Now we must undo the effects of grace notes, which are not
-- considered in this analysis.
--
-- There are seven grace notes here, either 1/8 or 1/16. After
-- inspecting the MIDI files, we found that a grace note of 1/8
-- causes:
--
-- 1.  the shortening of the previous note by 44
--
-- 2.  the insertion of an extra note of duration 43 ticks, starting
--     exactly at the end of the previous note
--
-- The grace note of 1/16 causes the same impact, except that the
-- numbers are respectively 22 and 21.
--
-- So our remedial actions would be to:
--
-- 1.  delete all grace notes (detected by the unusual lengths)
--
-- 2.  for each note ending at the start of a deleted grace note, add
--     1 + D to its duration, where D is the duration of the deleted
--     grace note.

\qecho --
\qecho -- Deleted grace notes
\qecho --

WITH deleted_graces AS (
  DELETE FROM notes
  WHERE d IN (21, 43)
  RETURNING *
)
UPDATE notes n
SET d = n.d + 1 + g.d
FROM deleted_graces g
WHERE (g.bwv, g.voice) = (n.bwv, n.voice)
  AND g.t = n.t + n.d
RETURNING
  n.bwv
, n.voice
, n.pitch
, n.t
, n.d
, g.t AS g_t
, g.d AS g_d
;

--
-- 4. Remove chords
--

-- We also delete chords, because we are only interested in analysing
-- monophonic voices. In other words, we make the assumption that
-- chords only occur outside of counterpoint.

WITH dups AS (
  SELECT bwv, voice, t
  , array_agg(pitch) AS pitches
  FROM notes
  GROUP BY bwv, voice, t
  HAVING count(*) > 1
)
DELETE FROM notes
USING dups
WHERE dups.voice = notes.voice
  AND dups.bwv   = notes.bwv
  AND dups.t     = notes.t;

--
-- 5. Detect and assert note time resolution
--

WITH a AS (
  SELECT bwv, gcd(t) FROM notes GROUP BY bwv
  UNION
  SELECT bwv, gcd(d) FROM notes GROUP BY bwv
)
UPDATE metadata
SET gcd = a.gcd
FROM a
WHERE metadata.bwv = a.bwv;

DO $_$ BEGIN

  ASSERT (
    SELECT count(*) = 0
    FROM metadata
    WHERE gcd NOT IN (192, 96, 48, 32, 16)
  );

END; $_$ LANGUAGE plpgsql;

--
-- 6. Readable view
--

-- View that exposes the notes in a readable format.

CREATE VIEW wtc AS
SELECT bwv
, clavis
, tempo
, voice
, pitch - 60 AS pitch
, t
, d
, bar
, round(pos, 3) AS pos
FROM notes NATURAL JOIN metadata, bar_pos(t, tempo) AS f(bar, pos)
ORDER BY bwv, bar, pos, voice;
