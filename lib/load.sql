--
-- First, we load the notes.csv file into the temporary "wtc_notes"
-- table, and we create and fill the wtc_metadata table (step 1).
--
-- We then perform data cleaning (steps 2, 3, 4), extraction (step 5)
-- and sanity checks (step 6).
--
-- Finally we expose the contents of wtc_notes and wtc_metadata in a
-- more readable "wtc" view (step 7).
--

--
-- 1. Data load
--

CREATE TEMP TABLE wtc_notes
( bwv int
, voice int
, pitch int
, t int
, d int NOT NULL
, CONSTRAINT wtc_notes_pk PRIMARY KEY (bwv, voice, t, pitch)
);

\copy wtc_notes FROM 'notes.csv' CSV HEADER

-- We populate the wtc_metadata table with some data that we do not
-- extract from the source .ly files.

CREATE TEMP TABLE wtc_metadata
( bwv int
, clavis clavis
, tempo text NOT NULL
, gcd int
, PRIMARY KEY (bwv)
);

COPY wtc_metadata (bwv, clavis, tempo) FROM stdin;
846	C	4/4
847	Cm	4/4
848	C#	4/4
849	C#m	2/2
850	D	4/4
851	Dm	3/4
852	Eb	4/4
853	D#m	4/4
854	E	4/4
855	Em	3/4
856	F	3/8
857	Fm	4/4
858	F#	4/4
859	F#m	6/4
860	G	6/8
861	Gm	4/4
862	Ab	4/4
863	G#m	4/4
864	A	9/8
865	Am	4/4
866	Bb	3/4
867	Bbm	2/2
868	B	4/4
869	Bm	4/4
870	C	2/4
871	Cm	4/4
872	C#	4/4
873	C#m	12/16
874	D	2/2
875	Dm	4/4
876	Eb	2/2
877	D#m	4/4
878	E	2/1
879	Em	2/2
880	F	6/16
881	Fm	2/4
882	F#	2/2
883	F#m	4/4
884	G	3/8
885	Gm	3/4
886	Ab	4/4
887	G#m	6/8
888	A	4/4
889	Am	4/4
890	Bb	3/4
891	Bbm	3/2
892	B	2/2
893	Bm	3/8
\.

ALTER TABLE wtc_metadata
ALTER COLUMN tempo TYPE tempo USING tempo(tempo);

--
-- 2. Correct anacrusis
--

-- Anacrusis (the \partial command in Lilypond) occurs in 5 of the 48
-- fugues, and is not preserved in the MIDI output. So we manually
-- restore the original note positions by moving the notes back for
-- the right amount of ticks.

UPDATE wtc_notes SET t = t - 192 WHERE bwv IN (856, 881, 893);
UPDATE wtc_notes SET t = t - 384 WHERE bwv = 879;
UPDATE wtc_notes SET t = t - 768 WHERE bwv = 882;

--
-- 3. Remove grace notes
--

-- We must undo the effects of grace notes, as they are not considered
-- in this analysis, and would confuse it.
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

CREATE TEMP TABLE wtc_grace_notes_deleted (LIKE wtc_notes);

WITH deleted AS (
  DELETE FROM wtc_notes
  WHERE d IN (21, 43)
  RETURNING *
), debug AS (
  INSERT INTO wtc_grace_notes_deleted
  SELECT * FROM deleted
)
UPDATE wtc_notes n
SET d = n.d + 1 + deleted.d
FROM deleted
WHERE (deleted.bwv, deleted.voice) = (n.bwv, n.voice)
  AND deleted.t = n.t + n.d
RETURNING
  n.bwv
, n.voice
, n.pitch
, n.t
, n.d
, deleted.t AS deleted_t
, deleted.d AS deleted_d
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
  FROM wtc_notes
  GROUP BY bwv, voice, t
  HAVING count(*) > 1
)
DELETE FROM wtc_notes
USING dups
WHERE dups.voice = wtc_notes.voice
  AND dups.bwv   = wtc_notes.bwv
  AND dups.t     = wtc_notes.t;

--
-- 5. Compute notes resolution
--

WITH a AS (
  SELECT bwv, gcd(t) FROM wtc_notes GROUP BY bwv
  UNION
  SELECT bwv, gcd(d) FROM wtc_notes GROUP BY bwv
)
UPDATE wtc_metadata
SET gcd = a.gcd
FROM a
WHERE wtc_metadata.bwv = a.bwv;

--
-- 6. Sanity checks
--

DO $_$ BEGIN

  ASSERT (
    SELECT count(*) = 0
    FROM wtc_metadata
    WHERE gcd NOT IN (192, 96, 48, 32, 16)
  ), 'Unexpected note tick resolution';

  ASSERT (
    SELECT count(*) = 7
    FROM wtc_grace_notes_deleted
  ), 'Unexpected number of grace notes deleted';

END; $_$ LANGUAGE plpgsql;

--
-- 7. Readable view
--

-- View that exposes notes and metadata in a readable format.

CREATE VIEW wtc AS
SELECT bwv
, clavis
, tempo
, voice
, pitch --- 60 AS pitch
, t
, d
, bar
, round(pos, 3) AS pos
FROM wtc_notes NATURAL JOIN wtc_metadata, bar_pos(t, tempo) AS f(bar, pos)
ORDER BY bwv, bar, pos, voice;
