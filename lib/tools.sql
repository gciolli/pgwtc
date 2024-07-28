--
-- 1. Tools
--


-- 1a. Generic

CREATE AGGREGATE gcd (int)
( STYPE = int
, SFUNC = gcd
);


-- 1b. Mapping semitones to notes in a given key
--
-- We represent each key with the number of alterations, with sharp
-- positive and flat negative, and we map each key to its seven notes,
-- as follows:
--
--   ...
--  -1 = F major = D minor = {0,2,4,5,7,9,10}
--   0 = C major = A minor = {0,2,4,5,7,9,11}
--   1 = G major = E minor = {0,2,4,6,7,9,11}
--   ...
--
-- We consider notes as integers between 0 and 11, so our 12 keys
-- range from Gb (excluded) to F# (included).

CREATE TABLE claves
( alterations int PRIMARY KEY
, notes int[]
);

COPY claves FROM stdin;
-5	{ 0,1,3,5,6, 8,10}
-4	{ 0,1,3,5,7, 8,10}
-3	{ 0,2,3,5,7, 8,10}
-2	{ 0,2,3,5,7, 9,10}
-1	{ 0,2,4,5,7, 9,10}
0	{ 0,2,4,5,7, 9,11}
1	{ 0,2,4,6,7, 9,11}
2	{ 1,2,4,6,7, 9,11}
3	{ 1,2,4,6,8, 9,11}
4	{ 1,3,4,6,8, 9,11}
5	{ 1,3,4,6,8,10,11}
6	{ 1,3,5,6,8,10,11}
\.

-- Then we mapping semitones to notes in a given key, where semitones
-- not in the scale are represented by 0.5 plus the note for the
-- semitone below, as in this C major example:
--
-- 60 = 35
-- 61 = 35.5
-- 62 = 36
-- 63 = 36.5
-- 64 = 37
-- 65 = 38
-- 66 = 38.5
-- 67 = 39
--   ...

CREATE FUNCTION note
( IN semitone int
, IN clavis int
, OUT note numeric
) LANGUAGE SQL
AS $BODY$
  WITH a AS (
    SELECT h, n1 - 1 AS n
    FROM claves c
    , unnest(c.notes) WITH ORDINALITY AS f(h, n1)
    WHERE c.alterations = $2
  ), b(h, note) AS (
    SELECT h, (semitone / 12) * 7 + n
    FROM a
    WHERE semitone % 12 = h
  UNION
    SELECT h, (semitone / 12) * 7 + n + 0.5
    FROM a
    WHERE semitone % 12 = h + 1
  )
  SELECT note
  FROM b
  ORDER BY h DESC LIMIT 1
$BODY$;


-- 1d. The "frase" data type

--
-- A "frase" is a sequence of notes. The pitches are stored as an
-- initial pitch, followed by an array of relative pitch differences,
-- and by an array of durations in ticks. A difference is NULL if it
-- means a pause.
--

CREATE TYPE frase AS
( initial_pitch int
, pitch_deltas int[]
, ticks int[]
);

--
-- The "frase" aggregate function takes a set of ordered notes (p,t,d)
-- and returns the corresponding frase.
--

CREATE FUNCTION frase_sfunc(s int[], p int, t int, d int)
RETURNS int[]
LANGUAGE plpgsql
AS $BODY$
BEGIN
  RETURN COALESCE (s, '{}') || ARRAY[ARRAY[p,t,d]];
END;
$BODY$;

CREATE FUNCTION frase_ffunc(s int[])
RETURNS frase
LANGUAGE plpgsql
AS $BODY$
DECLARE
  n int;
  o frase;
BEGIN
  -- n-th note: ARRAY[s[n][1], s[n][2], s[n][3]] = (p,t,d)
  o.initial_pitch := s[1][1];
  o.pitch_deltas := '{}';
  o.ticks := ARRAY[s[1][3]];
  FOR n IN 2..array_length(s,1) LOOP
    o.pitch_deltas := o.pitch_deltas || (s[n][1] - s[n-1][1]);
    o.ticks := o.ticks || s[n][3];
    -- TODO: consider pauses
  END LOOP;
  RAISE DEBUG E'\n';
  RETURN o;
END;
$BODY$;

CREATE AGGREGATE frase (p int, t int, d int)
( STYPE = int[]
, SFUNC = frase_sfunc
, FINALFUNC = frase_ffunc
);

--
-- Operators #+ and #- for frase visualization.
--

CREATE FUNCTION pitch2ly
( this_pitch int
, prev_pitch int
, sharp boolean DEFAULT false
, suffix text DEFAULT ''
) RETURNS text
LANGUAGE SQL
AS $BODY$
WITH ly(p12,ls,lf,s7,f7) AS (VALUES
  ( 0, 'c'  , 'c'  , 0, 0)
, ( 1, 'cis', 'des', 0, 1)
, ( 2, 'd'  , 'd'  , 1, 1)
, ( 3, 'dis', 'ees', 1, 2)
, ( 4, 'e'  , 'e'  , 2, 2)
, ( 5, 'f'  , 'f'  , 3, 3)
, ( 6, 'fis', 'ges', 3, 4)
, ( 7, 'g'  , 'g'  , 4, 4)
, ( 8, 'gis', 'aes', 4, 5)
, ( 9, 'a'  , 'a'  , 5, 5)
, (10, 'ais', 'bes', 5, 6)
, (11, 'b'  , 'b'  , 6, 6)
), a1 AS (
  SELECT
    CASE WHEN sharp THEN ls ELSE lf END AS letter
  , CASE WHEN sharp THEN s7 ELSE f7 END + ((this_pitch + 12) / 12) * 7
    AS this_note
  FROM ly
  WHERE p12 = (this_pitch + 12) % 12
), a2 AS (
  SELECT
    CASE WHEN sharp THEN s7 ELSE f7 END + ((prev_pitch + 12) / 12) * 7
    AS prev_note
  FROM ly
  WHERE p12 = (prev_pitch + 12) % 12
)
SELECT CASE
--WHEN true THEN format('%s%s [%s:%s]', letter, suffix, this_pitch, prev_pitch)
WHEN this_note - prev_note >  3
THEN pitch2ly(this_pitch, prev_pitch + 12, sharp, suffix || '''')
WHEN this_note - prev_note < -3
THEN pitch2ly(this_pitch + 12, prev_pitch, sharp, suffix || ',')
ELSE format ('%s%s', COALESCE(letter,'?'), suffix) END
FROM a1 FULL OUTER JOIN a2 ON true
$BODY$;

CREATE FUNCTION ticks2ly(int)
RETURNS text
LANGUAGE SQL
AS $BODY$
SELECT
  CASE $1
  WHEN 768 THEN  '2'
  WHEN 384 THEN  '4'
  WHEN 192 THEN  '8'
  WHEN  96 THEN '16'
  --
  WHEN 576 THEN '4.'
  WHEN 288 THEN '8.'
  --
  WHEN 672 THEN '4..'
  --
  ELSE format('ERROR(%s)', $1)
  END
$BODY$;

CREATE FUNCTION pd2ly
( this_pitch int
, prev_pitch int
, this_ticks int
, prev_ticks int
, sharp boolean DEFAULT false
) RETURNS text
LANGUAGE SQL
AS $BODY$
SELECT
CASE
  WHEN this_ticks = prev_ticks THEN letter
ELSE
  CASE this_ticks

  -- Certain durations cannot be expressed with a single note, so they
  -- require a tie of multiple notes
  WHEN 480 THEN format
    ( '%s%s ~ %s%s', letter, ticks2ly(384), letter, ticks2ly(96))

  -- Here we map all the remaining durations to Lilypond notation
  ELSE format('%s%s', letter,
    CASE this_ticks
    WHEN 768 THEN  '2'
    WHEN 384 THEN  '4'
    WHEN 192 THEN  '8'
    WHEN  96 THEN '16'
    --
    WHEN 576 THEN  '4.'
    WHEN 288 THEN  '8.'
    --
    WHEN 672 THEN  '4..'
    ELSE format('(TODO %s)', this_ticks)
    END)
  END
END
FROM pitch2ly(this_pitch, prev_pitch, sharp, '') AS f(letter)
$BODY$;

CREATE FUNCTION frase2ly(a frase, sharp boolean)
RETURNS text
LANGUAGE plpgsql
AS $BODY$
DECLARE
  n0 int := a.initial_pitch;
  d0 int := a.ticks[1];
  n1 int := n0;
  d1 int := NULL;
  x text := pd2ly(n0, n1, d0, d1, sharp);
BEGIN
  IF array_length(a.ticks,1) > 1 THEN
    FOR n IN 2..array_length(a.ticks,1) LOOP
      n1 := n0;
      n0 := n0 + a.pitch_deltas[n-1];
      d1 := d0;
      d0 := a.ticks[n];
      x := format('%s %s', x, pd2ly(n0, n1, d0, d1, sharp));
    END LOOP;
  END IF;
  RETURN x;
END;
$BODY$;

CREATE FUNCTION frase2ly_sharp(a frase)
RETURNS text
LANGUAGE SQL
AS 'SELECT frase2ly($1,true)';

CREATE FUNCTION frase2ly_flat(a frase)
RETURNS text
LANGUAGE SQL
AS 'SELECT frase2ly($1,false)';

CREATE OPERATOR #+
( RIGHTARG = frase
, FUNCTION = frase2ly_sharp
);

CREATE OPERATOR #-
( RIGHTARG = frase
, FUNCTION = frase2ly_flat
);

CREATE FUNCTION frase_dist(a frase, b frase)
RETURNS float
LANGUAGE plpgsql
AS $BODY$
BEGIN
  RAISE INFO 'a: %', a;
  RAISE INFO 'b: %', b;
  RAISE EXCEPTION 'TODO';
  RETURN NULL;
END;
$BODY$;

CREATE FUNCTION frase_transpose_tonal(f frase, n int)
RETURNS frase
LANGUAGE plpgsql
AS $BODY$
BEGIN
  f.initial_pitch := f.initial_pitch + n;  
  RETURN f;
END;
$BODY$;

CREATE FUNCTION frase_transpose_modal(f frase, n int)
RETURNS frase
LANGUAGE plpgsql
AS $BODY$
BEGIN
  f.initial_pitch := f.initial_pitch + n;  
  RETURN f;
END;
$BODY$;

--
-- 2. Data load and cleaning
--

-- 2a. Data load

CREATE TEMP TABLE notes
( bwv int
, voice int
, pitch int
, t int
, d int NOT NULL
, CONSTRAINT notes_pk PRIMARY KEY (bwv, voice, t, pitch)
);

\copy notes FROM 'notes.csv' CSV HEADER

-- 2b. Add metadata

-- We record metadata that we do not detect. This includes the tempo
-- signature and whether the key is sharp.

CREATE TEMP TABLE metadata
( bwv int
, tempo1 int NOT NULL
, tempo2 int NOT NULL
, sharp boolean NOT NULL
, gcd int
, PRIMARY KEY (bwv)
);

COMMENT ON COLUMN metadata.sharp IS
'This column records whether the key has flats (e.g. C minor) or sharps
(e.g. D major).';

-- TODO: inspect the scores and set the "sharp" column accordingly.

COPY metadata (bwv, sharp, tempo1, tempo2) FROM stdin;
846	t	4	4
847	t	4	4
848	t	4	4
849	t	2	2
850	t	4	4
851	t	3	4
852	t	4	4
853	t	4	4
854	t	4	4
855	t	3	4
856	t	3	8
857	t	4	4
858	t	4	4
859	t	6	4
860	t	6	8
861	t	4	4
862	t	4	4
863	t	4	4
864	t	9	8
865	t	4	4
866	t	3	4
867	t	2	2
868	t	4	4
869	t	4	4
870	t	2	4
871	f	4	4
872	t	4	4
873	t	12	16
874	t	2	2
875	t	4	4
876	t	2	2
877	t	4	4
878	t	2	1
879	t	2	2
880	t	6	16
881	t	2	4
882	t	2	2
883	t	4	4
884	t	3	8
885	t	3	4
886	t	4	4
887	t	6	8
888	t	4	4
889	t	4	4
890	t	3	4
891	t	3	2
892	t	2	2
893	t	3	8
\.

-- 2c. Correct anacrusis

--
-- Anacrusis (the \partial command in Lilypond) occurs in 5 of the 48
-- fugues, and is not preserved in the MIDI output. So we manually
-- restore the original note positions by moving the notes back for
-- the right amount of ticks.
--

UPDATE notes SET t = t - 192 WHERE bwv IN (856, 881, 893);
UPDATE notes SET t = t - 384 WHERE bwv = 879;
UPDATE notes SET t = t - 768 WHERE bwv = 882;

-- 2d. Remove grace notes

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
--

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

-- 2e. Remove chords

--
-- We also delete chords, because we are only interested in analysing
-- monophonic voices. In other words, we make the assumption that
-- chords only occur outside of counterpoint.
--

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

-- 2f. Detect note time resolution

WITH a AS (
  SELECT bwv, gcd(t) FROM notes GROUP BY bwv
  UNION
  SELECT bwv, gcd(d) FROM notes GROUP BY bwv
)
UPDATE metadata
SET gcd = a.gcd
FROM a
WHERE metadata.bwv = a.bwv;

-- 2g. Assert expected note resolution

DO $_$ BEGIN

  ASSERT (
    SELECT count(*) = 0
    FROM metadata
    WHERE gcd NOT IN (192, 96, 48, 32, 16)
  );

END; $_$ LANGUAGE plpgsql;

-- 2h. Readable view

--
-- Function that converts the duration in ticks into a more readable
-- pair composed by the bar number and the position within the bar.
--

CREATE FUNCTION dm
( IN ticks int
, IN tempo1 int
, IN tempo2 int
, OUT bar int
, OUT pos numeric
) LANGUAGE SQL
AS $$
SELECT 1 + floor (CAST (ticks AS numeric) * tempo2 / tempo1 / 1536)
, CAST (mod (ticks + 1536 * tempo1 / tempo2, 1536 * tempo1 / tempo2) AS numeric) / 1536 * tempo2
$$;

--
-- View that exposes the notes in a readable format.
--

CREATE VIEW wtc AS
SELECT bwv
, format('%s/%s', tempo1, tempo2) AS tempo
, voice
, pitch - 60 AS pitch
, t
, d
, bar
, round(pos, 3) AS pos
FROM notes NATURAL JOIN metadata, dm(t, tempo1, tempo2) AS f(bar, pos)
ORDER BY bwv, bar, pos, voice;
