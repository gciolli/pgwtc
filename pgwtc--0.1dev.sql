-- complain if script is sourced in psql, rather than via CREATE EXTENSION
\echo Use "CREATE EXTENSION pgwtc" to load this file. \quit

--
-- Chapter I. Creating database objects
--

--
-- I.1 Generic objects
--

CREATE AGGREGATE gcd (int)
( STYPE = int
, SFUNC = gcd
);

--
-- I.2 The "clavis" data type
--

CREATE TYPE clavis AS ENUM
( 'C'
, 'Cm'
, 'C#'
, 'Db'
, 'C#m'
, 'D'
, 'Dm'
, 'Eb'
, 'D#m'
, 'Ebm'
, 'E'
, 'Em'
, 'F'
, 'Fm'
, 'F#'
, 'Gb'
, 'F#m'
, 'G'
, 'Gm'
, 'Ab'
, 'G#m'
, 'A'
, 'Am'
, 'Bb'
, 'Bbm'
, 'B'
, 'Bm'
);

-- We record, for each clavis, the smallest positive offset in
-- semitones that eliminates alterations. This could be computed, but
-- it is easier to just record it as given metadata.

CREATE UNLOGGED TABLE clavis_metadata
( id clavis PRIMARY KEY
, maior boolean NOT NULL
, diesis boolean NOT NULL
, o int NOT NULL
);

COPY clavis_metadata FROM '/usr/share/postgresql/16/extension/pgwtc-clavis-metadata.csv' CSV HEADER;

CREATE FUNCTION spatium_clavium (a clavis, b clavis)
RETURNS int
LANGUAGE SQL
AS $BODY$
SELECT ma.o - mb.o
FROM clavis_metadata AS ma, clavis_metadata AS mb
WHERE ma.id = a AND mb.id = b
$BODY$;

CREATE OPERATOR -
( LEFTARG = clavis
, RIGHTARG = clavis
, FUNCTION = spatium_clavium
);

--
-- I.3 The "tempo" data type
--

CREATE TYPE tempo AS
( num int
, den int
);

CREATE FUNCTION tempo(text)
RETURNS tempo
LANGUAGE SQL
AS $BODY$
SELECT ROW(a[1], a[2]) :: tempo
FROM regexp_match($1, '^([0-9]+)/([0-9]+)$') AS f(a)
$BODY$;

CREATE FUNCTION tempo2ticks(tempo)
RETURNS int
LANGUAGE SQL
AS $BODY$
SELECT 1536 / ($1).den * ($1).num
$BODY$;

--
-- I.4 The "locutio" data type
--

-- A locutio is a sequence of notes in a given clavis. Originally
-- notes are triplets (pitch, time, duration); in the locutio we
-- record the time of the first note as "t" and then we store the
-- times of each note as relative offsets to the time of the first
-- note. The clavis can be added later with a dedicated operator.

CREATE TYPE locutio AS
( c clavis
, t int
, ps int[]
, ts int[]
, ds int[]
);

-- The "locutio" aggregate function takes a set of ordered notes
-- (p,t,d) and returns the corresponding locutio.

CREATE FUNCTION locutio_sfunc
( o INOUT locutio
, p IN int
, t IN int
, d IN int
) LANGUAGE plpgsql
AS $BODY$
DECLARE
  n int;
  pause int;
BEGIN
  IF o IS NULL THEN
    o.t := t;
    o.ps := ARRAY[p];
    o.ts := ARRAY[t];
    o.ds := ARRAY[d];
  ELSE
    n := array_length(o.ps,1);
    pause := t - o.ts[n] - o.ds[n];
    IF pause > 0 THEN
      -- pause, i.e. the next note starts after the end of the
      -- previous one
      o.ps := o.ps || ARRAY[NULL :: int];
      o.ts := o.ts || (t - pause);
      o.ds := o.ds || pause;
    END IF;
    o.ps := o.ps || p;
    o.ts := o.ts || t;
    o.ds := o.ds || d;
  END IF;
  RAISE DEBUG E'sfunc\n%\n%\n%\n', o.ps, o.ts, o.ds;
END;
$BODY$;

CREATE AGGREGATE locutio (p int, t int, d int)
( STYPE = locutio
, SFUNC = locutio_sfunc
);

--
-- I.5 Adding clavis to locutio
--

-- This function adds a clavis to a locutio that doesn't have one, or
-- transposes a locutio that has a clavis to another clavis.

CREATE FUNCTION locutio_clavis
( o INOUT locutio
, c clavis
) LANGUAGE plpgsql
AS $BODY$
DECLARE
  d int;
BEGIN
  IF o.c IS NULL THEN
    o.c := c;
  ELSE
    d := c - o.c;
    SELECT array_agg(p + d ORDER BY n)
    INTO o.ps
    FROM unnest(o.ps)
    WITH ORDINALITY AS f(p, n);
    o.c := c;
  END IF;
END;
$BODY$;

CREATE OPERATOR @
( LEFTARG = locutio
, RIGHTARG = clavis
, FUNCTION = locutio_clavis
);

--
-- I.6 Converting (ticks, tempo) to (bar, pos)
--

-- Function that converts the duration in ticks into a more readable
-- pair composed by the bar number and the position within the bar.

CREATE FUNCTION bar_pos
( IN ticks int
, IN tempo tempo
, OUT bar int
, OUT pos numeric
) LANGUAGE SQL
AS $$
SELECT 1 + floor (CAST (ticks AS numeric) * tempo.den / tempo.num / 1536)
, CAST (mod (ticks + 1536 * tempo.num / tempo.den, 1536 * tempo.num / tempo.den) AS numeric) / 1536 * tempo.den
$$;

--
-- I.7 Visualizing ticks
--

CREATE FUNCTION ticks2ly(int)
RETURNS text
LANGUAGE SQL
AS $BODY$
SELECT
  CASE $1
  WHEN 1536 THEN  '1'
  WHEN  768 THEN  '2'
  WHEN  384 THEN  '4'
  WHEN  192 THEN  '8'
  WHEN   96 THEN '16'
  WHEN   48 THEN '32'
  --
  WHEN 2304 THEN  '1.'
  WHEN 1152 THEN  '2.'
  WHEN  576 THEN  '4.'
  WHEN  288 THEN  '8.'
  WHEN  144 THEN '16.'
  --
  WHEN 2688 THEN  '1..'
  WHEN 1344 THEN  '2..'
  WHEN  672 THEN  '4..'
  WHEN  336 THEN  '8..'
  --
  ELSE format('ERROR(%s)', $1)
  END
$BODY$;

-- We also need to split uncommon lengths.

--
-- I.8 Operator # for locutio visualization.
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
  WHERE p12 = (this_pitch + 12) % 12 OR this_pitch IS NULL
), a2 AS (
  SELECT
    CASE WHEN sharp THEN s7 ELSE f7 END + ((prev_pitch + 12) / 12) * 7
    AS prev_note
  FROM ly
  WHERE p12 = (prev_pitch + 12) % 12
)
SELECT CASE
WHEN this_pitch IS NULL THEN 'r'
WHEN this_note - prev_note >  3
THEN pitch2ly(this_pitch, prev_pitch + 12, sharp, suffix || '''')
WHEN this_note - prev_note < -3
THEN pitch2ly(this_pitch + 12, prev_pitch, sharp, suffix || ',')
ELSE format ('%s%s'
     , COALESCE(letter,format('??[%s-%s]??', prev_pitch, this_pitch))
     , suffix) END
FROM a1 FULL OUTER JOIN a2 ON true
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
  WHEN  480 THEN format
    ( '%s%s ~ %s%s', letter, ticks2ly( 384), letter, ticks2ly( 96))
  WHEN  864 THEN format
    ( '%s%s ~ %s%s', letter, ticks2ly( 768), letter, ticks2ly( 96))
  WHEN 1632 THEN format
    ( '%s%s ~ %s%s', letter, ticks2ly(1536), letter, ticks2ly( 96))
  WHEN  960 THEN format
    ( '%s%s ~ %s%s', letter, ticks2ly( 768), letter, ticks2ly(192))
  WHEN 2496 THEN format
    ( '%s%s ~ %s%s', letter, ticks2ly(2304), letter, ticks2ly(192))

  -- Here we map all the remaining durations to Lilypond notation
  ELSE format('%s%s', letter, ticks2ly(this_ticks))
  END
END
FROM pitch2ly(this_pitch, prev_pitch, sharp, '') AS f(letter)
$BODY$;

-- TODO: the bar is inserted at the first gap when the bar changes,
-- which is inaccurate as it doesn't split notes which cross a bar.

-- TODO: rests do not need to be tied.

CREATE FUNCTION locutio2ly(l locutio, tempo tempo)
RETURNS text
LANGUAGE plpgsql
AS $BODY$
DECLARE
  p0 int := l.ps[1];
  t  int := l.ts[1];
  d0 int := l.ds[1];
  bar0 int;
  pos0 int;
  bar1 int;
  pos1 int;
  p1 int := p0;
  d1 int := NULL;
  sharp boolean;
  x text;
BEGIN
  SELECT diesis INTO STRICT sharp
  FROM clavis_metadata
  WHERE id = l.c;
  SELECT * INTO STRICT bar0, pos0
  FROM bar_pos(t, tempo);
  x := format('%2s | \time %s/%s %s', bar0, tempo.num, tempo.den
       , pd2ly(p0, p1, d0, d1, sharp));
  IF array_length(l.ds,1) > 1 THEN
    FOR n IN 2..array_length(l.ds,1) LOOP
      p1 := COALESCE(p0,p1);
      d1 := d0;
      bar1 := bar0;
      pos1 := pos0;
      t  := t + d1;
      SELECT * INTO STRICT bar0, pos0
      FROM bar_pos(t, tempo);
      p0 := l.ps[n];
      d0 := l.ds[n];
      x := format('%s%s%s', x
      , CASE WHEN bar1 = bar0
      THEN ' ' ELSE format(E'\n%2s | ', bar0) END
      , pd2ly(p0, p1, d0, d1, sharp));
    END LOOP;
  END IF;
  RETURN x;
END;
$BODY$;

CREATE OPERATOR #
( LEFTARG  = locutio
, RIGHTARG = tempo
, FUNCTION = locutio2ly
);

--
-- I.9 Comparing two locutiones
--

CREATE FUNCTION locutiones_dist(a locutio, b locutio)
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

CREATE OPERATOR -
( LEFTARG  = locutio
, RIGHTARG = locutio
, FUNCTION = locutiones_dist
);

--
-- Chapter II. Loading notes from the Well Tempered Clavier
--

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
-- II.1 Data load
--

CREATE TABLE wtc_notes
( bwv int
, voice int
, pitch int
, t int
, d int NOT NULL
, CONSTRAINT wtc_notes_pk PRIMARY KEY (bwv, voice, t, pitch)
);

COPY wtc_notes FROM '/usr/share/postgresql/16/extension/pgwtc-notes.csv' CSV HEADER;

-- We populate the wtc_metadata table with some data that we do not
-- extract from the source .ly files.

CREATE TABLE wtc_metadata
( bwv int
, clavis clavis
, tempo text NOT NULL
, gcd int
, PRIMARY KEY (bwv)
);

COPY wtc_metadata(bwv,clavis,tempo) FROM '/usr/share/postgresql/16/extension/pgwtc-metadata.csv' CSV HEADER;

ALTER TABLE wtc_metadata
ALTER COLUMN tempo TYPE tempo USING tempo(tempo);

--
-- II.2 Correct anacrusis
--

-- Anacrusis (the \partial command in Lilypond) occurs in 5 of the 48
-- fugues, and is not preserved in the MIDI output. So we manually
-- restore the original note positions by moving the notes back for
-- the right amount of ticks.

UPDATE wtc_notes SET t = t - 192 WHERE bwv IN (856, 881, 893);
UPDATE wtc_notes SET t = t - 384 WHERE bwv = 879;
UPDATE wtc_notes SET t = t - 768 WHERE bwv = 882;

--
-- II.3 Remove grace notes
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
-- II.4 Remove chords
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
-- II.5 Compute notes resolution
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
-- II.6 Sanity checks
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

DROP TABLE wtc_grace_notes_deleted;

--
-- II.7 Readable view
--

-- View that exposes notes and metadata in a readable format.

CREATE VIEW wtc AS
SELECT bwv
, clavis
, tempo
, voice
, pitch
, t
, d
, bar
, round(pos, 3) AS pos
FROM wtc_notes NATURAL JOIN wtc_metadata, bar_pos(t, tempo) AS f(bar, pos)
ORDER BY bwv, bar, pos, voice;
