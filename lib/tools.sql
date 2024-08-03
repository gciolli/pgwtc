--
-- 1. Generic tools
--

CREATE AGGREGATE gcd (int)
( STYPE = int
, SFUNC = gcd
);

--
-- 2. The "clavis" data type
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

--
-- 3. Metadata
--

-- We record metadata that we do not detect. This includes, for each
-- fugue, the tempo signature and the clavis.

CREATE TYPE tempo AS
( num int
, den int
);

CREATE TEMP TABLE metadata
( bwv int
, clavis clavis
, tempo text NOT NULL
, gcd int
, PRIMARY KEY (bwv)
);

COPY metadata (bwv, clavis, tempo) FROM stdin;
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

CREATE FUNCTION tempo(text)
RETURNS tempo
LANGUAGE SQL
AS $BODY$
SELECT ROW(a[1], a[2]) :: tempo
FROM regexp_match($1, '^([0-9]+)/([0-9]+)$') AS f(a)
$BODY$;

ALTER TABLE metadata
ALTER COLUMN tempo TYPE tempo USING tempo(tempo);

--
-- 4. Clavis algebra
--

-- We record, for each clavis, the smallest positive offset in
-- semitones that eliminates alterations. This could be computed, but
-- it is easier to just record it as given metadata.

CREATE TEMP TABLE clavis_metadata
( id clavis PRIMARY KEY
, maior boolean NOT NULL
, diesis boolean NOT NULL
, o int NOT NULL
);

COPY clavis_metadata FROM stdin;
C	t	t	0
Db	t	f	1
C#	t	t	1
D	t	t	2
Eb	t	f	3
E	t	t	4
F	t	f	5
F#	t	t	6
Gb	t	f	6
G	t	t	7
Ab	t	f	8
A	t	t	9
Bb	t	f	10
B	t	t	11
Cm	f	f	3
C#m	f	t	4
Dm	f	f	5
D#m	f	t	6
Ebm	f	f	6
Em	f	t	7
Fm	f	f	8
F#m	f	t	9
Gm	f	f	10
G#m	f	t	11
Am	f	t	0
Bbm	f	f	1
Bm	f	t	2
\.

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
-- 5. The "locutio" data type
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
-- 6. Adding clavis to locutio
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
-- 7. Converting (ticks, tempo) to (bar, pos)
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
-- 8. Operator # for locutio visualization.
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
    WHEN 1344 THEN  '2..'
    ELSE format('(TODO %s)', this_ticks)
    END)
  END
END
FROM pitch2ly(this_pitch, prev_pitch, sharp, '') AS f(letter)
$BODY$;

-- TODO: the bar is inserted at the first gap when the bar changes,
-- which is inaccurate as it doesn't split notes which cross a bar.

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
  x := format('| [%s/%s] %s', tempo.num, tempo.den
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
      THEN ' ' ELSE E'\n| ' END
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
-- 9. Comparing two locutiones
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

--TODO	CREATE FUNCTION locutio_transpose_tonal(f locutio, n int)
--TODO	RETURNS locutio
--TODO	LANGUAGE plpgsql
--TODO	AS $BODY$
--TODO	BEGIN
--TODO	  f.initial_pitch := f.initial_pitch + n;  
--TODO	  RETURN f;
--TODO	END;
--TODO	$BODY$;
--TODO	
--TODO	CREATE FUNCTION locutio_transpose_modal(f locutio, n int)
--TODO	RETURNS locutio
--TODO	LANGUAGE plpgsql
--TODO	AS $BODY$
--TODO	BEGIN
--TODO	  f.initial_pitch := f.initial_pitch + n;  
--TODO	  RETURN f;
--TODO	END;
--TODO	$BODY$;
