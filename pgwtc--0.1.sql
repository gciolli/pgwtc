-- complain if script is sourced in psql, rather than via CREATE EXTENSION
\echo Use "CREATE EXTENSION pgwtc" to load this file. \quit

--
-- 1. Metadata
--

CREATE TABLE metadata
( src text
, clavis clavis
, tempo text NOT NULL
, subject_length int
, PRIMARY KEY (src)
);

COPY metadata(src,clavis,tempo,subject_length)
FROM '/usr/share/postgresql/17/extension/pgwtc-metadata.csv' CSV HEADER;

ALTER TABLE metadata
ALTER COLUMN tempo TYPE tempo USING tempo(tempo);

--
-- 2. Notes
--

CREATE TABLE notes (LIKE ly2pg.notes INCLUDING INDEXES INCLUDING CONSTRAINTS);
COPY notes FROM '/usr/share/postgresql/17/extension/pgwtc-notes.csv' CSV HEADER;

--
-- 3. Spectrum
--

CREATE TYPE ratio AS ENUM
( '⅛'
, '⅙'
, '¼'
, '⅓'
, '½'
, '⅔'
, '¾'
, '1'
, '*'
, '1⅓'
, '1½'
, '2'
, '3'
, '4'
, '6'
, '8'
);

CREATE FUNCTION ratio(int,int)
RETURNS ratio
STRICT
LANGUAGE SQL AS $$
SELECT CASE $1 * 120 / $2
WHEN  15 THEN '⅛'
WHEN  20 THEN '⅙'
WHEN  30 THEN '¼'
WHEN  40 THEN '⅓'
WHEN  60 THEN '½'
WHEN  80 THEN '⅔'
WHEN  90 THEN '¾'

WHEN 120 THEN '1'

WHEN 160 THEN '1⅓'
WHEN 180 THEN '1½'
WHEN 240 THEN '2'
WHEN 360 THEN '3'
WHEN 480 THEN '4'
WHEN 720 THEN '6'
WHEN 960 THEN '8'
ELSE '*'
END :: ratio
$$;

CREATE TYPE spectrum AS
( deltas int[]
, ratios ratio[]
, duration int
);

CREATE TYPE spectrum_stype AS
( s spectrum
, start int
, last_nota nota
, last_ticks int
);

CREATE FUNCTION spectrum_sfunc
( s spectrum_stype
, n ly2pg.nota
, start int
, ticks int
) RETURNS spectrum_stype
LANGUAGE plpgsql
AS $$
DECLARE
  o spectrum_stype;
BEGIN
  IF s.start IS NULL THEN
    -- We attempt initializing the state, as it is not initialized
    -- yet.
    IF ly2pg.is_rest(n) THEN
      -- We start from a rest, so we initialize the state in a way
      -- that will not compute the spectrum.
      o.start      := NULL;
    ELSE
      -- We start from a tone, so we initialize the state properly, as
      -- we need to compute the spectrum.
      o.s.deltas   := '{}';
      o.s.ratios   := '{}';
      o.s.duration := ticks;
      o.start      := start;
      o.last_nota  := n;
      o.last_ticks := ticks;
    END IF;
  ELSE
    -- This is not the first iteration, so we compute one delta
    o.s.deltas :=
      (s).s.deltas || (
        CASE WHEN ly2pg.is_rest(n)
	THEN NULL
	ELSE
	  (n).tono - ((s).last_nota.tono % 128)
	END
      );
    o.s.ratios :=
      (s).s.ratios || ratio(ticks, (s).last_ticks);
    o.s.duration := (s).s.duration + ticks;
    o.start        := s.start;
    o.last_nota    := n;
    o.last_ticks   := ticks;
  END IF;
  RETURN o;
END;
$$;

CREATE FUNCTION spectrum_finalfunc
( s spectrum_stype
) RETURNS spectrum
LANGUAGE SQL
AS $$
SELECT CASE
WHEN (s).start IS NOT NULL
THEN ROW
  ( (s).s.deltas
  , (s).s.ratios
  , (s).s.duration
  - CASE WHEN ly2pg.is_rest((s).last_nota)
    THEN (s).last_ticks
    ELSE 0 END
  ) :: spectrum
ELSE NULL :: spectrum
END
$$;

CREATE AGGREGATE spectrum (ly2pg.nota, int, int)
( sfunc     = spectrum_sfunc
, stype     = spectrum_stype
, finalfunc = spectrum_finalfunc
);

CREATE FUNCTION spectrum_duration(spectrum)
RETURNS int
LANGUAGE SQL
AS $BODY$
SELECT ($1).duration
$BODY$;

CREATE FUNCTION deltas_to_text(spectrum)
RETURNS text
LANGUAGE SQL
AS $BODY$
SELECT string_agg
( CASE coalesce(d,999)
  WHEN 999 THEN '.'
  WHEN   0 THEN '='
  WHEN   1 THEN 'A'
  WHEN   2 THEN 'B'
  WHEN   3 THEN 'C'
  WHEN   4 THEN 'D'
  WHEN   5 THEN 'E'
  WHEN   6 THEN 'F'
  WHEN   7 THEN 'G'
  WHEN   8 THEN 'H'
  WHEN   9 THEN 'I'
  WHEN  10 THEN 'J'
  WHEN  11 THEN 'K'
  WHEN  12 THEN 'L'
  WHEN  13 THEN 'M'
  WHEN  14 THEN 'N'
  WHEN  -1 THEN 'a'
  WHEN  -2 THEN 'b'
  WHEN  -3 THEN 'c'
  WHEN  -4 THEN 'd'
  WHEN  -5 THEN 'e'
  WHEN  -6 THEN 'f'
  WHEN  -7 THEN 'g'
  WHEN  -8 THEN 'h'
  WHEN  -9 THEN 'i'
  WHEN -10 THEN 'j'
  WHEN -11 THEN 'k'
  WHEN -12 THEN 'l'
  WHEN -13 THEN 'm'
  WHEN -14 THEN 'n'
  ELSE format(' FIXME(%s) ', d) END
  , '' ORDER BY n )
FROM unnest(($1).deltas) WITH ORDINALITY AS f(d, n)
$BODY$;

CREATE FUNCTION rhythm_to_text(spectrum)
RETURNS text
LANGUAGE SQL
AS $BODY$
SELECT string_agg(r :: text, '' ORDER BY n)
FROM unnest(($1).ratios) WITH ORDINALITY AS f(r, n)
$BODY$;

CREATE FUNCTION spectrum_to_text(spectrum)
RETURNS text
LANGUAGE SQL
AS $BODY$
SELECT deltas_to_text($1) || ' ' || rhythm_to_text($1)
$BODY$;

CREATE OPERATOR ##
( FUNCTION = spectrum_to_text
, RIGHTARG = spectrum
);

CREATE OPERATOR #
( FUNCTION = spectrum_duration
, RIGHTARG = spectrum
);

--
-- Ticks 2 bars
--

CREATE FUNCTION ticks_display(int, tempo)
RETURNS text
LANGUAGE SQL
AS $$
WITH a(bar, beat) AS (
  SELECT
    384 * ($2).num / ($2).den AS bar
  , 384 / ($2).den AS beat
)
SELECT format('%03s:%s'
, $1 / bar + 1
, round(($1 % bar) / (beat :: numeric) + 1, 3)
)
FROM a
$$;

CREATE OPERATOR @
( FUNCTION = ticks_display
, LEFTARG = int
, RIGHTARG = tempo
);

--
-- Formatting Lilypond code
--

CREATE FUNCTION lilypond_voice
( src text
, vox vox
, start_id int DEFAULT NULL
, max_count bigint DEFAULT NULL
) RETURNS text
LANGUAGE plpgsql AS $BODY$
DECLARE
  v_src ALIAS FOR src;
  v_vox ALIAS FOR vox;
  start_ord int DEFAULT 1;
  x text;
BEGIN
  IF start_id IS NOT NULL THEN
    SELECT ord
    INTO STRICT start_ord
    FROM pgwtc.notes n
    WHERE n.src = v_src
    AND n.vox = v_vox
    AND id = start_id;
  END IF;
  WITH RECURSIVE ids AS (
    SELECT id, ord
    FROM pgwtc.notes n
    WHERE n.src = v_src
    AND n.vox = v_vox
    AND ord = start_ord
  UNION ALL
    SELECT n.id, n.ord
    FROM ids
    JOIN pgwtc.notes n
      ON n.src = v_src
     AND n.vox = v_vox
     AND n.ord = ids.ord + 1
     AND COALESCE(n.ord < start_ord + max_count, true)
  ), notes_with_durations AS (
    SELECT ord
    , string_agg(ly2pg.lilypond(nota, f.d), ' ~ ' ORDER BY f.i) AS lilypond
    FROM ids
    JOIN pgwtc.notes n USING (id, ord)
    CROSS JOIN LATERAL unnest(n.durations) WITH ORDINALITY AS f(d, i)
    GROUP BY ord
  )
  SELECT string_agg(lilypond, ' ' ORDER BY ord)
  INTO x STRICT
  FROM notes_with_durations;
  RETURN x;
END;
$BODY$;

--
-- Subjects
--

CREATE VIEW subjects AS
WITH RECURSIVE first_notes AS (
  SELECT DISTINCT ON (src)
    min(start) AS start
  , src
  , vox
  FROM pgwtc.notes
  WHERE (nota).tono IS NOT NULL
  GROUP BY src, vox
  ORDER BY src, min(start)
), subject_ids AS (
  SELECT
    src
  , vox
  , id
  , ord
  , 1 AS depth
  , n.nota
  FROM first_notes f
  JOIN pgwtc.notes n USING (src, vox, start)
UNION ALL
  SELECT
    n.src
  , n.vox
  , n.id
  , n.ord
  , s.depth + 1 AS depth
  , n.nota
  FROM pgwtc.notes n
  JOIN pgwtc.metadata m USING (src)
  JOIN subject_ids s
    ON n.src = s.src
   AND n.vox = s.vox
   AND n.ord = s.ord + 1
  WHERE s.depth < m.subject_length
), aggregated_subject_ids AS (
  SELECT
    src
  , vox
  , array_agg(id   ORDER BY ord) AS ids
  , array_agg(nota ORDER BY ord) AS note
  FROM subject_ids
  GROUP BY src, vox
  ORDER BY src, vox
)
SELECT
  src
, vox
, start
, tempo
, clavis
, lilypond_voice(src, vox, ids[1], array_length(ids, 1))
, ids
, note
FROM aggregated_subject_ids s
JOIN pgwtc.metadata USING (src)
JOIN first_notes USING (src, vox)
ORDER BY src, vox;

CREATE VIEW subject_occurrences AS
WITH RECURSIVE occurrences AS (
  SELECT 2 AS depth
  , n2.ord
  , n1.src
  , n1.vox
  , n1.start AS initio
  , ARRAY[n1.id , n2.id ] AS ids
  , ARRAY[n1.nota , n2.nota ] AS note
  FROM pgwtc.notes n1
  JOIN pgwtc.notes n2
    ON n2.src = n1.src
   AND n2.vox = n1.vox
   AND n2.ord = n1.ord + 1
  JOIN pgwtc.subjects s
    ON s.src = n1.src
  WHERE NOT ly2pg.is_rest(n1.nota)
    AND NOT s.ids[1] = n1.id
    AND n2.nota   - n1.nota
    	IS NOT DISTINCT FROM
        s.note[2] - s.note[1]
UNION ALL
  SELECT o.depth + 1
  , o.ord + 1
  , o.src
  , o.vox
  , o.initio
  , o.ids   || n.id
  , o.note  || n.nota
  FROM occurrences o
  JOIN pgwtc.notes n
    ON n.src = o.src
   AND n.vox = o.vox
   AND n.ord = o.ord + 1
  JOIN pgwtc.subjects s
    ON s.src = o.src
  WHERE n.nota - o.note[o.depth]
        IS NOT DISTINCT FROM
        s.note[o.depth + 1] - s.note[o.depth]
), longest_occurrences AS (
  SELECT DISTINCT ON (src, vox, initio)
    *
  FROM occurrences
  WHERE depth > 4
  ORDER BY src, vox, initio, depth DESC
)
SELECT
  o.src
, o.vox
, o.initio
, o.ord
, o.depth
, o.ids
FROM longest_occurrences o
ORDER BY src, o.initio, vox;

CREATE VIEW subject_occurrences_pretty AS
SELECT
  src
, vox
, initio @ tempo AS initio
, lilypond_voice(src, vox, ids[1], array_length(ids, 1))
FROM subject_occurrences o
JOIN pgwtc.metadata USING (src)
ORDER BY src, o.initio;

CREATE FUNCTION lilypond_subjects()
RETURNS SETOF text
LANGUAGE plpgsql
AS $BODY$
DECLARE
  x RECORD;
BEGIN

  RETURN NEXT $$
\documentclass[a4paper]{article}
\title{pgwtc subjects}
\begin{document}
$$;
  FOR x IN
    SELECT *
    , format('%s/%s', (tempo).num, (tempo).den) AS time
    FROM pgwtc.subjects
    ORDER BY src
  LOOP

    RETURN NEXT format($$
\section{%s (%s)}
\begin{lilypond}
\absolute{
  \key %s
  \time %s
  \clef %s
  %s
  %s
}
\end{lilypond}
$$
    , x.src
    , x.vox
    , ly2pg.clavis2lilypond(x.clavis)
    , x.time
    , CASE x.vox
      WHEN 'soprano' THEN 'treble'
      WHEN 'alto'    THEN 'treble'
      WHEN 'mezzo'   THEN 'treble'
      WHEN 'tenor'   THEN 'bass'
      WHEN 'bass'    THEN 'bass'
      END
    , CASE
      WHEN x.start > 0 THEN
      format('r%s', ly2pg.ticks2duration(x.start))
      ELSE ''
      END
    , x.lilypond_voice
    );
  END LOOP;

  RETURN NEXT $$
\end{document}
$$;
END;
$BODY$;
