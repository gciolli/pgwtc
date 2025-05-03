-- complain if script is sourced in psql, rather than via CREATE EXTENSION
\echo Use "CREATE EXTENSION pgwtc" to load this file. \quit

--
-- 1. Load data and metadata for the Well-Tempered Clavier
--

CREATE TYPE answer AS ENUM
( 'real'
, 'dominant tonal'
, 'subdominant tonal'
);

CREATE TABLE metadata
( src text
, clavis clavis
, tempo text NOT NULL
, subject_length int
, answer answer
, PRIMARY KEY (src)
);

COPY metadata(src,clavis,tempo,subject_length,answer)
FROM '/usr/share/postgresql/17/extension/pgwtc-metadata.csv' CSV HEADER;

ALTER TABLE metadata
ALTER COLUMN tempo TYPE tempo USING tempo(tempo);

CREATE TABLE notes (LIKE ly2pg.notes INCLUDING INDEXES INCLUDING CONSTRAINTS);
COPY notes FROM '/usr/share/postgresql/17/extension/pgwtc-notes.csv' CSV HEADER;

--
-- 2. Function that formats a voice for Lilypond
--

CREATE FUNCTION lilypond_voice
( src text
, vox vox
, start_id int DEFAULT NULL
, max_count bigint DEFAULT NULL
, add_key  boolean DEFAULT false
, add_time boolean DEFAULT false
, add_clef boolean DEFAULT false
, add_rest boolean DEFAULT false
) RETURNS text
LANGUAGE plpgsql
SET search_path = ly2pg
AS $BODY$
DECLARE
  v_src ALIAS FOR src;
  v_vox ALIAS FOR vox;
  v_start_ord   int DEFAULT 1;
  v_start_ticks int DEFAULT 0;
  v_key text;
  v_time text;
  v_clef text;
  v_notes text;
  v_tempo tempo;
  v_clavis clavis;
  v_is_maior boolean;
  v_anacrusis int;
BEGIN

  SELECT clavis
  INTO STRICT v_clavis
  FROM pgwtc.metadata m
  WHERE m.src = v_src;

  SELECT (tempo).num, (tempo).den
  INTO STRICT v_tempo.num, v_tempo.den
  FROM pgwtc.metadata m
  WHERE m.src = v_src;

  -- NOTE: the following syntax would be preferrable:
  --
  --   SELECT tempo INTO STRICT v_tempo
  --
  -- but it errors out due to an apparent PostgreSQL bug, which should
  -- be reported and investigated.

  SELECT key, is_maior
  INTO STRICT v_key, v_is_maior
  FROM clavis2lilypond(v_clavis);
  v_time := tempo2text(v_tempo);
  v_clef := CASE
    WHEN v_vox IN ('bass', 'tenor')
    THEN 'bass'
    ELSE 'treble'
    END;
  IF start_id IS NOT NULL THEN
    SELECT
      ord
    , start
    INTO STRICT
      v_start_ord
    , v_start_ticks
    FROM pgwtc.notes n
    WHERE n.src = v_src
    AND n.vox = v_vox
    AND n.id = start_id;
  END IF;
  v_anacrusis := v_start_ticks % tempo2ticks(v_tempo);
  WITH RECURSIVE ids AS (
    SELECT id, ord
    FROM pgwtc.notes n
    WHERE n.src = v_src
    AND n.vox = v_vox
    AND ord = v_start_ord
  UNION ALL
    SELECT n.id, n.ord
    FROM ids
    JOIN pgwtc.notes n
      ON n.src = v_src
     AND n.vox = v_vox
     AND n.ord = ids.ord + 1
     AND COALESCE(n.ord < v_start_ord + max_count, true)
  ), notes_with_durations AS (
    SELECT ord
    , string_agg(lilypond(nota, f.d), ' ~ ' ORDER BY f.i) AS lilypond
    FROM ids
    JOIN pgwtc.notes n USING (id, ord)
    CROSS JOIN LATERAL unnest(n.durations) WITH ORDINALITY AS f(d, i)
    GROUP BY ord
  )
  SELECT string_agg(lilypond, ' ' ORDER BY ord)
  INTO v_notes
  FROM notes_with_durations;
  RETURN format
  ( '%s%s%s%s%s'
  , CASE WHEN add_key
    THEN format('\key %s \%s ', v_key, CASE WHEN v_is_maior THEN 'major' ELSE 'minor' END)
    ELSE '' END
  , CASE WHEN add_time
    THEN format('\time %s ', v_time)
    ELSE '' END
  , CASE WHEN add_clef
    THEN format('\clef %s ', v_clef)
    ELSE '' END
  , CASE WHEN add_rest AND v_anacrusis > 0
    THEN format('r%s ', ticks2duration(v_anacrusis))
    ELSE '' END
  , v_notes
  );
END;
$BODY$;

--
-- 3. Views exposing subjects and their occurrences
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
  , n.ticks
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
  , n.ticks
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
  , array_agg(id    ORDER BY ord) AS ids
  , array_agg(nota  ORDER BY ord) AS note
  , array_agg(ticks ORDER BY ord) AS rhythm
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
, ids
, note
, rhythm
FROM aggregated_subject_ids s
JOIN pgwtc.metadata USING (src)
JOIN first_notes USING (src, vox)
ORDER BY src, vox;

CREATE VIEW subject_occurrences AS
WITH RECURSIVE subjects_and_deltas_real AS (

  --
  -- We transpose the subjects 5 octaves in either direction, which
  -- seems more than enough.
  --
  -- TODO: add new deltas to cover for inversions
  --
  -- TODO: adjust rhythm prolations
  --

  SELECT s.src
  , s.note
  , s.rhythm
  , s.clavis
  , format('R%s', f.o) AS pattern_id
  , f.o AS delta
  , array_fill(f.o, ARRAY[array_length(note,1)]) AS deltas
  FROM pgwtc.subjects s
  , generate_series(-35,35) AS f(o)
), subjects_and_deltas_tonal AS (

  --
  -- In this query we generate all the possible "tonal adjustments",
  -- by transposing a prominent note by one grade to the tonic, when
  -- possible.
  --
  -- The pattern id "Tx/y" means that pattern Ry was tonally adjusted
  -- by moving the x-th note, e.g. T1/-4 is the same as R-4 with the
  -- first note tonally adjusted.
  --
  -- Here we transpose a prominent dominant note when the tonal answer
  -- is the subject transposed by a fifth.

  SELECT src
  , note
  , rhythm
  , clavis
  , format('T1/%s', delta) AS pattern_id
  , delta
  , int_array_shift(deltas, 1, -1)
  FROM subjects_and_deltas_real
  WHERE (delta + 35) % 7 = 4 -- transposed by a fifth
    AND note[1] @ clavis = 4 -- starting with the dominant
    AND false
  -- TODO: perhaps filter by metadata
), subjects_and_deltas AS (
  SELECT *
  FROM subjects_and_deltas_real
  UNION ALL
  SELECT *
  FROM subjects_and_deltas_tonal
), patterns_unnested AS (
  SELECT s.src
  , s.pattern_id
  , f.ord
  , s.note[ord] + f.delta AS nota_transposed
  , f.ticks
  FROM subjects_and_deltas s
  , unnest(s.rhythm, s.deltas)
    WITH ORDINALITY AS f(ticks, delta, ord)
), patterns AS (
  SELECT src
  , pattern_id
  , array_agg(nota_transposed ORDER BY ord) AS pattern_note
  , array_agg(ticks           ORDER BY ord) AS pattern_rhythm
  FROM patterns_unnested
  GROUP BY src, pattern_id
), occurrences_unnested AS (
  SELECT n.id
  , n.src
  , n.vox
  , n.ord
  , n.nota
  , n.ticks
  , 1 AS length
  , n.start
  , n.id AS first
  , p.pattern_id
  , p.pattern_note
  , array_length(p.pattern_note,1) AS max_iter
  FROM pgwtc.notes n
  JOIN patterns p USING (src)
  WHERE (n.nota).tono IS NOT DISTINCT FROM (p.pattern_note[1]).tono
UNION ALL
  SELECT n.id
  , n.src
  , n.vox
  , n.ord
  , n.nota
  , n.ticks
  , a.length + 1 AS length
  , a.start
  , a.first
  , a.pattern_id
  , a.pattern_note
  , a.max_iter
  FROM occurrences_unnested a
  JOIN pgwtc.notes n USING (src, vox)
  WHERE n.ord = a.ord + 1
  AND (n.nota).tono IS NOT DISTINCT FROM (a.pattern_note[a.length + 1]).tono
  AND length < max_iter
), occurrences AS (
  SELECT src
  , vox
  , start
  , pattern_id
  , array_agg(id    ORDER BY ord) AS ids
  , array_agg(nota  ORDER BY ord) AS note
  , array_agg(ticks ORDER BY ord) AS rhythm
  FROM occurrences_unnested
  GROUP BY src, vox, start, pattern_id
)
SELECT src
, o.vox
, o.start
, o.ids
, o.rhythm
, pattern_id
, pattern_rhythm
FROM occurrences o
JOIN patterns p USING (src, pattern_id)
WHERE array_length(o.ids,1) > greatest(3, 0.5 * array_length(pattern_note,1))
ORDER BY src, o.start;

CREATE INDEX ON notes(src);
CREATE INDEX ON notes(nota);

--
-- 4. Functions creating lilypond-book sources
--

CREATE FUNCTION lilypond_book_subjects()
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
    , pgwtc.lilypond_voice
      ( src
      , vox
      , ids[1]
      , array_length(ids, 1)
      , add_key  := true
      , add_time := true
      , add_clef := true
      , add_rest := true
      )
    FROM pgwtc.subjects
    ORDER BY src
  LOOP
    RETURN NEXT format($$
\section{%s (%s)}
\begin{lilypond}
\absolute{
  %s
}
\end{lilypond}
$$
    , x.src
    , x.vox
    , x.lilypond_voice
    );
  END LOOP;
  RETURN NEXT $$
\end{document}
$$;
END;
$BODY$;

--
-- 5. User interface (views)
--

CREATE VIEW notes_pretty AS
SELECT id
, src
, vox
, ord
, lilypond(nota)
, start @ tempo AS initio
, durations
FROM notes
JOIN metadata USING (src);

CREATE VIEW subjects_pretty AS
SELECT src
, vox
, start @ tempo AS initio
, clavis
, lilypond_voice(src, vox, ids[1], array_length(ids, 1))
FROM subjects;

CREATE VIEW subject_occurrences_pretty AS
SELECT src
, o.vox
, o.start @ m.tempo AS initio
, o.pattern_id
, lilypond_voice(src, o.vox, o.ids[1], array_length(o.ids,1))
, o.pattern_rhythm
FROM subject_occurrences o
JOIN metadata m USING (src)
ORDER BY src, o.start;
