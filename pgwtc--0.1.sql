-- complain if script is sourced in psql, rather than via CREATE EXTENSION
\echo Use "CREATE EXTENSION pgwtc" to load this file. \quit

--
-- 1. Load data and metadata for the Well-Tempered Clavier
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

CREATE TABLE notes (LIKE ly2pg.notes INCLUDING INDEXES INCLUDING CONSTRAINTS);
COPY notes FROM '/usr/share/postgresql/17/extension/pgwtc-notes.csv' CSV HEADER;

--
-- 3. Function that formats a voice for Lilypond
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
LANGUAGE plpgsql AS $BODY$
DECLARE
  v_src ALIAS FOR src;
  v_vox ALIAS FOR vox;
  v_start_ord   int DEFAULT 1;
  v_start_ticks int DEFAULT 0;
  v_key text;
  v_time text;
  v_clef text;
  v_notes text;
  v_tempo ly2pg.tempo;
  v_clavis ly2pg.clavis;
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

  v_key := ly2pg.clavis2lilypond(v_clavis);
  v_time := ly2pg.tempo2text(v_tempo);
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
  v_anacrusis := v_start_ticks % ly2pg.tempo2ticks(v_tempo);
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
    , string_agg(ly2pg.lilypond(nota, f.d), ' ~ ' ORDER BY f.i) AS lilypond
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
    THEN format('\key %s ', v_key)
    ELSE '' END
  , CASE WHEN add_time
    THEN format('\time %s ', v_time)
    ELSE '' END
  , CASE WHEN add_clef
    THEN format('\clef %s ', v_clef)
    ELSE '' END
  , CASE WHEN add_rest AND v_anacrusis > 0
    THEN format('r%s ', ly2pg.ticks2duration(v_anacrusis))
    ELSE '' END
  , v_notes
  );
END;
$BODY$;

--
-- 4. Views exposing subjects and their occurrences
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
  , array_agg(ticks ORDER BY ord) AS ticks
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
, ticks
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

--
-- 5. User interface (views)
--

CREATE VIEW notes_pretty AS
SELECT id
, src
, vox
, ord
, lilypond(nota)
, start @ tempo AS start
, durations
FROM notes
JOIN metadata USING (src);

CREATE VIEW subjects_pretty AS
SELECT src
, vox
, start @ tempo AS start
, clavis
, lilypond_voice(src, vox, ids[1], array_length(ids, 1))
FROM subjects;

CREATE VIEW subject_occurrences_pretty AS
SELECT
  src
, vox
, initio @ tempo AS initio
, lilypond_voice(src, vox, ids[1], array_length(ids, 1))
FROM subject_occurrences o
JOIN pgwtc.metadata USING (src)
ORDER BY src, o.initio;

--
-- 6. Functions creating lilypond-book sources
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
