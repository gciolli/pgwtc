-- complain if script is sourced in psql, rather than via CREATE EXTENSION
\echo Use "CREATE EXTENSION ly2pg" to load this file. \quit
--
-- This code parses a .ly file and loads its notes in a PostgreSQL
-- table.
--
-- It is expected to work on all the files contained in the ly/voces/
-- subdirectory [1], but it should also work on several other Lilypond
-- sources.
--
-- [1] The files in the ly/voces/ directory are the individual voices
--     for each fugue in the Well-Tempered Clavier, extracted from [2]
--     using LilyPond's \displayLilyMusic command.
--
--     This command produces a simpler format by expanding LilyPond
--     macros and converting relative pitches to absolute ones.
--
--     We also use LilyPond to transpose each piece to the simplest
--     corresponding key, which is either C major or A minor.
--
-- [2] The files in the ly/ directory are copied from the Open Score
--     edition of the Well-Tempered Clavier, prepared by Kyle Rother
--     with the support of the Centre for Innovation in Learning and
--     Teaching at the University of Cape Town.
--

CREATE TYPE vox AS ENUM ('soprano', 'alto', 'mezzo', 'tenor', 'bass');

CREATE TABLE tokenized_all
( id serial PRIMARY KEY
, src text NOT NULL
, vox vox NOT NULL
, token text NOT NULL
, matched text NOT NULL
, args text[]
);

COMMENT ON TABLE tokenized_all IS

'This table includes the sequence of all the tokens extracted from the
input. The whole data captured by the regular expressions is retained;
we will filter and organize it when we will transfer it to the
"objectified" table.';

CREATE TABLE objectified
( id int REFERENCES tokenized_all (id)
, src text NOT NULL
, vox vox NOT NULL
, obj jsonb NOT NULL
);

COMMENT ON TABLE objectified IS

'This table includes the sequence of all the music objects extracted
from the tokenized input. There are two differences with the
"tokenized" table: (1) the unnecessary data is filtered out, and (2)
the data that is kept is organized in JSON objects, with meaningful
keys.';

CREATE TYPE note_name AS enum ('R', 'r', 'c', 'd', 'e', 'f', 'g', 'a', 'b');

CREATE TYPE note AS
( name note_name
, alt int
, oct int
);

CREATE TYPE nota AS
( tono int
, alt int
);

COMMENT ON TYPE nota IS $$
5 <= tono <= 55, corresponding to the 52 white keys on the piano, with
middle C being 7*4 = 28.

-2 <= alt <= 2, corresponding to the amount of semitones that the
alteration applies.
$$;

CREATE FUNCTION nota(jsonb)
RETURNS nota
LANGUAGE sql AS
$FUNC$
SELECT CASE
WHEN substr($1 ->> 'note', 1, 1) IN ('r', 'R', 's') THEN
NULL :: nota
ELSE ROW
( CASE substr($1 ->> 'note', 1, 1)
  WHEN 'c' THEN 0
  WHEN 'd' THEN 1
  WHEN 'e' THEN 2
  WHEN 'f' THEN 3
  WHEN 'g' THEN 4
  WHEN 'a' THEN 5
  WHEN 'b' THEN 6
  END
+ 7
* CASE substr($1 ->> 'note', 2)
  WHEN ',,,,' THEN 0
  WHEN ',,,' THEN 1
  WHEN ',,' THEN 2
  WHEN ',' THEN 3
  WHEN '' THEN 4
  WHEN '''' THEN 5
  WHEN '''''' THEN 6
  WHEN '''''''' THEN 7
  WHEN '''''''''' THEN 8
  END
, CASE $1 ->> 'alteration'
  WHEN 'eses' THEN -2
  WHEN 'es' THEN -1
  WHEN 'is' THEN 1
  WHEN 'isis' THEN 2
  ELSE 0
  END
) :: nota END
$FUNC$;

CREATE FUNCTION lilypond(nota)
RETURNS text
LANGUAGE SQL AS
$$
SELECT CASE WHEN ($1).tono > 127 THEN 'r' ELSE
format('%s%s%s'
, CASE ($1).tono % 7
  WHEN 0 THEN 'c'
  WHEN 1 THEN 'd'
  WHEN 2 THEN 'e'
  WHEN 3 THEN 'f'
  WHEN 4 THEN 'g'
  WHEN 5 THEN 'a'
  WHEN 6 THEN 'b'
  END
, CASE ($1).alt
  WHEN  2 THEN 'isis'
  WHEN  1 THEN 'is'
  WHEN  0 THEN ''
  WHEN -1 THEN 'es'
  WHEN -2 THEN 'eses'
  END
, CASE ($1).tono / 7
  WHEN 0 THEN ',,,,'
  WHEN 1 THEN ',,,'
  WHEN 2 THEN ',,'
  WHEN 3 THEN ','
  WHEN 4 THEN ''
  WHEN 5 THEN ''''
  WHEN 6 THEN ''''''
  WHEN 7 THEN ''''''''
  WHEN 8 THEN ''''''''''
  END
) END
$$;

CREATE FUNCTION lilypond(nota[])
RETURNS text
LANGUAGE SQL AS
$$
SELECT string_agg(ly2pg.lilypond(ROW(tono, alt) :: ly2pg.nota), ' ')
FROM unnest($1) AS f(tono, alt)
$$;

CREATE FUNCTION semitono(nota)
RETURNS int
LANGUAGE sql AS
$$
SELECT CASE nota.tono % 7
WHEN 0 THEN 0
WHEN 1 THEN 2
WHEN 2 THEN 3
WHEN 3 THEN 5
WHEN 4 THEN 7
WHEN 5 THEN 8
WHEN 6 THEN 10
END + nota.alt + (nota.tono / 7) * 12
$$;

CREATE FUNCTION is_rest(nota)
RETURNS boolean
LANGUAGE SQL AS $$
SELECT ($1).tono IS NULL OR ($1).tono > 127
$$;

CREATE FUNCTION parse_note (text)
RETURNS note STRICT
LANGUAGE SQL
AS $$
SELECT ROW
( CAST (substr($1, 1, 1) AS note_name)
, CASE substr($1, 2)
  WHEN 'eses' THEN -2
  WHEN 'es' THEN -1
  WHEN '' THEN 0
  WHEN 'is' THEN 1
  WHEN 'isis' THEN 2
  END
, NULL :: int
) :: note
$$;

CREATE FUNCTION nota_sub(nota, nota)
RETURNS int
LANGUAGE SQL AS $$
SELECT CASE WHEN ly2pg.is_rest($1)
THEN NULL
ELSE ($1).tono - (($2).tono % 128)
END
$$;

COMMENT ON FUNCTION nota_sub(nota,nota) IS

'This function works on both tones and rests, as it is aware of the
encoding of rests that remembers the pitch of the previous note.';

CREATE OPERATOR -
( FUNCTION = nota_sub
, LEFTARG = nota
, RIGHTARG = nota
);

--
-- The "tempo" data type
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

--
-- The "clavis" data type
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

CREATE UNLOGGED TABLE claves
( id clavis PRIMARY KEY
, maior boolean NOT NULL
, diesis boolean NOT NULL
, o int NOT NULL
);

COPY claves FROM '/usr/share/postgresql/17/extension/ly2pg-claves.csv' CSV HEADER;

--
-- The "notes" table
--

CREATE TABLE notes
( id int PRIMARY KEY REFERENCES tokenized_all (id)
, src text NOT NULL
, vox vox NOT NULL
, ord int NOT NULL
, nota nota
, start int NOT NULL
, ticks int NOT NULL
, durations text[] NOT NULL
);

COMMENT ON TABLE notes IS

'This table includes the notes extracted from the music objects';

CREATE TABLE context
( id serial PRIMARY KEY
, kind text NOT NULL
, begins_at int REFERENCES tokenized_all (id)
, ends_at   int REFERENCES tokenized_all (id)
);

COMMENT ON TABLE context IS

'This table includes the contexts extracted from the tokenized input';

CREATE OR REPLACE VIEW tokenized AS
SELECT id, src, vox, token, matched, args
, jsonb_build_object
  ( 'token', token
  , 'args', args
  ) AS obj
FROM ly2pg.tokenized_all
WHERE token NOT IN ('BLANK', 'COMMENT', 'BAR', '[', ']', '(', ')');

COMMENT ON VIEW tokenized IS

'This view exposes the subset of the "tokenized_all" table that is
relevant for counterpoint analysis';

CREATE TABLE tokens
( ord int
, token text
, regexp text NOT NULL
, PRIMARY KEY (ord, token)
);

COPY tokens FROM '/usr/share/postgresql/17/extension/ly2pg-tokens.txt';

COMMENT ON TABLE tokens IS

'This table contains the definition of the available token types';

CREATE FUNCTION int_error(text)
RETURNS int
LANGUAGE plpgsql
AS $$
BEGIN
  RAISE EXCEPTION '%', $1;
  RETURN NULL;
END;
$$;

CREATE FUNCTION duration2ticks(text)
RETURNS int
LANGUAGE SQL AS $$
SELECT CASE $1
WHEN '64' THEN   6
WHEN '32' THEN  12
WHEN '16' THEN  24
WHEN  '8' THEN  48
WHEN  '4' THEN  96
WHEN  '2' THEN 192
WHEN  '1' THEN 384

WHEN '64.' THEN   6
WHEN '32.' THEN  12 +   6
WHEN '16.' THEN  24 +  12
WHEN  '8.' THEN  48 +  24
WHEN  '4.' THEN  96 +  48
WHEN  '2.' THEN 192 +  96
WHEN  '1.' THEN 384 + 192

WHEN '64..' THEN   6
WHEN '32..' THEN  12 +   6
WHEN '16..' THEN  24 +  12 +  6
WHEN  '8..' THEN  48 +  24 + 12
WHEN  '4..' THEN  96 +  48 + 24
WHEN  '2..' THEN 192 +  96 + 48
WHEN  '1..' THEN 384 + 192 + 96

WHEN '\breve' THEN 384 * 2

ELSE int_error(format('invalid duration <%s>', $1))
END $$;

CREATE PROCEDURE extract_notes(v_src text, v_vox vox)
LANGUAGE plpgsql
AS $BODY$
DECLARE
--  context_id int;
  context_ids int[] := '{}';
  context_kinds text[] := '{}';

  c SCROLL CURSOR (s text, v vox) FOR
    SELECT id, obj
    FROM ly2pg.objectified
    WHERE src = v_src
      AND vox = v_vox
    ORDER BY id;

  x1 record;
  x2 record;
  x3 record;
  x4 record;
  x5 record;
  x6 record;

  -- Variables capturing Lilypond state
  absolute_pitch_mode boolean;
  key_note note;
  key_major boolean;
  time_num int;
  time_den int;
  current_note_start int := 0;
  current_note_ord int := 1;
  current_note nota;
  previous_note nota;
  previous_note_id int := NULL;
  note_durations text[] := '{}';
  ticks int := 0;
BEGIN
  RAISE DEBUG 'CP400 extract_notes';

  FOR x IN c (v_src, v_vox) LOOP

    CASE

    --
    -- (I) strings
    --

    WHEN x.obj ->> 0 = '|'
    THEN
      CONTINUE;

    WHEN x.obj ->> 0 IN ('{', '<', '<<')
    THEN
      context_ids   := array_append(context_ids, x.id);
      context_kinds := array_append(context_kinds, x.obj ->> 0);

    WHEN x.obj ->> 0 in ('}', '>', '>>')
    THEN
      context_ids   := trim_array(context_ids,   1);
      context_kinds := trim_array(context_kinds, 1);

    --
    -- (II) notes
    --

    WHEN x.obj ? 'note'
    THEN
      CONTINUE WHEN context_kinds != ARRAY['{'];
      current_note   := nota(x.obj);
      IF current_note IS NULL THEN
        current_note := ROW
        ( (previous_note).tono + 128
        , (previous_note).alt
        ) :: nota;
      ELSE
        previous_note := current_note;
      END IF;
      note_durations := note_durations     || (x.obj ->> 'duration');
      ticks          := ticks + duration2ticks(x.obj ->> 'duration');
      FETCH c INTO x1;
      MOVE PRIOR FROM c;
      IF x1.obj ->> 0 = '~'
      THEN
        -- skip tie symbol
        MOVE NEXT FROM c;
      ELSIF 
        (
          x.obj  ->> 'note' IN ('R', 'r')
          AND
          x1.obj ->> 'note' IN ('R', 'r')
        )
      THEN
        -- implicitly tie rests
        NULL;
      ELSE
        -- do not tie; emit the note instead
        INSERT INTO notes
        ( id
        , src
        , vox
        , ord
        , nota
        , start
        , ticks
        , durations
        ) VALUES
        ( x.id
        , v_src
        , v_vox
        , current_note_ord
        , current_note
        , current_note_start
        , ticks
        , note_durations
        );
        COMMIT;
        current_note_start := current_note_start + ticks;
        current_note_ord := current_note_ord + 1;
        previous_note_id := x.id;
        note_durations := '{}';
        ticks := 0;
      END IF;

    --
    -- (III) keys with 0 arguments
    --

    WHEN x.obj ->> 'key' IN ('once', 'noBeam', 'fermata', 'noBreak', 'pageBreak')
    THEN
      CONTINUE;

    WHEN x.obj ->> 'key' = 'absolute'
    THEN
      absolute_pitch_mode := true;

    --
    -- (IV) keys with 1 arguments
    --

    WHEN x.obj ->> 'key' IN ('bar', 'clef', 'revert')
    THEN
      MOVE NEXT FROM c;
      CONTINUE;

    WHEN x.obj ->> 'key' = 'time'
    -- example: \time 4/4
    THEN
      FETCH c INTO x1;
      time_num := CAST (x1.obj ->> 0 AS int);
      time_den := CAST (x1.obj ->> 1 AS int);
      -- TODO: use time_num,time_den

    --
    -- (V) keys with 2 arguments
    --

    WHEN x.obj ->> 'key' = 'key'
    -- example: \key c \major
    THEN
      FETCH c INTO x1;
      FETCH c INTO x2;
      key_note := parse_note(x1.obj ->> 'note');
      key_major := x2.obj ->> 'key' = 'major';

    --
    -- (VI) keys with 3 arguments
    --

    WHEN x.obj ->> 'key' IN ('set', 'override')
    THEN
      FETCH c INTO x1;
      FETCH c INTO x2;
      FETCH c INTO x3;
      CONTINUE;

    --
    -- (VII) Safely skipping certain rehearsal marks:
    --

    WHEN x.obj ->> 'key' = 'mark'
    THEN
      FETCH c INTO x1; -- \markup
      FETCH c INTO x2; -- \line
      FETCH c INTO x3; -- {
      FETCH c INTO x4; -- \musicglyph
      FETCH c INTO x5; -- "scripts.dfermata" or "scripts.ufermata"
      FETCH c INTO x6; -- }
      IF    x1.obj ->> 'key' = 'markup'
        AND x2.obj ->> 'key' = 'line'
        AND x3.obj ->> 0     = '{'
        AND x4.obj ->> 'key' = 'musicglyph'
        AND x5.obj ->> 'str' IN ('scripts.dfermata', 'scripts.ufermata')
        AND x6.obj ->> 0     = '}'
      THEN
        CONTINUE;
      ELSE
        RAISE EXCEPTION 'CP450 UNSUPPORTED [% %] %\n% | % | % | % | % | %'
        , v_src, v_vox, x.obj
        , x1.obj
        , x2.obj
        , x3.obj
        , x4.obj
        , x5.obj
        , x6.obj
        ;
      END IF;
      
    ELSE
      RAISE EXCEPTION 'CP499 ELSE [% %] %', v_src, v_vox, x.obj;

    END CASE;
  END LOOP;
END;
$BODY$;

COMMENT ON PROCEDURE extract_notes IS

'This procedure populates the "notes" table using data from tables
"contexts" and "objectified"';

CREATE PROCEDURE detect_contexts(v_src text, v_vox vox)
LANGUAGE plpgsql
AS $BODY$
DECLARE
  i int;
  o jsonb;
  t text;
  --
  context_id int;
  context_ids int[];
  context_kinds text[];
  matching_token text;
BEGIN
  RAISE DEBUG 'CP300 detect_contexts';
  FOR i, o, t IN
    SELECT id, obj, obj ->> 0
    FROM ly2pg.objectified m
    WHERE m.src = v_src
      AND m.vox = v_vox
    ORDER BY m.id
  LOOP
    CASE

    WHEN t IN ('{', '<', '<<')
    THEN
      INSERT INTO ly2pg.context(kind, begins_at)
        VALUES (t, i)
        RETURNING id INTO STRICT context_id;
      context_ids   := array_append(context_ids, context_id);
      context_kinds := array_append(context_kinds, t);

    WHEN t in ('}', '>', '>>')
    THEN
      matching_token := context_kinds[array_length(context_kinds, 1)];
      ASSERT (matching_token = '{'  AND t = '}' )
        OR   (matching_token = '<'  AND t = '>' )
        OR   (matching_token = '<<' AND t = '>>')
        , 'incorrect context nesting';
      UPDATE ly2pg.context
        SET ends_at = i
        WHERE id = context_ids[array_length(context_ids, 1)];
      context_ids   := trim_array(context_ids,   1);
      context_kinds := trim_array(context_kinds, 1);

    ELSE
      NULL;

    END CASE;
  END LOOP;

  ASSERT context_kinds = '{}',
    format('voice %s from %s ends with unbalanced parentheses', v_vox, v_src);
END;
$BODY$;

COMMENT ON PROCEDURE detect_contexts IS

'This procedure lists all the contexts, recording their start and ends,
and verifies that they are not incorrectly nested';

CREATE PROCEDURE build_objects(v_src text, v_vox vox)
LANGUAGE plpgsql
AS $BODY$
DECLARE
  x ly2pg.tokenized;
BEGIN
  RAISE DEBUG 'CP200 build_objects';
  FOR x IN
    SELECT *
    FROM ly2pg.tokenized m
    WHERE m.src = v_src
      AND m.vox = v_vox
    ORDER BY m.id
  LOOP
    CASE

    --
    -- Boolean tokens
    --

    WHEN x.token IN ('FALSE', 'TRUE')
    THEN
      INSERT INTO objectified(id, src, vox, obj)
      SELECT x.id
      , v_src
      , v_vox
      , CAST (lower(x.token) AS jsonb)
      ;

    --
    -- Number tokens
    --

    WHEN x.token IN ('INT')
    THEN
      INSERT INTO objectified(id, src, vox, obj)
      SELECT x.id
      , v_src
      , v_vox
      , CAST (x.args[1] AS jsonb)
      ;

    --
    -- Tokens whose object is the homonymous untyped string
    --

    WHEN x.token IN ('<', '>', '{', '}', '<<', '>>', '~', '=')
    THEN
      INSERT INTO objectified(id, src, vox, obj)
      SELECT x.id
      , v_src
      , v_vox
      , CAST (format('"%s"', x.token) AS jsonb)
      ;

    --
    -- The object for a note records alteration and duration
    --

    WHEN x.token IN ('NOTE')
    THEN
      INSERT INTO objectified(id, src, vox, obj)
      SELECT x.id
      , v_src
      , v_vox
      , jsonb_strip_nulls
        ( jsonb_build_object
          ( 'note'      , x.args[1] || coalesce(x.args[3], '')
          , 'alteration', x.args[2]
          , 'duration'  , x.args[5]
          )
        )
      ;

    --
    -- Multi-measure rest
    --

    WHEN x.token IN ('MREST')
    THEN
      INSERT INTO objectified(id, src, vox, obj)
      SELECT x.id
      , v_src
      , v_vox
      , jsonb_strip_nulls
        ( jsonb_build_object
          ( 'note'      , 'R'
          , 'duration'  , x.args[1]
          , 'scale_num' , x.args[2]
          , 'scale_den' , x.args[3]
          )
        )
      ;

    --
    -- Tokens whose object is the homonymous typed string
    --

    WHEN x.token IN ('KEY', 'ID', 'STR', 'REF')
    THEN
      INSERT INTO objectified(id, src, vox, obj)
      SELECT x.id
      , v_src
      , v_vox
      , jsonb_build_object(lower(x.token), x.args[1])
      ;

    --
    -- The object for TIME is an array of two integers
    --

    WHEN x.token IN ('TIME')
    THEN
      INSERT INTO objectified(id, src, vox, obj)
      SELECT x.id
      , v_src
      , v_vox
      , jsonb_build_array
        ( x.args[1]
        , x.args[2]
        )
      ;

    --
    -- We need SCM to avoid breaking the syntax of \override
    --

    WHEN x.token IN ('SCM')
    THEN
      INSERT INTO objectified(id, src, vox, obj)
      SELECT x.id
      , v_src
      , v_vox
      , jsonb_build_object('#', x.args[1])
      ;

    --
    -- Tokens that are ignored
    --

    WHEN x.token IN ('CHORD', '\\')
    THEN
      NULL;

    --
    -- All the tokens that were not captured above (there should be
    -- none)
    --

    ELSE
      INSERT INTO objectified(id, src, vox, obj)
      SELECT x.id
      , v_src
      , v_vox
      , jsonb_strip_nulls
        ( jsonb_build_object
          ( 'todo'  , 'true'
          , 'token' , x.token
          , 'args'  , x.args
          )
        )
      NULL;

    END CASE;
  END LOOP;
END;
$BODY$;

COMMENT ON PROCEDURE build_objects IS

'This procedure populates the "objectified" table with the content
extracted from the "tokenized" view, grouping a sequence of tokens
into a single object whenever necessary';

CREATE PROCEDURE tokenize (v_src text, v_vox vox, v_cnt text)
LANGUAGE plpgsql
AS $BODY$
DECLARE
  n int := length(v_cnt);
  i int := 1;
  t text;
  r text;
  m text[];
  x text;
  MAX_MATCH int := 200;
BEGIN
  RAISE DEBUG 'CP100 tokenize';
  LOOP
    x := substr(v_cnt,i,MAX_MATCH);
    EXIT WHEN x = '';
    FOR t, r IN
      SELECT token, regexp FROM ly2pg.tokens
    LOOP
      m := regexp_match(x, '^(' || r || ')');
      EXIT WHEN m IS NOT NULL;
    END LOOP;
    ASSERT m IS NOT NULL, format(E'unmatched code:\n%s', x);
    i := i + length(m[1]);
    INSERT INTO ly2pg.tokenized_all(src, vox, token, matched, args)
    VALUES (v_src, v_vox, t, m[1], m[2:]);
    COMMIT;
  END LOOP;
END;
$BODY$;

COMMENT ON PROCEDURE tokenize IS

'This procedure populates the "tokenized" table with the content
extracted from the text input';

CREATE PROCEDURE process
( src text
, vox vox
, cnt text
) LANGUAGE plpgsql
AS $BODY$
BEGIN
  RAISE NOTICE 'Processing %:%', src, vox;
  --
  -- Step 1. We split the input into a sequence of tokens.
  --
  CALL ly2pg.tokenize (src, vox, cnt);
  --
  -- Step 2. We process the sequence of tokens and build a sequence of
  --         JSON objects that include only the relevant data,
  --         properly tagged.
  --
  CALL ly2pg.build_objects (src, vox);
  --
  -- Step 3. We identify and record the various kinds of contexts
  --         (groups) being used in the input.
  --
  CALL ly2pg.detect_contexts (src, vox);
  --
  -- Step 4. We process the sequence of JSON objects, and build a
  --         sequence of musical notes (and rests) for each voice.
  --
  CALL ly2pg.extract_notes (src, vox);
END;
$BODY$;

COMMENT ON PROCEDURE process IS

'This is the main procedure for processing Lilypond input and
populating ly2pg tables';
