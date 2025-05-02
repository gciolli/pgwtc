--
-- Please see ly2pg-README.md for more information on this script.
--

\set ON_ERROR_STOP 1

DROP EXTENSION IF EXISTS ly2pg CASCADE;
CREATE EXTENSION ly2pg;

--SET search_path = ly2pg, public;

--
-- 1. Load all the source files into a sequence of tokens.
--

\set cnt `cat ly/voces/fuga-bwv846-alto.ly`
CALL ly2pg.tokenize('BWV846', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv846-bass.ly`
CALL ly2pg.tokenize('BWV846', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv846-soprano.ly`
CALL ly2pg.tokenize('BWV846', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv846-tenor.ly`
CALL ly2pg.tokenize('BWV846', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv847-alto.ly`
CALL ly2pg.tokenize('BWV847', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv847-bass.ly`
CALL ly2pg.tokenize('BWV847', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv847-soprano.ly`
CALL ly2pg.tokenize('BWV847', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv848-alto.ly`
CALL ly2pg.tokenize('BWV848', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv848-bass.ly`
CALL ly2pg.tokenize('BWV848', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv848-soprano.ly`
CALL ly2pg.tokenize('BWV848', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv849-alto.ly`
CALL ly2pg.tokenize('BWV849', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv849-bass.ly`
CALL ly2pg.tokenize('BWV849', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv849-mezzo.ly`
CALL ly2pg.tokenize('BWV849', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv849-soprano.ly`
CALL ly2pg.tokenize('BWV849', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv849-tenor.ly`
CALL ly2pg.tokenize('BWV849', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv850-alto.ly`
CALL ly2pg.tokenize('BWV850', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv850-bass.ly`
CALL ly2pg.tokenize('BWV850', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv850-soprano.ly`
CALL ly2pg.tokenize('BWV850', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv850-tenor.ly`
CALL ly2pg.tokenize('BWV850', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv851-bass.ly`
CALL ly2pg.tokenize('BWV851', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv851-mezzo.ly`
CALL ly2pg.tokenize('BWV851', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv851-soprano.ly`
CALL ly2pg.tokenize('BWV851', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv852-bass.ly`
CALL ly2pg.tokenize('BWV852', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv852-mezzo.ly`
CALL ly2pg.tokenize('BWV852', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv852-soprano.ly`
CALL ly2pg.tokenize('BWV852', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv853-bass.ly`
CALL ly2pg.tokenize('BWV853', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv853-mezzo.ly`
CALL ly2pg.tokenize('BWV853', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv853-soprano.ly`
CALL ly2pg.tokenize('BWV853', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv854-bass.ly`
CALL ly2pg.tokenize('BWV854', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv854-mezzo.ly`
CALL ly2pg.tokenize('BWV854', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv854-soprano.ly`
CALL ly2pg.tokenize('BWV854', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv855-bass.ly`
CALL ly2pg.tokenize('BWV855', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv855-soprano.ly`
CALL ly2pg.tokenize('BWV855', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv856-bass.ly`
CALL ly2pg.tokenize('BWV856', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv856-mezzo.ly`
CALL ly2pg.tokenize('BWV856', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv856-soprano.ly`
CALL ly2pg.tokenize('BWV856', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv857-alto.ly`
CALL ly2pg.tokenize('BWV857', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv857-bass.ly`
CALL ly2pg.tokenize('BWV857', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv857-soprano.ly`
CALL ly2pg.tokenize('BWV857', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv857-tenor.ly`
CALL ly2pg.tokenize('BWV857', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv858-bass.ly`
CALL ly2pg.tokenize('BWV858', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv858-mezzo.ly`
CALL ly2pg.tokenize('BWV858', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv858-soprano.ly`
CALL ly2pg.tokenize('BWV858', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv859-alto.ly`
CALL ly2pg.tokenize('BWV859', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv859-bass.ly`
CALL ly2pg.tokenize('BWV859', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv859-soprano.ly`
CALL ly2pg.tokenize('BWV859', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv859-tenor.ly`
CALL ly2pg.tokenize('BWV859', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv860-bass.ly`
CALL ly2pg.tokenize('BWV860', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv860-mezzo.ly`
CALL ly2pg.tokenize('BWV860', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv860-soprano.ly`
CALL ly2pg.tokenize('BWV860', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv861-alto.ly`
CALL ly2pg.tokenize('BWV861', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv861-bass.ly`
CALL ly2pg.tokenize('BWV861', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv861-soprano.ly`
CALL ly2pg.tokenize('BWV861', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv861-tenor.ly`
CALL ly2pg.tokenize('BWV861', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv862-alto.ly`
CALL ly2pg.tokenize('BWV862', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv862-bass.ly`
CALL ly2pg.tokenize('BWV862', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv862-soprano.ly`
CALL ly2pg.tokenize('BWV862', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv862-tenor.ly`
CALL ly2pg.tokenize('BWV862', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv863-alto.ly`
CALL ly2pg.tokenize('BWV863', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv863-bass.ly`
CALL ly2pg.tokenize('BWV863', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv863-soprano.ly`
CALL ly2pg.tokenize('BWV863', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv863-tenor.ly`
CALL ly2pg.tokenize('BWV863', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv864-bass.ly`
CALL ly2pg.tokenize('BWV864', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv864-mezzo.ly`
CALL ly2pg.tokenize('BWV864', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv864-soprano.ly`
CALL ly2pg.tokenize('BWV864', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv865-alto.ly`
CALL ly2pg.tokenize('BWV865', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv865-bass.ly`
CALL ly2pg.tokenize('BWV865', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv865-soprano.ly`
CALL ly2pg.tokenize('BWV865', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv865-tenor.ly`
CALL ly2pg.tokenize('BWV865', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv866-bass.ly`
CALL ly2pg.tokenize('BWV866', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv866-mezzo.ly`
CALL ly2pg.tokenize('BWV866', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv866-soprano.ly`
CALL ly2pg.tokenize('BWV866', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv867-alto.ly`
CALL ly2pg.tokenize('BWV867', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv867-bass.ly`
CALL ly2pg.tokenize('BWV867', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv867-mezzo.ly`
CALL ly2pg.tokenize('BWV867', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv867-soprano.ly`
CALL ly2pg.tokenize('BWV867', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv867-tenor.ly`
CALL ly2pg.tokenize('BWV867', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv868-alto.ly`
CALL ly2pg.tokenize('BWV868', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv868-bass.ly`
CALL ly2pg.tokenize('BWV868', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv868-soprano.ly`
CALL ly2pg.tokenize('BWV868', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv868-tenor.ly`
CALL ly2pg.tokenize('BWV868', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv869-alto.ly`
CALL ly2pg.tokenize('BWV869', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv869-bass.ly`
CALL ly2pg.tokenize('BWV869', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv869-soprano.ly`
CALL ly2pg.tokenize('BWV869', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv869-tenor.ly`
CALL ly2pg.tokenize('BWV869', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv870-bass.ly`
CALL ly2pg.tokenize('BWV870', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv870-mezzo.ly`
CALL ly2pg.tokenize('BWV870', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv870-soprano.ly`
CALL ly2pg.tokenize('BWV870', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv871-alto.ly`
CALL ly2pg.tokenize('BWV871', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv871-bass.ly`
CALL ly2pg.tokenize('BWV871', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv871-soprano.ly`
CALL ly2pg.tokenize('BWV871', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv871-tenor.ly`
CALL ly2pg.tokenize('BWV871', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv872-bass.ly`
CALL ly2pg.tokenize('BWV872', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv872-mezzo.ly`
CALL ly2pg.tokenize('BWV872', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv872-soprano.ly`
CALL ly2pg.tokenize('BWV872', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv873-bass.ly`
CALL ly2pg.tokenize('BWV873', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv873-mezzo.ly`
CALL ly2pg.tokenize('BWV873', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv873-soprano.ly`
CALL ly2pg.tokenize('BWV873', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv874-alto.ly`
CALL ly2pg.tokenize('BWV874', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv874-bass.ly`
CALL ly2pg.tokenize('BWV874', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv874-soprano.ly`
CALL ly2pg.tokenize('BWV874', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv874-tenor.ly`
CALL ly2pg.tokenize('BWV874', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv875-bass.ly`
CALL ly2pg.tokenize('BWV875', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv875-mezzo.ly`
CALL ly2pg.tokenize('BWV875', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv875-soprano.ly`
CALL ly2pg.tokenize('BWV875', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv876-alto.ly`
CALL ly2pg.tokenize('BWV876', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv876-bass.ly`
CALL ly2pg.tokenize('BWV876', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv876-soprano.ly`
CALL ly2pg.tokenize('BWV876', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv876-tenor.ly`
CALL ly2pg.tokenize('BWV876', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv877-alto.ly`
CALL ly2pg.tokenize('BWV877', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv877-bass.ly`
CALL ly2pg.tokenize('BWV877', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv877-soprano.ly`
CALL ly2pg.tokenize('BWV877', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv877-tenor.ly`
CALL ly2pg.tokenize('BWV877', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv878-alto.ly`
CALL ly2pg.tokenize('BWV878', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv878-bass.ly`
CALL ly2pg.tokenize('BWV878', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv878-soprano.ly`
CALL ly2pg.tokenize('BWV878', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv878-tenor.ly`
CALL ly2pg.tokenize('BWV878', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv879-bass.ly`
CALL ly2pg.tokenize('BWV879', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv879-mezzo.ly`
CALL ly2pg.tokenize('BWV879', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv879-soprano.ly`
CALL ly2pg.tokenize('BWV879', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv880-bass.ly`
CALL ly2pg.tokenize('BWV880', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv880-mezzo.ly`
CALL ly2pg.tokenize('BWV880', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv880-soprano.ly`
CALL ly2pg.tokenize('BWV880', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv881-bass.ly`
CALL ly2pg.tokenize('BWV881', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv881-mezzo.ly`
CALL ly2pg.tokenize('BWV881', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv881-soprano.ly`
CALL ly2pg.tokenize('BWV881', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv882-bass.ly`
CALL ly2pg.tokenize('BWV882', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv882-mezzo.ly`
CALL ly2pg.tokenize('BWV882', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv882-soprano.ly`
CALL ly2pg.tokenize('BWV882', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv883-bass.ly`
CALL ly2pg.tokenize('BWV883', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv883-mezzo.ly`
CALL ly2pg.tokenize('BWV883', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv883-soprano.ly`
CALL ly2pg.tokenize('BWV883', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv884-bass.ly`
CALL ly2pg.tokenize('BWV884', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv884-mezzo.ly`
CALL ly2pg.tokenize('BWV884', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv884-soprano.ly`
CALL ly2pg.tokenize('BWV884', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv885-alto.ly`
CALL ly2pg.tokenize('BWV885', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv885-bass.ly`
CALL ly2pg.tokenize('BWV885', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv885-soprano.ly`
CALL ly2pg.tokenize('BWV885', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv885-tenor.ly`
CALL ly2pg.tokenize('BWV885', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv886-alto.ly`
CALL ly2pg.tokenize('BWV886', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv886-bass.ly`
CALL ly2pg.tokenize('BWV886', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv886-soprano.ly`
CALL ly2pg.tokenize('BWV886', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv886-tenor.ly`
CALL ly2pg.tokenize('BWV886', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv887-bass.ly`
CALL ly2pg.tokenize('BWV887', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv887-mezzo.ly`
CALL ly2pg.tokenize('BWV887', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv887-soprano.ly`
CALL ly2pg.tokenize('BWV887', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv888-bass.ly`
CALL ly2pg.tokenize('BWV888', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv888-mezzo.ly`
CALL ly2pg.tokenize('BWV888', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv888-soprano.ly`
CALL ly2pg.tokenize('BWV888', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv889-bass.ly`
CALL ly2pg.tokenize('BWV889', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv889-mezzo.ly`
CALL ly2pg.tokenize('BWV889', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv889-soprano.ly`
CALL ly2pg.tokenize('BWV889', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv890-bass.ly`
CALL ly2pg.tokenize('BWV890', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv890-mezzo.ly`
CALL ly2pg.tokenize('BWV890', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv890-soprano.ly`
CALL ly2pg.tokenize('BWV890', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv891-alto.ly`
CALL ly2pg.tokenize('BWV891', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv891-bass.ly`
CALL ly2pg.tokenize('BWV891', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv891-soprano.ly`
CALL ly2pg.tokenize('BWV891', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv891-tenor.ly`
CALL ly2pg.tokenize('BWV891', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv892-alto.ly`
CALL ly2pg.tokenize('BWV892', 'alto', :'cnt');
\set cnt `cat ly/voces/fuga-bwv892-bass.ly`
CALL ly2pg.tokenize('BWV892', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv892-soprano.ly`
CALL ly2pg.tokenize('BWV892', 'soprano', :'cnt');
\set cnt `cat ly/voces/fuga-bwv892-tenor.ly`
CALL ly2pg.tokenize('BWV892', 'tenor', :'cnt');
\set cnt `cat ly/voces/fuga-bwv893-bass.ly`
CALL ly2pg.tokenize('BWV893', 'bass', :'cnt');
\set cnt `cat ly/voces/fuga-bwv893-mezzo.ly`
CALL ly2pg.tokenize('BWV893', 'mezzo', :'cnt');
\set cnt `cat ly/voces/fuga-bwv893-soprano.ly`
CALL ly2pg.tokenize('BWV893', 'soprano', :'cnt');

--
-- 2. We temporarily skip some fugues which present complexities not
--    related to our main goal, which is counterpoint analysis rather
--    than parsing complex LilyPond syntax.
--
--    The idea is that we can start by analysing the remaining fugues,
--    and develop the analysis tools against them; later we will
--    extend the analysis to the fugues we skipped in the first place.
--

--
-- (2.I) fugues that have multiple sub-voices inside a single voice,
--       i.e. using "<<" and ">>"
--

DELETE FROM ly2pg.tokenized_all WHERE src IN
( 'BWV851'
, 'BWV852'
, 'BWV854'
, 'BWV860'
, 'BWV865'
, 'BWV869'
, 'BWV870'
, 'BWV871'
, 'BWV872'
, 'BWV877'
, 'BWV879'
, 'BWV880'
, 'BWV884'
, 'BWV886'
);

--
-- (2.II) fugues that present anacrusis (\partial)
--

DELETE FROM ly2pg.tokenized_all WHERE src IN
( 'BWV856'
, 'BWV879'
, 'BWV881'
, 'BWV882'
, 'BWV893'
);

--
-- (2.III) fugues that present "~" right after ">"
--

DELETE FROM ly2pg.tokenized_all WHERE src IN
( 'BWV847'
);

--
-- (2.IV) fugues that use \grace
--

DELETE FROM ly2pg.tokenized_all WHERE src IN
( 'BWV848'
, 'BWV850'
, 'BWV873'
, 'BWV884'
, 'BWV887'
, 'BWV893'
);

--
-- (2.V) fugues that use \tuplet
--

DELETE FROM ly2pg.tokenized_all WHERE src IN
( 'BWV850'
, 'BWV875'
, 'BWV879'
);

--
-- 3. Process all the tokens, extracting voices as sequences of notes.
--

DO $$
DECLARE
  s text;
  v ly2pg.vox;
BEGIN
  FOR s, v IN
    SELECT DISTINCT src, vox
    FROM ly2pg.tokenized
    ORDER BY src, vox
  LOOP
    CALL ly2pg.build_objects  (s,v);
    CALL ly2pg.detect_contexts(s,v);
    CALL ly2pg.extract_notes  (s,v);
  END LOOP;
END;
$$ LANGUAGE plpgsql;

\copy (TABLE ly2pg.notes) TO 'pgwtc-notes.csv' CSV HEADER
