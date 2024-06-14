BEGIN;

CREATE TYPE voce AS enum('soprano', 'alto', 'mezzo', 'tenor', 'bass');

CREATE TEMP TABLE checkin
( id serial primary key
, bwv int
, cnt text
);

CREATE TABLE voci
( bwv int
, v voce
, rel text
, cnt text
, PRIMARY KEY (bwv,v)
);

CREATE PROCEDURE ingest_words(bwv int, ws text[])
LANGUAGE plpgsql
AS $BODY$
BEGIN
  INSERT INTO voci(bwv,v,rel,cnt)
  VALUES (bwv, CAST(ws[5] AS voce), ws[2], ws[1]);
END;
$BODY$;

CREATE FUNCTION parse_ly(bwv int, x text)
RETURNS SETOF text
LANGUAGE plpgsql
AS $BODY$
DECLARE
  i int;
  n int := length(x);
  db int := 0;
  dq int := 0;
  c text;
  a text := '';
  ws text[] := '{}';
BEGIN
  FOR i IN 1..n LOOP
    c := substr(x,i,1);
    CASE c
      WHEN '{' THEN db := db + 1;
      WHEN '}' THEN db := db - 1;
      WHEN '"' THEN dq := (dq + 1) % 2;
      ELSE NULL;
    END CASE;
    IF c = ' ' AND db = 0 THEN
      ws := a || ws;
      ws := ws[1:5];
      RAISE DEBUG 'x[%] = %', i, a;
      RAISE DEBUG 'ws = %', ws;
      IF ws[5] IN ('soprano', 'alto', 'mezzo', 'tenor', 'bass') THEN
        CALL ingest_words(bwv, ws);
      END IF;
      a := '';
    ELSE
      a := a || c;
    END IF;
  END LOOP;
  RETURN;
END;
$BODY$;

\timing on

\copy checkin(bwv, cnt) FROM PROGRAM 'bash lib/ly2pg.sh ly' DELIMITER '@'

SELECT parse_ly(bwv, cnt) FROM checkin
--WHERE bwv = 871
;

\timing off

--
-- Sanity check #1
--

DO $BODY$
BEGIN

  ASSERT (
    SELECT count(*) = 0
    FROM voci
    WHERE NOT cnt ~ $_$^{ \\global .+ \\bar "|." | }$$_$
  );

END;
$BODY$ LANGUAGE plpgsql;

--
-- Cleaning #1
--

--UPDATE voci SET cnt = substr(cnt, 11, length(cnt) - 12);
UPDATE voci SET cnt = regexp_replace(cnt, '  +', ' ', 'g');
--UPDATE voci SET cnt = regexp_replace(cnt, $$\\barx "|."$$, '', 'g');
UPDATE voci SET cnt = regexp_replace(cnt, $$[a-g]!?([0-9]*)\rest$$, $$r\1$$, 'g');

SELECT bwv, v
, substr(cnt,1,16)
, substr(cnt,length(cnt)-13,18)
FROM voci
ORDER BY 4, bwv, v;

--
-- Sanity check #2
--

--SELECT DISTINCT w
--FROM voci, regexp_split_to_table(cnt, ' ') AS f(w)
--ORDER BY w;

--SELECT cnt
--FROM voci;

ROLLBACK;

-- 0556930577
