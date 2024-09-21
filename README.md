# Quickstart

Create the `pgwtc` extension as follows:

    CREATE EXTENSION pgwtc;

Add the `pgwtc` schema to your search path:

    SET search_path = pgwtc, public;

Query some notes as in the following example:

    SELECT *
	FROM wtc
	WHERE BWV=871
	AND voice=2
	ORDER BY t LIMIT 9;

You should get the following output:

     bwv | clavis | tempo | voice | pitch |  t   |  d  | bar |  pos  
    -----+--------+-------+-------+-------+------+-----+-----+-------
     871 | Cm     | (4,4) |     2 |    67 |  192 | 192 |   1 | 0.500
     871 | Cm     | (4,4) |     2 |    63 |  384 | 192 |   1 | 1.000
     871 | Cm     | (4,4) |     2 |    65 |  576 | 192 |   1 | 1.500
     871 | Cm     | (4,4) |     2 |    67 |  768 | 192 |   1 | 2.000
     871 | Cm     | (4,4) |     2 |    60 |  960 | 192 |   1 | 2.500
     871 | Cm     | (4,4) |     2 |    65 | 1152 | 192 |   1 | 3.000
     871 | Cm     | (4,4) |     2 |    63 | 1344 |  96 |   1 | 3.500
     871 | Cm     | (4,4) |     2 |    62 | 1440 |  96 |   1 | 3.750
     871 | Cm     | (4,4) |     2 |    63 | 1536 | 384 |   2 | 0.000
    (9 rows)
