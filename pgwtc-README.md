# Overview

This extension includes the music from the fugues in the two books of
the [Well-Tempered Clavier], and a collection of tools which can be
used to analyse these fugues from the perspective of counterpoint.

## Well-Tempered Clavier

The Well-Tempered Clavier is a collection of 48 musical pieces by
Johann Sebastian Bach, each composed by a Prelude and a Fugue.

The 48 fugues have been transcribed for LilyPond in [Rother,
K. 2015. An 'open source' open score edition of Bach's Well-tempered
Clavier. Music score. University of Cape
Town.](hdl.handle.net/11427/13106).

The notes from these fugues were extracted using the `ly2pg` extension
into the `pgwtc-notes.csv` file, which is then loaded into the `notes`
table.

# Quickstart

## Installation

To install `pgwtc`, just build the two extensions and install them as
follows:

    sudo make install

and then you can connect to PostgreSQL as superuser and issue

    CREATE EXTENSION pgwtc CASCADE;

which will automatically install the `ly2pg` extension as a
dependency:
	
	NOTICE: installing required extension "ly2pg"
	CREATE EXTENSION

## Notes

First, query some notes as in the following example:

    SELECT *
    FROM pgwtc.notes_pretty
    WHERE src = 'BWV846'
    AND vox = 'alto'
    ORDER BY ord LIMIT 16;

You should get the following output:

     id |  src   | vox  | ord | lilypond |   start   | durations 
    ----+--------+------+-----+----------+-----------+-----------
     20 | BWV846 | alto |   1 | r        |   1:1.000 | {8}
     22 | BWV846 | alto |   2 | c'       |   1:1.500 | {8}
     24 | BWV846 | alto |   3 | d'       |   1:2.000 | {8}
     26 | BWV846 | alto |   4 | e'       |   1:2.500 | {8}
     28 | BWV846 | alto |   5 | f'       |   1:3.000 | {8.}
     30 | BWV846 | alto |   6 | g'       |   1:3.750 | {32}
     32 | BWV846 | alto |   7 | f'       |   1:3.875 | {32}
     34 | BWV846 | alto |   8 | e'       |   1:4.000 | {8}
     36 | BWV846 | alto |   9 | a'       |   1:4.500 | {8}
     40 | BWV846 | alto |  10 | d'       |   2:1.000 | {8}
     46 | BWV846 | alto |  11 | g'       |   2:1.500 | {8,16}
     48 | BWV846 | alto |  12 | a'       |   2:2.250 | {16}
     50 | BWV846 | alto |  13 | g'       |   2:2.500 | {16}
     52 | BWV846 | alto |  14 | f'       |   2:2.750 | {16}
     54 | BWV846 | alto |  15 | e'       |   2:3.000 | {16}
     56 | BWV846 | alto |  16 | f'       |   2:3.250 | {16}
    (16 rows)

## Metadata

The `pgwtc.metadata` table contains some data that was transcribed
from the literature (e.g. [1]), as in this example:

    SELECT *
    FROM pgwtc.metadata
	WHERE src = 'BWV846';

      src   | clavis | tempo | subject_length 
    --------+--------+-------+----------------
     BWV846 | C      | (4,4) |             14
    (1 row)

The number 14 refers to the fact that the subject of BWV846 is
composed by the first 14 notes, skipping the initial rest when there
is one, as in this case.

## Subjects

The `pgwtc.subjects_pretty` view exposes the subjects for each fugue
in a easier aggregate format, combined with some metadata:

    SELECT *
    FROM pgwtc.subjects_pretty
    WHERE src = 'BWV846';
    
      src   | vox  |   start   | clavis |                            lilypond_voice                             
    --------+------+-----------+--------+-----------------------------------------------------------------------
     BWV846 | alto |   1:1.500 | C      | c'8 d'8 e'8 f'8. g'32 f'32 e'8 a'8 d'8 g'8 ~ g'16 a'16 g'16 f'16 e'16
    (1 row)

The subjects are displayed in a more conventional music notation in
the `doc/pgwtc-subjects.lb.pdf` file.  If you have Lilypond installed,
you can build it automatically as follows:

    make -C doc

## Subject Occurrences

The `pgwtc.subject_occurrences_pretty` view exposes the occurrences of
the subjects for each fugue, in a similar format:

    SELECT *
    FROM pgwtc.subject_occurrences_pretty
    WHERE src = 'BWV846';

      src   |   vox   |  initio   |                                    lilypond_voice                                     
    --------+---------+-----------+---------------------------------------------------------------------------------------
     BWV846 | soprano |   2:3.500 | g'8 a'8 b'8 c''8. d''32 c''32 b'8 e''8 a'8 d''8 ~ d''16 e''16 d''16 c''16 b'16
     BWV846 | tenor   |   4:1.500 | g8 a8 b8 c'8. d'32 c'32 b8 e'8 a8 d'8 ~ d'16 e'16 d'16 c'16 b8
     BWV846 | bass    |   5:3.500 | c8 d8 e8 f8. g32 f32 e8 a8 d8 g8 ~ g16 a16 g16 f16 e16
     BWV846 | soprano |   7:1.500 | c''8 d''8 e''8 f''8. g''32 f''32 e''8 a''8 d''8 g''8 ~ g''16 a''16 g''16 f''16 e''8
     BWV846 | tenor   |   7:2.500 | g8 a8 b8 c'8. d'32 c'32 b8 e'8 a8 d'8 ~ d'16 e'16 d'16 c'16 b8
     BWV846 | alto    |   9:1.500 | g'8 a'8 b'8 c''8. d''32 c''32 b'8 e''8 a'8 d''8 ~ d''16 e''16 d''16 c''16 b'8 r8 ~ r8
     BWV846 | bass    |  10:3.500 | g,8 a,8 b,8 c8. d32 c32 b,8 e8 a,8 d8 ~ d16 e16 d16 c16 b,8
    (...)

TODO: repeat the query after properly removing short subject
fragments.

TODO: choose one between "initio" and "start".

TODO: subject occurrences should also be displayed with
`lilypond-book`

# References

-   [1]: Joh. Seb. Bach, Das wohltemperierte Klavier, Bruno Mugellini
    ausgabe, Breitkopf & Härtel, 1909
