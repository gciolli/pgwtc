# 2025-05-07

Mugellini reports the following subject entries:

| BWV | Subject entries                                  | ?   |
|-----|--------------------------------------------------|-----|
| 853 | 3 8 12 19 20 24 24 27 27 -30 -36 -39 -44 -45 -47 |     |
|     | 52 52 -54 -54 57 61 62 -64 67 67 72 77 77 77 80  |     |
| 855 | 1 3 11 13 20 22 30 32 39                         | OK* |
| 857 | 1 4 7 13 19 27 34 40 47 53                       | OK  |
| 858 | 1 3 5 11 15 20 28 31                             | OK* |
| 859 | 1 4 8 15 -20 25 29 -32 37                        | OK* |
| 861 | 1 2 5 6 12 13 15 17 17 20 21 23 28 28 29 31 33   |     |

## BWV853

- we find 1, 3, 8, 12, 19 and 20 only, because the subject is modified
  starting from 20 onwards.

## BWV855

- it finds all entries except 39, because it is truncated

## BWV858

-   it misses the entry at 11, because it is slightly simplified as
	
	```
	[cis8] cis fis eis fis eis16 dis cis4 dis4
	```

    while the original subject is:
	
	```
	[ r8 ] cis fis eis fis eis16 dis cis8. b32 cis dis4
	```

## BWV859

-   it misses the inverse entries (as expected), and the entry at 25,
    because the start is slightly altered to
	
	```
	[b8 gis a] cis fis dis e2. ~ e4 dis8 eis8 fis4 ~ fis4 ...
	```

	from the original of

	```
	[r4] fis gis a2. ~ a4 gis8 ais b4 ~ b4 ...
	```

### BWV861

-   We are missing four: 13 (bass), one of the two 17 (alto), the 23
    (alto), the 29 (bass)

-   13 is missed because the starting note is lowered by one tone:

	```
	r8 bes d f e4 f r8 g16 a bes8 a16 g a4
	```
	
-   17 is missed because the part after the rest is lowered by one
    tone:
	
	```
	r8 c d f, ees4 f r8 f16 g aes8 g16 f g8
	```

-   23 is missed because the (1) start is modified and (2) the rest is
    suppressed, as follows:

	```
	[g8] c16 d ees8 g, fis4 g4. a16 bes c8 bes16 a bes
	```

-   29 is missed because it is modified as follows:

	```
	r8 d ees g, fis4 a d8, d16' c bes8 c16 d ees2
	```

# 2025-05-04

## Notes on BWV853

Mugellini reports the following subject entries: 

# 2025-05-03

We have identified the following tonal answers:

- real: 846, 849, 855, 859, 867, 874, 878, 888, 891, 892

- dominant tonal
  - on the first note: 857, 858, 861, 866, 883, 885, 889
  - on the second note: 853, 862, 876
  - on the fifth note: 890

- subdominant tonal: 863, 864, 868

The tonal answer does not exclude the non tonal answer, i.e. we need
to add tonal answers to the other non-tonal transpositions.

> Example (BWV857). At measures 4 and 19, the subject occurs twice,
> transposed by the same grade (a fifth). Yet only the first
> occurrence is adjusted to a tonal answer by lowering the start note
> to the tonic.

## Latest Plan

- Allow multiple subjects (e.g. 883)

- Tonal answers imply that identifying subjects by computing relative
  differences is tricky

  - An alternative strategy is to encode transposition logic, and then
	compute all the 6 transpositions of each subject, so we can look
	for subjects in an absolute way, irrespective of relative
	differences

  - This would also remove the need to have rests that encode the
	memory of the last sound

  - We can even implement inversions, initially just as verbatim
    copies from the literature, until we formalize the rules. Looks
    like test-driven development.

### Tonal answers

All our code is based on referring to the `pgwtc.notes` table; this
means we cannot easily implement subjects as sequences of notes.

So we might want to implement transpositions, inversions etc. as
arrays of modifiers that are then applied on the fly while looking for
matches.

# 2025-02-03

As of today, this is the status:

| Task             | Status | Notes |
|------------------|--------|-------|
| Step 1: Tokens   | Done   |       |
| Step 2: Objects  | Done   |       |
| Step 3: Contexts | Done   |       |
| Step 4: Notes    | WIP    |       |
| ...              |        |       |

# Additional Notes

The initial assumption was that each fugue is composed exactly by the
union of its monophonic voices, in the number that is declared at the
start of the fugue.

However, there are some extra notes, mostly (but not exclusively) at
the end of the fugue, in the form of a closing chord, or as a doubled
voice.

Those extra notes can only be created with the following three tokens:

-   `<` ("clusters" of notes), 59 occurrences
-   `<<` ("polyphony" of voices), 23 occurrences
-   `q` (repeats the previous chord), 4 occurrences

for a total of 86 occurrences of tokens allowing extra notes,
affecting 37 of the 48 fugues.

The `q` token is only used in two sources, and never appears alone,
because it just replicates the preceding cluster.

> There are also 19 tokens `\\`, but they are not relevant for
> detecting additional voices, because their purpose is just to
> distinguish between temporary and permanent voices when using the
> polyphony syntax.

First, we need to decide how to handle extra notes. The available
options include:

1.  Manually overriding the text (i.e. eliminating extra notes with ad
    hoc editing, reflecting Bruno Mugellini's edition)

2.  Automatically eliminating extra notes (i.e. for each chord, select
    only the first note; for each polyphonic segment, select only the
    first voice)

3.  Automatically creating extra voices

4.  Converting chords to voices wherever it makes sense

Let's classify the 82 occurrences.

## Example 1: single cluster of two or three notes

| BWV | Voice | Measure |
|-----|-------|--------:|
| 846 | alto  |      27 |
| 850 | bass  |      22 |
| 890 | mezzo |      88 |

BWV846 is a minimal example, with just a single cluster of two notes:

```
a4 r16 f' d8 <g e>2 \fermata \bar "|." | % m. 27
```

This is just an ending chord that does not affect polyphony: any
resolution will preserve counterpoint.

BWV850 is the same, and BWV890 as well, except that it features a
three note chord.

## Example 2: multiple clusters, at the end

| BWV | Voice   |         Measures |
|-----|---------|-----------------:|
| 847 | soprano |               30 |
|     | alto    |     30,  31 (x3) |
|     | bass    |       29, 30, 31 |
| 848 | soprano |               55 |
|     | alto    |          55 (x3) |
| 861 | alto    | 33 (x3), 34 (x3) |

## Example 3: two voice polyphony, at or near the end

| BWV | Voice | Measures |
|-----|-------|---------:|
| 852 | mezzo |    36-37 |
| 854 | bass  |    26-26 |
| 869 | bass  |    74-76 |
| 877 | alto  |    45-46 |

## Example 4: doubling voices with cluster at the end

| BWV | Voice   |             Measures |
|-----|---------|---------------------:|
| 851 | mezzo   |          43 (x5), 44 |
|     | bass    |   43-44, 43 (x5), 44 |

## Example 5: polyphony mixed with clusters

| BWV | Voice   |             Measures |
|-----|---------|---------------------:|
| 860 | soprano |                   86 |
|     | mezzo   |              85 (x2) |
|     | bass    |                83-86 |
| 865 | soprano |   80, 82 (x2), 83-83 |
|     | alto    |       86-87, 87 (x3) |
|     | tenor   |                87-87 |
|     | bass    |   80, 81 (x2), 83-87 |
| 870 | soprano | 80 (x4), 81 (x2), 83 |
|     | mezzo   |                82-83 |
| 871 | alto    |        27-27, 27, 28 |
|     | bass    |                   28 |
| 872 | mezzo   |     29-30, 32-35, 35 |
|     | bass    |                32-35 |
| 879 | soprano |                86-86 |
|     | mezzo   |                   83 |
| 880 | soprano |                83-86 |
|     | mezzo   |        85-85, 86, 87 |
|     | bass    |                86-87 |
| 884 | soprano |                62-64 |
|     | mezzo   |   30-31, 30, 60 (x2) |
| 886 | alto    |                   46 |
|     | bass    |                48-50 |

### BWV851, bass, measures 43-44

```
f8 g a g a4 | % m. 42
<<
  { d,2. | d2. \fermata \bar "|."}
  \new Staff \with {
  \key d \minor
  \remove "Time_signature_engraver"
  alignAboveContext = #"bass"
  \clef bass
  } { r8 <d fis> <e g> <fis a> <g bes> <e g> | <fis a>2. \fermata }
>> | % mm. 43 and 44 
```

This example introduces an additional temporary voice which also
contains clusters of two note chords.
