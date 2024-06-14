\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue X"
  subtitle = "in 3 voices"
  opus = "BWV 879"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key e \minor
  \time 2/2
  \partial 4
}

soprano = \relative c' {
  \global
  
  \tuplet 3/2 { r8 e fis }
  g4-. a16 g fis g a4-. b16 a g a | % m. 1
  b4-. g-. e-. c'~ | % m. 2
  c4 b8. dis16 e4-. a,~ | % m. 3
  a4 g8. dis'16 e4-. fis,~ | % m. 4
  \tuplet 3/2 4 { fis8 [g a] \omit TupletNumber g [fis e] dis [c' b] a [g fis] } | % m. 5
  \tuplet 3/2 4 { g8 [a b] a [g fis] e [e' d] cis [ b ais] } | % m. 6
  \tuplet 3/2 { b8 [d fis] } gis4-. \tuplet 3/2 { cis,8 [e g!] } ais4-. | % m. 7
  \tuplet 3/2 { d,8 [fis ais] } b4~ \tuplet 3/2 4 { b8 [a! g] fis [e dis] } | % m. 8
  e2 d | % m. 9
  cis2 b | % m. 10
  ais4 b cis2~ | % m. 11
  cis4 fis8. ais,16 b2~ | % m. 12
  \tuplet 3/2 { b8 [g b] } cis4~ \tuplet 3/2 { cis8 [a c!] } dis4~ | % m. 13
  \tuplet 3/2 { dis8 [b dis] } e4~ e2~ | % m. 14
  \tuplet 3/2 { e8 [dis e] } fis4~ \tuplet 3/2 4 { fis8 [e d!] cis [ dis e] } | % m. 15
  \tuplet 3/2 { dis8 [e fis] } b,4~ \tuplet 3/2 4 { b8 [a g] a [b c~] } | % m. 16
  c4 \tuplet 3/2 { b8 [a g] } a4 dis,! | % m. 17
  b'2~ b8.[ e,16] dis8. [e16] | % m. 18
  c'2~ c8. [fis,16] e8. [fis16] | % m. 19
  d'2~ \tuplet 3/2 4 { d8 [g, a] b [c d] } | % m. 20
  e2~ \tuplet 3/2 4 { e8 [fis, g] a [b c] } | % m. 21
  d2~ \tuplet 3/2 4 { d8 [e, fis] g [a b] } | % m. 22
  c2~ \tuplet 3/2 4 { c8 [d, e] fis [g a] } | % m. 23
  b4-. c16 b a b c4-. d16 c b c | % m. 24
  d4-. b-. g-. e'~ | % m. 25
  e4 d8. fis16 g4-. c,~ | % m. 26
  c4 b8. fis'16 g4-. a,~ | % m. 27
  \tuplet 3/2 4 { a8 [b c] b [a g] fis [e' d] c [b a] } | % m. 28
  \tuplet 3/2 4 { b8 [c d] c [b a] g [g' fis] e [d cis] } | % m. 29
  d4 r cis r | % m. 30
  c!4 r b d | % m. 31
  g2 fis | % m. 32
  e2 d | % m. 33
  cis4 d e2~ | % m. 34
  e8. [cis16] d8. e16 fis2~ | % m. 35
  fis8. [dis16] e8. fis16 g4 cis,!~ | % m. 36
  \tuplet 3/2 4 { cis8 [d e] d [cis b] } ais'4 cis, \turn | % m. 37
  fis2. e4~ | % m. 38
  \tuplet 3/2 4 { e8 [fis e] d [cis b] } fis'2~ | % m. 39
  \tuplet 3/2 4 { fis8 [g fis] e [dis cis] } a'2~ | % m. 40
  a4 g fis2~ | % m. 41
  fis2 \omit TupletBracket \tuplet 3/2 { r8 e [gis,!] } ais4 | % m. 42
  \tuplet 3/2 {d8 [fis ais,] } b4~ b2~ | % m. 43
  \tuplet 3/2 4 { b8 [cis b] ais [gis fis] r b [a!] g! [fis e] } | % m. 44
  \tuplet 3/2 4 { ais8 [fis gis] a [b cis] d [cis,! d] e [fis g!~] } | % m. 45
  g4 \tuplet 3/2 { fis8 [e d] } e4 ais, | % m. 46
  fis'2~ fis8. [b,16] ais!8. b16 | % m. 47
  g'2~ g8. [cis,!16] b8. cis16 | % m. 48
  a'2~ a8. e16 dis!4 | % m. 49
  e4 r c'! r | % m. 50
  b4 \tuplet 3/2 4 { r8 e [d!] c [b a] gis [a b] } | % m. 51
  \tuplet 3/2 4 { e,8 [fis e] dis [e fis] b, [e d!] cis [dis e] } | % m. 52
  \tuplet 3/2 4 { dis8 [e fis] b, [c a] g [a b] c [b a] } | % m. 53
  b2 r4 dis' | % m. 54
  e2 \tuplet 3/2 4 { r8 f! [e] d [c b] } | % m. 55
  \tuplet 3/2 4 { c8 [d e] d [c b] a [b' a] g! [fis e] } | % m. 56
  \tuplet 3/2 4 { fis8 [g a] g [fis e] d [e d] c! [b a] } | % m. 57
  \tuplet 3/2 4 { b8 [c d] c [b a] } g r r4 | % m. 58
  r2 r4 \tuplet 3/2 4 { r8 a [b] } | % m. 59
  c4-. d16 c b c d4-. e16 d c d | % m. 60
  e4-. c-. a-. f'!~ | % m. 61
  f4 e8. gis16 a4-. d,~ | % m. 62
  d4 c8. gis'16 a4-. b,~ | % m. 63
  \tuplet 3/2 4 { b8 [c d] c [b a] gis [f'! e] d [c b] } | % m. 64
  \tuplet 3/2 4 { c [d e] d [c b] } a2~ | % m. 65
  a2~ \tuplet 3/2 4 { a8 [dis,! e] fis [g a] } | % m. 66
  g2~ \tuplet 3/2 4 { g8 [dis e] g [fis a] } | % m. 67
  dis,8. e16 fis4~ \tuplet 3/2 4 { fis8 [b, d] fis [b dis,] } | % m. 68
  e2~ \tuplet 3/2 4 { e8 [g ais] cis [dis e] } | % m. 69
  e,4 dis \fermata r2 | % m. 70
  \tuplet 3/2 4 { r8 b [cis] dis [e fis] g [c! b] a [g fis] } | % m. 71
  \tuplet 3/2 { e8 g' e } cis4 \tuplet 3/2 { fis,8 a' fis } dis4 | % m. 72
  \tuplet 3/2 4 { g,8 [b' g] e [d! cis] } d2 | % m. 73
  \tuplet 3/2 4 { c8 [fis, e] dis [e fis] g [a b] c [b a] } | % m. 74
  \tuplet 3/2 4 { dis8 [e fis] b, [c a] g [fis g] a [b c~] } | % m. 75
  c4 \tuplet 3/2 { b8 a g } a2~ | % m. 76
  a8. [fis16] b8. a16 g2~ | % m. 77
  \tuplet 3/2 4 { g8 [b a] g [fis e] } dis16 [c'8.~] \tuplet 3/2 { c8 e, dis } | % m. 78
  \tuplet 3/2 { b'8 e, g } cis4~ \tuplet 3/2 { cis4 a8 } dis4~ | % m. 79
  \tuplet 3/2 { dis4 b8 } e4~ \tuplet 3/2 { e4 dis8 } fis4~ | % m. 80
  \tuplet 3/2 { fis4 e8 } g4 r2 | % m. 81
  \tuplet 3/2 4 { r8 c, [fis] a [dis, fis] c [b dis] fis [a, c] } | % m. 82
  \tuplet 3/2 4 { g8 [fis a] c [fis, a] e [dis \prall \fermata dis' \turn] } e4~ | % m. 83
  \tuplet 3/2 4 { e8 [d! c] b [a g] a [fis g] a [b c~] } | % m. 84
  \tuplet 3/2 4 { c8 [b a] g [dis! e~] e [fis16 a c8] } dis,8. \prall e16 | % m. 85
  <<
    { e1 }
    \\
    { \omit TupletBracket \omit TupletNumber \tuplet 3/2 { r8 b d! } c4 b2 \bar "|." }
  >> | % m. 86
    
}

mezzo = \relative c' {
  \global
  
  r4
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  R1 | % m. 5
  r2 r4 \tuplet 3/2 { r8 b [cis] } | % m. 6
  d4-. e16 d cis d e4-. fis16 e d e | % m. 7
  fis4-. d-. b-. g'~ | % m. 8
  g4 fis8. ais16 b4-. e,~ | % m. 9
  e4 d8. ais'16 b4 cis,~ | % m. 10
  \tuplet 3/2 4 { cis8 [d e] \omit TupletNumber d [cis b] ais [g' fis] e [d cis] } | % m. 11
  \tuplet 3/2 4 { d [e fis] e [d cis] b [a'! g] fis [e dis] } | % m. 12
  e2 fis | % m. 13
  g2 \tuplet 3/2 4 { gis8 [d'! c] b [a gis] } | % m. 14
  a2 g | % m. 15
  fis2 e | % m. 16
  dis4 e fis2~ | % m. 17
  fis8. [dis16] e8. fis16 g2~ | % m. 18
  g8. [e16] fis8. g16 a2~ | % m. 19
  a8. [fis16] g8. a16 b4 r | % m. 20
  \omit TupletBracket \tuplet 3/2 4 { r8 d [c] b [a g] } a4 r | % m. 21
  \tuplet 3/2 4 { r8 c [b] a [g fis] } g4 r | % m. 22
  \tuplet 3/2 4 { r8 b [a] g [fis e] } fis4 r | % m. 23
  \tuplet 3/2 { r8 b,8 [d] } e4~ \tuplet 3/2 { e8 [c e] } fis4~ | % m. 24
  \tuplet 3/2 4 { fis8 [d fis] g [ \clef bass g, b] e [e, fis] g [a b] } | % m. 25
  c2 b | % m. 26
  a2 \tuplet 3/2 4 { r8 d, [g~] g [fis g] } | % m. 27
  fis4 g a fis | % m. 28
  g8. b16 d2 \tuplet 3/2 { \clef treble cis8 [d e] } | % m. 29
  fis4-. g16 fis e fis g4-. a16 g fis g | % m. 30
  a4-. fis-. d-. b'~ | % m. 31
  b4 a8. cis16 d4-. g,~ | % m. 32
  g4 fis8. cis'16 d4 e,~ | % m. 33
  \tuplet 3/2 4 { e8 [fis g] fis [e d] cis [b' a] g [fis e] } | % m. 34
  \tuplet 3/2 4 { fis8 [g a] g [fis e] d [c'! b] a [g fis] } | % m. 35
  \tuplet 3/2 4 { g [a b] a [g fis] e [d'! cis] b [ais gis] } | % m. 36
  ais4 b cis! cis~ | % m. 37
  \tuplet 3/2 4 { cis8 [e d] cis [b ais] } b4 cis | % m. 38
  fis,2 r4 fis | % m. 39
  b2 cis4 \tuplet 3/2 { dis!8 e fis } | % m. 40
  b,4 e~ \tuplet 3/2 4 { e8 [e d] cis [b ais] } | % m. 41
  \tuplet 3/2 { b8 d b } gis4 cis2~ | % m. 42
  cis4 b~ \tuplet 3/2 4 { b8 [a! g] fis [e dis] } | % m. 43
  e2 d | % m. 44
  cis2 b | % m. 45
  ais4 b cis2~ | % m. 46
  cis8. [ais16] b8. cis16 d2~ | % m. 47
  d8. [b16] cis8. d16 e2~ | % m. 48
  e8. [cis16] dis8. e16 fis4~ \tuplet 3/2 { fis8 e fis } | % m. 49
  g4 a16 g fis g a4 b16 a g a | % m. 50
  b4-. g-. e-. c'~ | % m. 51
  c4 b8. dis16 e4-. a,~ | % m. 52
  a4 g8. dis'16 e4-. fis,~ | % m. 53
  \tuplet 3/2 4 { fis8 [g a] g [fis e] dis [c' b] a [g fis] } | % m. 54
  \tuplet 3/2 4 { g [a b] a [g fis] } e2~ | % m. 55
  e8. [fis16~] fis8. gis16 a4 e~ | % m. 56
  e8. [d16~] d8. cis16 d2~ | % m. 57
  d8. [e16~] e8. fis16 \tuplet 3/2 4 { g8 [a g] f! [e d] } | % m. 58
  \tuplet 3/2 4 { e8 [f! g] f [e d] c [d c] b [a gis] } | % m. 59
  a2~ \tuplet 3/2 4 { a8 [ \clef bass d, f!] } gis4 | % m. 60
  \tuplet 3/2 4 { r8 c, [d] e [fis gis] } a r r4 | % m. 61
  \clef treble d'2 c | % m. 62
  b2 a | % m. 63
  gis8. [gis,16] a8. c16 \tuplet 3/2 4 { b8 [d! c] b [a gis] } | % m. 64
  a8 r r4 \tuplet 3/2 4 { r8 g' [fis] e [dis cis] } | % m. 65
  \tuplet 3/2 4 { dis8 [e fis] e [dis cis] } b2~ | % m. 66
  b8. [b16] c8. b16 ais4 a!~ | % m. 67
  \tuplet 3/2 4 { a8 [c b] a [g fis] } g2~ | % m. 68
  g4 r g~ \tuplet 3/2 { g8 fis g~ } | % m. 69
  g4 fis \fermata r2 | % m. 70
  R1 | % m. 71
  R1 | % m. 72
  r2 \tuplet 3/2 4 { r8 b' [gis] e [fis gis] } | % m. 73
  a2 g | % m. 74
  fis2 e | % m. 75
  dis4 e fis2~ | % m. 76
  fis8. [dis16] b8. dis16 e2~ | % m. 77
  e4 r \tuplet 3/2 4 { r8 \clef bass dis,8 [e] fis [g a] } | % m. 78
  \tuplet 3/2 { g8 \clef treble e'4~ } e4~ \tuplet 3/2 { e8 fis4~ } fis4~ | % m. 79
  \tuplet 3/2 { fis8 g4~ } g4~ \tuplet 3/2 { g8 a4~ } a4~ | % m. 80
  \tuplet 3/2 { a8 b4~ } b4 r2 | % m. 81
  R1 | % m. 82
  r2 <fis, a>4 r8 r16 fis' | % m. 83
  g4~ \tuplet 3/2 { g8 fis e } dis8. [e16] fis8. e16 | % m. 84
  dis8 r \tuplet 3/2 { r8 \clef bass fis, [g] } fis4~ \tuplet 3/2 { fis8 [g a!~] } | % m. 85
  \tuplet 3/2 { a8 gis4~ gis8 [gis a~] } a4 gis \fermata \bar "|." | % m. 86
    
}

bass = \relative c {
  \global
  
  r4
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  R1 | % m. 5
  R1 | % m. 6
  R1 | % m. 7
  R1 | % m. 8
  R1 | % m. 9
  R1 | % m. 10
  R1 | % m. 11
  r2 r4 \tuplet 3/2 { r8 e fis } | % m. 12
  g4-. a16 g fis g a4-. b16 a g a | % m. 13
  b4-. g-. e-. c'~ | % m. 14
  c4 b8. dis16 e4-. a,~ | % m. 15
  a4 g8. dis'16 e4-. fis,~ | % m. 16
  \tuplet 3/2 4 { fis8 [g a] \omit TupletNumber g [fis e] dis [c' b] a [g fis] } | % m. 17
  \tuplet 3/2 4 { g8 [a b] a [g fis] e [d'! c] b [ a g] } | % m. 18
  \tuplet 3/2 4 { a8 [b c] b [a g] fis [e' d] c [b a] } | % m. 19
  \tuplet 3/2 4 { b8 [c d] c [b a] g [b d] } g4 | % m. 20
  c,4 e \tuplet 3/2 { fis,8 a c } fis4 | % m. 21
  b,4 d \tuplet 3/2 { e,8 g b } e4 | % m. 22
  a,4 c \tuplet 3/2 { d,8 fis a } d4 | % m. 23
  g,2 a | % m. 24
  b4 r r2 | % m. 25
  \omit TupletBracket \tuplet 3/2 4 { r8 a [g] fis [g a] r g [fis] e [fis g] } | % m. 26
  \tuplet 3/2 4 { r8 fis [e] d [e c] } b4 cis | % m. 27
  d2 r4 d | % m. 28
  g4~ g8. fis16 e4 a | % m. 29
  \tuplet 3/2 { d,8 fis a } b4 \tuplet 3/2 { e,8 g b } cis4 | % m. 30
  \tuplet 3/2 { fis,8 g a } d4 \tuplet 3/2 4 { r8 c [b] a [g fis] } | % m. 31
  \tuplet 3/2 4 { e8 [g b] cis [e a,] d, [d' cis] b [a g] } | % m. 32
  \tuplet 3/2 4 { cis,8 [e g] a [cis fis,] b, [b' a] g [fis e] } | % m. 33
  a2 r4 cis | % m. 34
  d2 r4 dis | % m. 35
  e2 r4 eis | % m. 36
  fis2~ \tuplet 3/2 4 { fis8 [g fis] e [d cis] } | % m. 37
  d2~ \tuplet 3/2 4 { d8 [e d] cis [b ais] } | % m. 38
  b2~ \tuplet 3/2 4 { b8 [c! b] a [g fis] } | % m. 39
  g2~ \tuplet 3/2 4 { g8 [a g] fis [e dis] } | % m. 40
  \tuplet 3/2 4 { e8 [fis e] d! [cis b] ais [fis gis] ais [b cis] } | % m. 41
  d4-. e16 d cis d e4-. fis16 e d e | % m. 42
  fis4-. d-. b-. g'~ | % m. 43
  g4 fis8. ais16 b4-. e,~ | % m. 44
  e4 d8. ais'16 b4 cis,~ | % m. 145
  \tuplet 3/2 4 { cis8 [d e] d [cis b] ais [g' fis] e [d cis] } | % m. 46
  \tuplet 3/2 4 { d [e fis] e [d cis] b [a'! g] fis [e d] } | % m. 47
  \tuplet 3/2 4 { e8 [fis g] fis [e d] cis [b' a] g [fis e] } | % m. 48
  \tuplet 3/2 4 { fis8 [g a] g [fis e] dis [c'! b] a [g fis] } | % m. 49
  \tuplet 3/2 { e8 g b } cis4-. \tuplet 3/2 { fis,8 a c! } dis4-. | % m. 50
  \tuplet 3/2 { g,8 b dis } e4~ \tuplet 3/2 4 { e8 [d! c] b [a gis] } | % m. 51
  a2 g | % m. 52
  fis2 e | % m. 53
  dis4 e fis b, | % m. 54
  e4 fis g gis | % m. 55
  a4 b c cis | % m. 56
  d,4 e fis d | % m. 57
  g4 a b g | % m. 58
  \tuplet 3/2 4 { c,8 [d e] d [e f!] e [gis a] } d,4 | % m. 59
  \tuplet 3/2 { c8 d e } f!4 b,2 | % m. 60
  a4 r \tuplet 3/2 4 { r8 b' [c] d [e f!] } | % m. 61
  \tuplet 3/2 4 { gis,8 [a b] c [d e] fis, [gis a] b [c a] } | % m. 62
  \tuplet 3/2 4 { gis8 [fis e] a [b c] f,! [a g!] f [g e] } | % m. 63
  d8. [f!16] e8. dis16 e2 | % m. 64
  a,8. [e'16] a8. b16 c8. [a16] fis8. a16 | % m. 65
  \tuplet 3/2 4 { b,8 [cis dis] cis [fis e] dis [a' g] fis [e dis] } | % m. 66
  \tuplet 3/2 4 { e8 [fis g] fis [e d!] } cis4 c! | % m. 67
  b1~ | % m. 68
  \tuplet 3/2 4 { b8 [cis dis] e [fis g] } ais,2 | % m. 69
  b2 \tuplet 3/2 4 { r8 c [b] a [g fis] } | % m. 70
  \tuplet 3/2 4 { g8 [a g] fis [e dis] } e4 \tuplet 3/2 { r8 e fis } | % m. 71
  g4-. a16 g fis g a4-. b16 a g a | % m. 72
  b4-. g-. e-. c'~ | % m. 73
  c4 b8. dis16 e4-. a,~ | % m. 74
  a4 g8. dis'16 e4-. fis,~ | % m. 75
  \tuplet 3/2 4 { fis8 [g a] g [fis e] dis [c' b] a [g fis] } | % m. 76
  \tuplet 3/2 4 { g8 [a b] a [g fis] e [e' d] c [b ais] } | % m. 77
  b1~ | % m. 78
  b4~ \tuplet 3/2 { b8 ais' b } b,4~ \tuplet 3/2 { b8 ais' b } | % m. 79
  b,4~ \tuplet 3/2 { b8 ais' b } b,4~ \tuplet 3/2 { b8 ais' b } | % m. 80
  b,4~ \tuplet 3/2 4 { b8 [ais' b] fis [g dis] e [b c] } | % m. 81
  dis,4 r r2 | % m. 82
  r2 c'4 r8 r16 ais' | % m. 83
  b1~ | % m. 84
  \tuplet 3/2 { b8 [cis dis] } \tuplet 3/2 { e8 r ais,, } b2 | % m. 85
  e2~ \tuplet 3/2 { e8 dis e } e,4 \fermata \bar "|." | % m. 86
    
}

\paper {
  max-systems-per-page = 5
}

\score {
  \new StaffGroup
  <<
    \new Staff = "soprano"
      \soprano
    
    \new Staff = "mezzo" 
      \mezzo
    
    \new Staff = "bass"
      { \clef bass \bass }
      
  >>
  
\layout {
  indent = 0.0
  }

}
