\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue V"
  subtitle = "in 4 voices"
  composer = "BWV 850"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key d \major
  \time 4/4
  \override TupletBracket.bracket-visibility = ##f
  \omit TupletNumber
}

soprano = \relative c'' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  r4 a32 b cis d cis b cis a fis'8. fis16 e8. d16 | % m. 5
  cis8. \tuplet 3/2 { e32 fis g } a8. a16 a8. \tuplet 3/2 { d,32 e fis } g8. g16 | % m. 6
  g4 fis8. e16 d8. e16 cis8. cis16 | % m. 7
  cis4 b32 cis d e d cis d b g'8. g16 fis8. e16 | % m. 8
  d4 b'16 a b gis gis fis gis e e d e cis \noBreak | % m. 9
  \grace d16 (cis4) a'16 g! a fis fis e fis d d c! d b | % m. 10
  b4 g32 a b c! b a b g e'8. e16 d8. c16 | % m. 11
  b8. cis!16 d2 cis4 | % m. 12
  d4 d32 e fis g fis e fis d b'8. b16 a8. g16 | % m. 13
  fis8. \tuplet 3/2 { d32 e fis } g8. g16 g8. a16 fis8. fis16 | % m. 14
  fis8. e32 dis e4~ e8. dis16 e8. fis16 | % m. 15
  b,8. c!32 a b8. c16 g4 fis | % m. 16
  e32 fis g a g fis g e g'8 r r2 \noBreak | % m. 17
  a,32 b cis d cis b cis a fis'8 r r2 | % m. 18
  g,32 a b c! b a b g e'8 r r2 \noBreak | % m. 19
  fis,32 g a b a g a fis d'8. d16 d8. e16 cis8.\prall d32 e | % m. 20
  fis4 b16 a b g g fis g e e d e cis | % m. 21
  cis4 d8. d16 d8. e16 cis8. \downprall d16 | % m. 22
  d4 b32 cis d e d cis d b e4 a,32 b cis d cis b cis a | % m. 23
  d4 g,32 a b c! b a b g cis32 d e fis e d e cis fis g a b a g a fis | % m. 24
  b8. cis,16 d8. e16 fis8. fis,16 g8. a16 | % m. 25
  b8. b16 cis8. d16 fis,4 e | % m. 26
  d1 \fermata \bar "|." | % m. 27
    
}

alto = \relative c' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  r4 d32 e fis g fis e fis d b'8. b16 a8. g16 | % m. 4
  fis8. gis16 a4~ a gis | % m. 5
  a4 cis d d | % m. 6
  cis8. \tuplet 3/2 { e32 d cis} d8. cis16 b4 e, | % m. 7
  a4 b~ b ais | % m. 8
  b4 r d8. b16 b8. a16 | % m. 9
  a4 r c!8. a16 a8. g16 | % m. 10
  g4 r r2 | % m. 11
  r4 d32 e fis g fis e fis d b'8. b16 a8. g16 | % m. 12
  fis8. a16 d4~ d c!~ | % m. 13
  c4 b a2 | % m. 14
  g4 b a2~ | % m. 15
  a4 g8. e16 e4 dis | % m. 16
  e4 r e8. cis16 cis8. a16 | % m. 17
  a4 r d8. b16 b8. g16 | % m. 18
  g4 r c8. a16 a8. fis16 | % m. 19
  fis4 b32 cis d e d cis d b g'2 | % m. 20
  fis4 r d'8. b16 b8. a16 | % m. 21
  a4 a8. a16 b8. b16 a4 | % m. 22
  a4 r b r | % m. 23
  a4 r r2 | % m. 24
  r8. a16 a8. cis16 d8. d,16 d8. fis16 | % m. 25
  g8. d16 e8. fis16 d4 cis | % m. 26
  a1 \fermata  \bar "|." | % m. 27
  
}

tenor = \relative c' {
  \global
  
  R1 | % m. 1
  r4 a32 b cis d cis b cis a fis'8. fis16 e8. d16 | % m. 2
  cis4 fis8. fis16 b,8. \tuplet 3/2 { b32 cis d } e16 d e cis | % m. 3
  a4 d~ d cis | % m. 4
  d4 cis b2 | % m. 5
  a4 fis'32 g a b a g a fis b4 e,32 fis g a g fis g e | % m. 6
  a4 a8. g16 fis8. g16 a4 | % m. 7
  d,4 d cis2 | % m. 8
  b4 r b'8. gis16 gis8. e16 | % m. 9
  e4 r a8. fis16 fis8. d16 | % m. 10
  d4 r r2 | % m. 11
  R1 | % m. 12
  R1 | % m. 13
  r4 g,32 a b c! b a b g e'8. e16 d8. c16 | % m. 14
  b4 g' fis2~ | % m. 15
  fis4 e8. \clef bass fis,16 g8. a16 b8. a16 | % m. 16
  g4 r b8. g16 g8. e16 | % m. 17
  e4 r a8. fis16 fis8. d16 | % m. 18
  d4 r g8. e16 e8. cis16 | % m. 19
  cis4 r r a'32 b cis d cis b cis a | % m. 20
  d4 r \clef treble b'8. g16 g8. e16 | % m. 21
  e4 fis8. fis16 g8. g16 e4 | % m. 22
  fis4 r e4 r | % m. 23
  d4 r r2 | % m. 24
  r8. e16 fis8. g16 a8. a,16 b8. c!16 | % m. 25
  d8. g16 g8. a16 a,4 g | % m. 26
  fis1 \fermata \bar "|." | % m. 27
    
}

bass = \relative c {
  \global
  
  r4 d32 e fis g fis e fis d b'8. b16 a8. g16 | % m. 1
  fis8. g16 fis8. e16 d4 e | % m. 2
  a,8. \tuplet 3/2 { e'32 fis g } a2 g4~ | % m. 3
  g8. fis32 e fis8. d16 g8. e16 a8. a,16 | % m. 4
  d8. e16 fis8. e16 d8. b16 e4 | % m. 5
  a,4 r r2 | % m. 6
  r4 d,32 e fis g fis e fis d b'8. b16 a8. g16 | % m. 7
  fis8. \tuplet 3/2 { d'32 e fis } g8. fis16 e8. cis16 fis4 | % m. 8
  b,32 cis d e d cis d b gis'8 r r2 | % m. 9
  a,32 b cis d cis b cis a fis'8 r r2 | % m. 10
  g,32 a b c! b a b g e'8. d16 c4 d | % m. 11
  g8. a16 b8. a16 g8. e16 a8. a,16 | % m. 12
  b32 cis d e d cis d b b'8. a16 g8. e16 a8. a,16 | % m. 13
  d4 e c'!8. a16 d8. d,16 | % m. 14
  g4 e,32 fis g a g fis g e c'!8. c16 b8. a16 | % m. 15
  g8. fis16 g8. a16 b2 | % m. 16
  e4 b'16 a b g g fis g e e d e cis | % m. 17
  cis4 a'16 g a fis fis e fis d d cis d b | % m. 18
  b4 g'16 fis g e e d e cis cis b cis a | % m. 19
  a4 r e'32 fis g a g fis g e a4 | % m. 20
  d,32 e fis g fis e fis d g8 r r2 | % m. 21
  <g g,>4 fis32 g a g fis e d cis b cis d cis b a b g a8. a16 | % m. 22
  d,8. \tuplet 3/2 { d'32 e fis } g4 cis,32 d e fis e d e cis fis4 | % m. 23
  b,32 cis d e d cis d b e fis g a g fis g e a b cis d cis b cis a d e fis g fis e fis d | % m. 24
  g8. g,16 fis8. e16 d8. c!16 b8. a16 | % m. 25
  g8. fis 16 e8. d16 a'2 | % m. 26
  d,1 \fermata \bar "|." | % m. 27
    
}

\score {
  \new StaffGroup
  <<
    \new Staff = "soprano" 
      \soprano
    
     \new Staff = "alto" 
      \alto
    
    \new Staff = "tenor" 
     \tenor
    
    \new Staff = "bass"
      { \clef bass \bass }
  
  >>
  
\layout {
  indent = 0.0
  }
 
}
