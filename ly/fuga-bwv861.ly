\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue XVI"
  subtitle = "in 4 voices"
  opus = "BWV 861"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key g \minor
  \time 4/4
}

soprano = \relative c'' {
  \global
  
  R1 | % m. 1
  r2 r8 g bes d, | % m. 2
  cis4 d r8 e!16 f g8 f16 e | % m. 3
  f8 g16 a bes8 a16 g a8 bes16 c d8 c16 bes | % m. 4
  c4 bes a8 d16 c! bes8 c16 d | % m. 5
  es8 c a fis' g4 r | % m. 6
  r8 e,!8 a f d4 cis8 e~ | % m. 7
  e8 d g4~ g16 fis g a bes4~ | % m. 8
  bes4 a~ a16 d, e! fis g4~ | % m. 9
  g8 fis16 g a8 g16 fis g4. c8 | % m. 10
  f,8 g16 a bes2 a4 | % m. 11
  bes4 r r2 | % m. 12
  R1 | % m. 13
  R1 | % m. 14
  r8 c d f, e!4 f | % m. 15
  r8 g16 a bes8 a16 g a8 bes16 c d8 c16 bes | % m. 16
  c8 d16 c bes8 c16 d es8 f16 es d8 es16 f | % m. 17
  g8 es c a' bes r r4 | % m. 18
  r8 g16 as bes8 as16 g as8 as16 g f8 g16 as | % m. 19
  d,8 g4 f16 es d4. c16 b! | % m. 20
  c4 b!8 d g, g' as c, | % m. 21
  b!4 c r8 d16 es f8 es16 d | % m. 22
  es16 d es f g8 r r d16 c bes8 c16 d | % m. 23
  es16 d c bes a8 fis' g r r16 d c bes | % m. 24
  a4~ a16 bes c d g,4~ g16 bes as g | % m. 25
  f4~ f16 g as bes es,4~ es16 g f es | % m. 26
  d4~ d16 d  e! fis g4~ g16 bes a g | % m. 27
  fis8 d' es g, fis4 g | % m. 28
  r8 a16 bes c8 bes16 a d8 r r4 | % m. 29
  r2 r8 d16 es f!8 es16 d | % m. 30
  es4~ es16 d c bes a4 r8 es'16 d | % m. 31
  c8 d16 es d8 e!16 fis g8 fis16 g a4~ | % m. 32
  a8 d, g f es d c bes | % m. 33
  a2 g \fermata \bar "|." | % m. 34
  
}

alto = \relative c' {
  \global
  
  r8 d es g, fis4 g | % m. 1
  r8 a16 bes c8 bes16 a bes4 g | % m. 2
  r8 a16 g f8 g16 a bes8 g e! cis' | % m. 3
  \clef treble d4. e!8 f e f g | % m. 4
  a8 g16 fis g8 cis, d a'4 g16 fis | % m. 5
  g4 fis8 a d4 r | % m. 6
  R1 | % m. 7
  R1 | % m. 8
  R1 | % m. 9
  R1 | % m. 10
  R1 | % m. 11
  r8 f, g bes, a4 bes | % m. 12
  r8 c16 d es8 d16 c d e! d e f8 e16 d | % m. 13
  g4. f16 e! f4 e8 bes'~ | % m. 14
  bes8 a4 g16 a bes8 a16 g a4~ | % m. 15
  a8 g16 f e!8 c~ c d16 es f8. g16 | % m. 16
  a4 g r8 c d f, | % m. 17
  es4 f r8 f16 g as8 g16 f | % m. 18
  g8 r r4 r8 f'16 es d8 es16 f | % m. 19
  b,!8 es16 d c8 d16 es f,8 g16 f es8 f16 g | % m. 20
  as8 f d b'! c4 r8 es, | % m. 21
  d8 es16 f g8 a!16 b! c8 b16 c d8 g,~ | % m. 22
  g8 c16 d es8 g, fis4 g~ | % m. 23
  g8 a16 bes c8 bes16 a bes d, e! fis g4~ | % m. 24
  g16 g f! es! d4~ d16 bes c d es4~ | % m. 25
  es16 es d c bes4~ bes16 g a! b! c4~ | % m. 26
  c16 c bes! a g4~ g16 g a bes c8 cis | % m. 27
  d4 r r8 d'16 c bes8 c16 d | % m. 28
  es8 d c es a, r r4 | % m. 29
  r8 fis16 g a8 g16 fis g2~ | % m. 30
  g2 r8 d' es g, | % m. 31
  fis4 g r8 a16 bes c8 bes16 a | % m. 32
  bes4. <g b!>8 <a c> r <d, g> r | % m. 33
  <es g>4 <d fis> <d g>2 \fermata \bar "|." | % m. 34
  
}

tenor = \relative c' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  R1 | % m. 5
  r2 r8 g bes d, | % m. 6
  cis4 d r8 e!16 f g8 f16 e | % m. 7
  f8 g16 a bes8 a16 g a8 bes16 c d8 c16 bes | % m. 8
  c2. bes8 es! | % m. 9
  a,16 bes a bes c8 bes16 a bes8 c16 d es4~ | % m. 10
  es4 d c2 | % m. 11
  d4 r r8 f,16 es d8 es16 f | % m. 12
  g8 es c a' bes2~ | % m. 13
  bes8 c16 bes a8 bes16 c d8 bes g e'! | % m. 14
  f8. es!16 d4 c4. d16 c | % m. 15
  bes2 a4 r16 bes c d | % m. 16
  es8 f16 es d8 g,~ g f bes as | % m. 17
  g4 a!8 c f, r r4 | % m. 18
  R1 | % m. 19
  R1 | % m. 20
  R1 | % m. 21
  R1 | % m. 22
  R1 | % m. 23
  R1 | % m. 24
  R1 | % m. 25
  R1 | % m. 26
  R1 | % m. 27
  r2 r8 d' es g, | % m. 28
  fis4 g r8 a16 bes c8 bes16 a | % m. 29
  bes2~ bes8 b!16 a g8 a16 b | % m. 30
  c4 r r2 | % m. 31
  R1 | % m. 32
  r8 d es g, fis4 g | % m. 33
  r8 a16 bes c8 bes16 a b!2 \fermata \bar "|." | % m. 34
  
}

bass = \relative c {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  r8 d es g, fis4 g | % m. 5
  r8 a16 bes c8 bes16 a bes4 g | % m. 6
  r8 a16 g f8 g16 a bes8 g e! cis'! | % m. 7
  d1 | % m. 8
  e!8 a16 g fis8 g16 a bes,8 c16 d es8 d16 c | % m. 9
  d2 g,4. a8 | % m. 10
  bes8 f' g d es d16 es f8 f, | % m. 11
  bes4 r r2 | % m. 12
  r2 r8 bes d f, | % m. 13
  e!4 f r8 g16 a bes8 a16 g | % m. 14
  a4 bes r8 c16 bes a8 bes16 c | % m. 15
  d8 bes g e'! f2~ | % m. 16
  f8 f g bes, a4 bes | % m. 17
  r8 c16 d es8 d16 c d8 c16 bes c8 d | % m. 18
  es8 d16 c d8 e! f f,16 g as8 g16 f | % m. 19
  g8 g' as c, b!4 c | % m. 20
  r8 d16 es f8 es16 d es8 e! f fis | % m. 21
  g16 as g f! es8 f16 g as8 f d b'! | % m. 22
  c4~ c16 d c bes a d, c d es d c bes | % m. 23
  a8 c d d, g g'16 a bes8 a16 g | % m. 24
  d'8 d,16 es f8 es16 d es,8 es'16 f g8 f16 es | % m. 25
  bes'8 bes,16 c d8 c16 bes c,8 c'16 d es8 d16 c | % m. 26
  g'8 g,16 a bes8 a16 g es'2 | % m. 27
  d4 r r2 | % m. 28
  r8 d es g, fis4 a | % m. 29
  d,8 d'16 c bes8 c16 d es2~ | % m. 30
  es8 es16 d c8 d16 es fis,8 a16 g fis8 g16 a | % m. 31
  d,8 d'16 c bes8 c16 d es8 c a fis' | % m. 32
  g8 f! es d c bes a g | % m. 33
  c8 a d d, g2 \fermata \bar "|." | % m. 34
  
}

\score {
  \new StaffGroup 
  <<
    \new Staff = "soprano"
      \soprano
    
     \new Staff = "alto" 
       { \clef bass \alto }
    
     \new Staff = "tenor" 
       { \clef bass \tenor }
    
    \new Staff = "bass" 
      { \clef bass \bass }
      
  >>
  
\layout {
  indent = 0.0
  }

}
