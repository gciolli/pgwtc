\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue II"
  subtitle = "in 3 voices"
  composer = "BWV 847"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key c \minor
  \time 4/4
}

soprano = \relative c'' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  r8 g'16 fis g8 c, es g16 fis g8 a! | % m. 3
  d,8 g16 fis g8 a! c,16 d es4 d16 c | % m. 4
  bes8 es16 d es8 g, as f'16 es f8 a,! | % m. 5
  bes8 g'16 f g8 b,! c d16 es f4~ | % m. 6
  f8 es16 d c bes! as g f8 as' g f | % m. 7
  es8 d es f b,! c d b | % m. 8
  c8 g'16 fis g8 d es4 r8 e!8 | % m. 9
  f8 f16 e! f8 c d4 r8 d \pageBreak | % m. 10
  es8 es16 d es8 bes c es16 d es8 f | % m. 11
  bes,8 es16 d es8 f as,16 bes c4 bes16 as | % m. 12
  g16 es f g as bes c d es d c d es f g a! | % m. 13
  bes16 f, g as bes c d e! f es d es f g a! b! | % m. 14
  c8 b!16 a! g f! es d c8 es d c | % m. 15
  bes8 a! bes c fis, g a fis | % m. 16
  g8 d'16 c d8 r r e!16 d e8 r8 | % m. 17
  r8 fis16 e! fis8 r r g,16 f! g8 r | % m. 18
  r8 a!16 g a!8 r r b!16 a b8 r | % m. 19
  r8 c16 b! c8 g as c16 b c8 d | % m. 20
  g,8 c16 b! c8 d f,16 g as4 g16 f | % m. 21
  es8 c'16 b! c8 g as4 r8 a! | % m. 22
  bes8 bes16 a! bes8 f g4 r8 g~ | % m. 23
  g8 as16 bes c b! c as f2~ | % m. 24
  f8 d'16 c d8 f, es es'16 d es8 g, | % m. 25
  f8 f'16 es f8 as, g16 f' es d c b! a! g | % m. 26
  c8 f es d r as g f | % m. 27
  g8 f16 es f8 d as' g r a! | % m. 28
  b!8 c f,16 es d c c8 c'16 b c8 g | % m. 29
  as8 c16 b! c8 <as! b! d> g c16 b c8 d | % m. 30
  f,16 g as4 g16 f e!2 \bar "|." | % m. 31
  
}

alto = \relative c' {
  \global
  
  r8 c'16 b! c8 g as c16 b c8 d | % m. 1
  g,8 c16 b! c8 d f,16 g as4 g16 f | % m. 2
  es16 c' b! a! g f! es d c8 es' d c | % m. 3
  bes8 a! bes c fis, g a fis | % m. 4
  g4 r16 c,d es f g as8~ as16 d, es f | % m. 5
  g16 a! bes8~ bes16 es, f g as g f es d8 c'16 b! | % m. 6
  c4 r r8 f es d | % m. 7
  r8 as g f g f16 es f8 d | % m. 8
  g4 r8 b! c c16 b c8 g | % m. 9
  as4 r8 a! bes bes16 a bes8 f | % m. 10
  g4 r8 g as as g f | % m. 11
  r8 \clef bass as, bes c r as16 g as8 f | % m. 12
  bes8 c bes as bes g f es | % m. 13
  f8 des' c bes c as g f | % m. 14
  g8 \clef treble g'16 fis g8 c, es g16 fis g8 a! | % m. 15
  d,8 g16 fis g8 a! c,16 d es4 d16 c | % m. 16
  bes8 r r16 d e! fis g a! bes8~ bes16 e, f! g | % m. 17
  a!16 bes c8~ c16 fis, g a bes8 es,16 d es8 g, | % m. 18
  as8 f'16 es f8 a,! bes g'16 f g8 b,! | % m. 19
  c16 f es d c bes as g f8 as' g f | % m. 20
  es8 d es f b,! c d b | % m. 21
  c4 r8 e! f f16 e f8 c | % m. 22
  d4 r8 d es es16 d es8 bes | % m. 23
  c2~ c8 d16 es f es f d | % m. 24
  b!8 r r b c r r es | % m. 25
  d8 r r f~ f r r f | % m. 26
  es8 as g f es d es f | % m. 27 
  b,! c d b b c r c | % m. 28
  f16 d es c~ c8 b! c4 r8 e! | % m. 29
  f4 r8 f f es16 d es8 <as f> | % m. 30
  <d, b>8 r q r <c g>2 \bar "|." | % m. 31
  
}

bass = \relative c {
  \global
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  R1 | % m. 5
  R1 | % m. 6
  r8 c'16 b! c8 g as c16 b c8 d | % m. 7
  g,8 c16 b! c8 d f,16 g as4 g16 f | % m. 8
  es16 c' b! a! g f es d c  d es d c bes! as! g | % m. 9
  f16 bes' as g f es d c bes c d c bes as g f | % m. 10
  es16 as' g f es des c bes as8 c' bes as | % m. 11
  g8 f g as d, es f d | % m. 12
  es8 as g f g es d c | % m. 13
  d8 bes' as g as f es d! | % m. 14
  es8 r r4 r8 c bes a! | % m. 15
  r8 es'8 d c d c16 bes c8 d | % m. 16
  g,8 bes'16 a! bes8 d, es c'16 bes c8 e,! | % m. 17
  f8 d'16 c d8 fis, g4 r16 g, a! b! | % m. 18
  c16 d es8~ es16 a,! bes c d es f8~ f16 b,! c d | % m. 19
  es8 r r e! f f, es! d | % m. 20
  r8 as' g f g f16 es f8 g | % m. 21
  c16 d es d c bes as g f bes' as g f es d c | % m. 22
  bes16 c d c bes as g f es as' g f es d c bes | % m. 23
  as16 bes c bes as g f es d g' f es d c b! a! | % m. 24
  g4 r r16 g a! b! c d es f | % m. 25
  g16 f as g f es d c b!8 c16 b c8 g | % m. 26
  as8 c16 b! c8 d g, c16 b c8 d | % m. 27
  f,16 g as4 g16 f es4 r8 es' | % m. 28
  d8 c g' g, <c c,>2~ | % m. 29
  q1~ | % m. 30
  q1 \bar "|." | % m. 31
  
}

\score {
  \new StaffGroup
  <<
    \new Staff = "soprano" 
      \soprano
    
    \new Staff = "alto"
      \alto
    
    \new Staff = "bass"
    { \clef bass \bass }
    
  >>
  
\layout { 
  indent = 0.0
  }
  
}
