\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue XXI"
  subtitle = "in 3 voices"
  opus = "BWV 866"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key bes \major
  \numericTimeSignature
  \time 3/4
}

soprano = \relative c' {
  \global
  
  r8 f g f bes d, | % m. 1
  c8 a'16 g bes a g f c'8 es, | % m. 2
  d8 bes'16 c a bes c d es d c es | % m. 3
  d16 c bes c a bes c d es d c es | % m. 4
  d8. c16 bes a bes c d8 f~ | % m. 5
  f8 bes, a bes e! g~ | % m. 6
  g8 a,16 bes c8 c c c | % m. 7
  c16 bes a bes c8 c c c | % m. 8
  c4 r r16 d bes d | % m. 9
  es8 r r4 r16 es c es | % m. 10
  f8 r r a,16 bes c bes a c | % m. 11
  bes4 r8 a16 bes c bes a c | % m. 12
  bes8 bes d bes f' a, | % m. 13
  g8 e'!16 d f e d c g'8 bes, | % m. 14
  a8 f'16 g e! f g a bes a g bes | % m. 15
  a16 g f g e! f g a bes a g bes | % m. 16
  a16 bes a g fis g a bes c bes a c | % m. 17
  bes16 a g a fis g a bes c bes a c | % m. 18
  bes16 a g f es f g a bes a g bes | % m. 19
  a16 g f es d es f g a g f a | % m. 20
  g16 f es d c d es f g fis e! g | % m. 21
  fis16 e! d c bes a bes c d8 g~ | % m. 22
  g8 c, bes c fis a~ | % m. 23
  a8 bes,16 c d8 d d d | % m. 24
  d16 c bes c d8 d d d | % m. 25
  d8 r r4 r16 es c es | % m. 26
  d8 r r4 r16 f d f | % m. 27
  g8 r r b,!16 c d c b d | % m. 28
  c8 r r b!16 c d c b d | % m. 29
  c8 es d es c a' | % m. 30
  bes8 d, c d bes g' | % m. 31
  a8 c, bes c a fis' | % m. 32
  g16 f! e! d cis d e f g f e g | % m. 33
  f16 es d c b! c d es f es d f | % m. 34
  es16 c b! a g f g c es4~ | % m. 35
  es8 as, g as d f~ | % m. 36
  f8 bes, c bes es g, | % m. 37
  f8 d'16 c es d c bes f'8 as, | % m. 38
  g8 es'16 f d es f g as g f as | % m. 39
  g16 f es f d es f g as g f as | % m. 40
  g8. f16 es d es f g8 bes~ | % m. 41
  bes8 es, d es a c~ | % m. 42
  c8 d,16 es f8 f f f | % m. 43
  f16 es d es f8 f f f | % m. 44
  f8 r r a,16 bes c bes a c | % m. 45
  bes8 r r a16 bes c bes a c | % m. 46
  bes8 g' d4 c | % m. 47
  bes2. \fermata \bar "|." | % m. 48
    
}

mezzo = \relative c' {
  \global
  
  R2. | % m. 1
  R2. | % m. 2
  R2. | % m. 3
  R2. | % m. 4
  r8 bes d bes f' a, | % m. 5
  g8 e'!16 d f e d c g'8 bes, | % m. 6
  a8 f'16 g e! f g a bes a g bes | % m. 7
  a16 g f g e! f g a bes a g bes | % m. 8
  a8. g16 f es! d es f8 bes~ | % m. 9
  bes8 es, d es a c~ | % m. 10
  c8 d,16 es f8 f f f | % m. 11
  f16 es d es f8 f f f | % m. 12
  f4 r r16 \clef bass a, f a | % m. 13
  bes8 r r4 r16 bes g bes | % m. 14
  c8 r r e,!16 f g f e g | % m. 15
  f8 r r e!16 f g f e g | % m. 16
  f16 a bes c d8 d d d | % m. 17
  d16 c bes c d8 d d d | % m. 18
  d8 r r4 r | % m. 19
  R2. | % m. 20
  R2. | % m. 21
  r8 \clef treble d es! d g bes, | % m. 22
  a8 fis'16 e! g fis e d a'8 c, | % m. 23
  bes8 g'16 a fis g a bes c bes a c | % m. 24
  bes16 a g a fis g a bes c bes a c | % m. 25
  bes16 c d es d c bes as g8 c~ | % m. 26
  c8 f, es f b! d~ | % m. 27
  d8 es,16 f g8 g g g | % m. 28
  g16 f es f g8 g g g | % m. 29
  g8 r r4 r8 es' | % m. 30
  f8 r r4 r8 d | % m. 31
  es8 r r4 r8 c | % m. 32
  d8 bes, a bes g cis | % m. 33
  d8 as g as f b! | % m. 34
  c8 c es c g' bes,! | % m. 35
  as8 d16 c es d c bes f'8 as, | % m. 36
  g16 bes c d es d es f g8 es~ | % m. 37
  es8 as, g as d f~ | % m. 38
  f8 g,16 as bes8 bes bes bes | % m. 39
  bes16 as g as bes8 bes bes bes | % m. 40
  bes8 es g es bes' d, | % m. 41
  c8 a'16 g bes a g f c'8 es, | % m. 42
  d8 bes'16 c a bes c d es d c es | % m. 43
  d16 c bes c a bes c d es d c es | % m. 44
  d8 f,,16 g a bes c d es d c es | % m. 45
  d16 c bes c a bes c d es d c es | % m. 46
  d8 bes' bes4 a8 es | % m. 47
  d2. \fermata \bar "|." | % m. 48
    
}

bass = \relative c {
  \global
  
  R2. | % m. 1
  R2. | % m. 2
  R2. | % m. 3
  R2. | % m. 4
  R2. | % m. 5
  R2. | % m. 6
  R2. | % m. 7
  R2. | % m. 8
  r8 f g f bes d, | % m. 9
  c8 a'16 g bes a g f c'8 es, | % m. 10
  d8 bes'16 c a bes c d es d c es | % m. 11
  d16 c bes c a bes c d es d c es | % m. 12
  d8 d,16 c bes a bes c d8 f~ | % m. 13
  f8 bes, a bes e! g~ | % m. 14
  g8 a,16 bes c8 c c c | % m. 15
  c16 bes a bes c8 c c c | % m. 16
  f,8 r r fis'16 g a g fis a | % m. 17
  g8 r r fis16 g a g fis a | % m. 18
  g8 d c d g, es' | % m. 19
  f8 c bes c f, d' | % m. 20
  es8 bes a bes es,! c' | % m. 21
  d8 r r4 r16 g es! g | % m. 22
  c,8 r r4 r16 a' fis a | % m. 23
  d,8 r r fis,16 g a g fis a | % m. 24
  g8 r r fis16 g a g fis a | % m. 25
  g8 g' bes g c es, | % m. 26
  d8 b'!16 a c b a g d'8 f, | % m. 27
  es8 c'16 d b! c d es f es d f | % m. 28
  es16 d c d b! c d es f es d f | % m. 29
  es16 d c bes! a bes c d es d c es | % m. 30
  d16 c bes a g a bes c d c bes d | % m. 31
  c16 bes a g fis g a bes c bes a c | % m. 32
  bes8 g f g e!4 | % m. 33
  d8 f es! f d4 | % m. 34
  c8 r r4 r16 c es c | % m. 35
  f8 r r4 r16 bes, d bes | % m. 36
  es8 r r4 r16 es g es | % m. 37
  bes'8 r r4 r16 bes d bes | % m. 38
  es8 r r d,16 es f es d f | % m. 39
  es8 r r d16 es f es d f | % m. 40
  es8 r r4 r16 es g es | % m. 41
  f8 r r4 r16 f a f | % m. 42
  bes8 r r a16 bes c bes a c | % m. 43
  bes8 r r a16 bes c bes a c | % m. 44
  bes16 f d es f8 f f f | % m. 45
  f16 es d es f8 f f f | % m. 46
  f16 es d es f8 es f f, | % m. 47
  bes2. \fermata \bar "|." | % m. 48
    
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
