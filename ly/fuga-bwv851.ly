\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue VI"
  subtitle = "in 3 voices"
  composer = "BWV 851"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key d \minor
  \time 3/4
}

soprano = \relative c' {
  \global
  
  r8 d e f g e | % m. 1
  f16 (d cis d) bes'4-. g\trill | % m. 2
  a4~ a16 g f e g f e d | % m. 3
  e8 c'~ c16 b! a b b a gis a | % m. 4
  a16 g! fis g e'4-. g, | % m. 5
  f8 d'~ d16 c bes a c bes a g | % m. 6
  a4 g2~ | % m. 7
  g8 e f g a f | % m. 8
  bes16 g fis g es'4-. cis\turn | % m. 9
  d4 d-. b!\turn | % m. 10
  c4 c-. a\turn | % m. 11
  bes2.~ | % m. 12
  bes8 a b! cis d b | % m. 13 
  cis16 a gis a f'4-. d\trill | % m. 14
  e8 a~ a16 g fis e g fis e d | % m. 15
  es16 c' bes8 r16 a gis fis a gis fis e! | % m. 16
  d16 c b! c c b a b b a gis a | % m. 17
  g!4~ g16 f e f f e d a' | % m. 18
  a2.~ | % m. 19
  a4~ a16 dis e a, c b! a gis | % m. 20
  a4~ \mordent a16 g f e g f e d | % m. 21
  e8 e' d cis b! d | % m. 22
  cis16 e f e g,4-. bes~ | % m. 23
  bes16 a g a a g f g g f e f | % m. 24
  f4~ f16 e fis g fis g a g | % m. 25
  g16 a bes a a b! c b b cis d cis | % m. 26
  cis8 a' g f e g | % m. 27
  f16 a bes a cis,4 e~ | % m. 28
  e8 a,8 d4 c!~ | % m. 29
  c8 es~ es16 d c d d c bes c | % m. 30
  c bes a bes d c bes c c bes a bes | % m. 31
  bes16 a g a c bes a bes bes a g a | % m. 32
  a g f a e'4-. cis \trill | % m. 33
  d4~ d16 c bes a c bes a g | % m. 34
  a8 fis g a bes g | % m. 35
  a16 c es8~ es16 d c bes d c bes a | % m. 36
  bes16 d f8~ f16 e d cis e d cis b! | % m. 37
  cis16 e bes'8~ bes16 a g f a g f e | % m. 38
  g f e f f e d e e d cis d | % m. 39
  c!4~ c16 bes a bes bes a g d' | % m. 40
  d2.~ | % m. 41
  d4~ d16 gis a d, f e d cis | % m. 42
  d2. | % m. 43
  d2. \fermata  \bar "|." | % m. 44 
  
}

mezzo = \relative c' {
  \global
  
  R2. | % m. 1
  R2. | % m. 2
  r8 a b! c d b | % m. 3
  c16 a gis a f'4-. d \trill | % m. 4
  e4~ e16 d cis b! d cis b a | % m. 5
  d4 r r | % m. 6
  r8 f~ f16 e d e e d cis d | % m. 7
  cis4 d8 e cis d | % m. 8
  es8 d g4 e!4 | % m. 9
  r16 a g a f4 d | % m. 10
  r16 g f g es4 c | % m. 11
  r8 a' g f e! g | % m. 12
  cis,4 d8 e a,4 | % m. 13
  r8 e' d cis b! d | % m. 14
  cis16 e f e a,4-. c! | % m. 15
  bes16 d es d b!4 e!~ | % m. 16
  e8 r r4 r | % m. 17
  r8 a, b! cis d b | % m. 18
  c!16 a gis a f'4-. d \trill | % m. 19
  e8 f b,!2 | % m. 20
  a4 r r | % m. 21
  R2. | % m. 22
  R2. | % m. 23
  R2. | % m. 24
  r8 d c bes a c | % m. 25
  bes4 d2 | % m. 26
  g,4 r r | % m. 27
  r8 d' e f g e | % m. 28
  f16 d cis  d b'4-. g \prall | % m. 29
  a4 \clef bass fis,-. a-. | % m. 30
  r16 g fis g e4-. g-. | % m. 31
  r16 f e f d4-. f-. | % m. 32
  r8 f g a bes g | % m. 33
  a f g a g4 | % m. 34
  r8 a g fis e g | % m. 35
  fis8 bes a g fis a | % m. 36
  g8 c b! a gis b | % m. 37
  a8 \clef treble f' e d cis e | % m. 38
  d8 r r4 r | % m. 39
  r8 d e fis g e | % m. 40
   f!16 d cis d bes'4-. g \trill | % m. 41
   a8 bes e,2 \trill | % m. 42
   r8 <a c> <g bes> <fis a> <e g> <g bes> | % m. 43
   <fis a>2. \fermata \bar "|." | % m. 44 
  
}

bass = \relative c {
  \global
  
  R2. | % m. 1
  R2. | % m. 2
  R2. | % m. 3
  R2. | % m. 4
  R2. | % m. 5
  r8 d e f g e | % m. 6
  f16 d cis d bes'4-. g \trill | % m. 7
  a4~ a16 g f e g f e d | % m. 8
  g8 bes~ bes16 a g a a g f g | % m. 9
  g16 f e f a g f g g f e f | % m. 10
  f16 e d e g f es f f es d es | % m. 11
  es16 d cis d bes'4-. g \trill | % m. 12
  a4~ a16 g f e g f e d | % m. 13
  e8 cis d e f d | % m. 14
  a'4 fis8 e d fis | % m. 15
  g4 gis8 fis e gis | % m. 16
  a8 a, b! c d b | % m. 17
  cis16 a gis a f'4-. d \trill | % m. 18
  e8 f e16 d c d d c b! c | % m. 19
  c8 d e d e4 | % m. 20
  a8 a, b! cis d b | % m. 21
  cis16 a gis a f'4-. d \trill | % m. 22
  a'8 f e d cis e | % m. 23
  d16 f g f a,4-. cis \trill | % m. 24
  d2. | % m. 25
  r8 g fis e d f! | % m. 26
  e4~ e16 d cis b! d cis b a | % m. 27
  d4 r r | % m. 28
  r8 a' g f e g | % m. 29
  fis16 a bes a d,4 fis | % m. 30
  g,4 c e | % m. 31
  f,4 bes d | % m. 32
  e2. | % m. 33
  f8 d e fis g e | % m. 34
  fis16 d cis d bes'8 a g bes | % m. 35
  a8 g fis e d fis | % m. 36
  g,8 a' gis fis e gis | % m. 37
  a,8 d' cis b! a cis | % m. 38
  d8 d, e f g e | % m. 39
  fis16 d cis d bes'4-. g \trill | % m. 40
  a8 bes a16 g f g g f e f | % m. 41
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
