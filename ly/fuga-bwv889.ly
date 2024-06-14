\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue XX"
  subtitle = "in 3 voices"
  opus = "BWV 889"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key a \minor
  \time 4/4
}

soprano = \relative c'' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  R1 | % m. 5
  r4 e c f | % m. 6
  gis,4 r r8 d' b e | % m. 7
  c8 a fis d' b g e c' | % m. 8
  a8 b c4~ c32 c d e f e d c b4 \noBreak | % m. 9
  r8 c'32 b a g f8 a32 g f e d e f16. e32 d c b c d16. c32 b a | % m. 10
  g8. a32 b b8. \trill a32 b c4 r | % m. 11
  r8 g e a fis8. fis32 gis gis8. \trill fis32 gis | % m. 12
  a16 b32 c d e fis gis a4 g! c \noBreak | % m. 13
  dis,4 r r8 a' fis b | % m. 14
  g8 e cis a' f! r32 d e f g8 r32 f e d \noBreak | % m. 15
  c8 r32 e f g a bes a g f e d c b! c b a gis fis e d c8 c' | % m. 16
  b2~ b16 e, a8~ a16 a, d8~ | % m. 17
  d8 f' b, d gis,4 b~ | % m. 18
  b8 e32 d c b a16 cis32 e g8 f r r4 | % m. 19
  r16 d32 c b16 c32 d g,8 f' e r r4 | % m. 20
  r4 a f bes \noBreak | % m. 21
  cis,4 r r8 g' e a | % m. 22
  f8 d r4 r8 f d g | % m. 23
  e8 c a f' d b gis e' | % m. 24
  c4~ c8 r r32 b c d e fis gis a b a gis fis e d c b | % m. 25
  e32 d c b a gis fis e a g! f! e d c b a f' r r16 r8 r4 | % m. 26
  r8 r32 e fis gis gis8. \trill fis32 gis a2~ | % m. 27
  a8 b e, gis a2 \fermata \bar "|." | % m. 28
    
}

mezzo = \relative c'' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  r4 a g c | % m. 3
  dis,4 r r8 a'-! fis-! b-! | % m. 4
  g8-! e-! cis-! a'-! f-! d-! b-! g'-! | % m. 5
  e8 a4 gis8 a, a'32 g! f e d8 f32 e d c | % m. 6
  \clef bass b32 c d16. c32 b a gis a b16. a32 gis fis e8. fis32 gis gis8. \trill fis32 gis | % m. 7
  a8 r32 a b c d8 r32 c b a g8 r32 g a b c8 r32 bes a g | % m. 8
  f16 \clef treble f'8 f16~ f32 e f g a g f e d4~ d32 d e f g f e d | % m. 9
  c8 g c f r a d, f | % m. 10
  b,8 d f d g c, a d | % m. 11
  b8. b32 cis cis8. \trill b32 cis d4 r | % m. 12
  r4 r16 c' b fis'!~ fis b, e32 d! c b a8 c32 b a g | % m. 13
  fis32 g a16. g32 fis e dis e fis16. e32 dis cis b8. cis32 dis dis8. \trill cis32 dis | % m. 14
  e8 r r4 r8 d b g' | % m. 15
  e8 r r4 r r32 e f! g! a g f e | % m. 16
  d8 f b, d c4 f | % m. 17
  gis,4 r r8 d' b e | % m. 18
  c8 r r4 r8 a'32 g f e d16 fis32 a c!8 | % m. 19
  b8 r r4 r16 g32 f e16 f32 g c,8 bes' | % m. 20
  a16 b!32 cis d8~ d c d4 r | % m. 21
  r8 bes e, g cis, e' a,4~ | % m. 22
  a8 a d, f b, d' g,4~ | % m. 23
  g8 r r4 r8 d b gis'! | % m. 24
  a4 r32 b a g! f! e d c d8 \clef bass b16 a gis4 | % m. 25
  a4 r r32 e' d c b a gis fis d' c b a gis fis e dis | % m. 26
  e8 r r4 r8 r32 b' c d cis8. \trill b32 cis | % m. 27
  d8 f gis, b cis2 \fermata \bar "|." | % m. 28
  
}

bass = \relative c' {
  \global
  
  r4 e c f | % m. 1
  gis,4 r r8 d'-! b-! e-! | % m. 2
  c8-! a-! fis-! dis'-! e, e'32 d! c b a8 c32 b a g | % m. 3
  fis32 g a16. g32 fis e dis e fis16. e32 d cis b8. cis32 dis dis8. \trill cis32 dis | % m. 4
  e8 r32 e f! g a8 r32 g f e d8 r32 d e f g8 r32 f e d | % m. 5
  c8. d16 e f32 e d16 e f4 r | % m. 6
  r8 f b, d gis, b e4 | % m. 7
  a,8 r r fis' g r r e | % m. 8
  f8 g a g f d g f | % m. 9
  e4 a b, r | % m. 10
  r8 f' d g e8. e32 fis fis8. \trill e32 fis | % m. 11
  g8 e a4 r8 d, b e | % m. 12
  c8 a fis dis' e,8 r r4 | % m. 13
  r8 c'' fis, a dis, fis b4 | % m. 14
  e,8 r32 e, f g a8 r32 g f e d8 r r4 | % m. 15
  r8 a'' f d' gis, e a4~ | % m. 16
  a32 a b c d c b a gis a gis fis e d c b a8 a'32 g! f! e d8 f32 e d c | % m. 17
  b32 c d16. c32 b a gis a b16. a32 gis fis e8. fis32 gis gis8. \trill fis32 gis | % m. 18
  a8. b32 c cis8. \trill b32 cis d8. e32 f fis8. \trill e32 fis | % m. 19
  g8. a32 b b8. \trill a32 b \clef treble c8. d32 e e8. \trill d32 e | % m. 20
  f8. g32 a a8. \trill g32 a bes16 \clef bass bes,32 c! d c bes a g f g a bes a g f | % m. 21
  e32 f g16. f32 e d cis d e16. d32 cis b! a8. b32 cis cis8. \trill b32 cis | % m. 22
  d32 e f16. e32 d c b c d16. c32 b a g8. a32 b b8. \trill a32 b | % m. 23
  c8 r32 c' d e f8 r32 e d c b8 r32 b, c d e8 r32 d c b | % m. 24
  a16 b32 c d e fis gis a4~ a8 gis16 fis e8 d | % m. 25
  c4 f! gis, r | % m. 26
  r8 d'! b e c a e g! | % m. 27
  f32 e d8 e32 f e8. \upprall d32 e a,2 \fermata \bar "|." | % m. 28
    
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
