\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

#(ly:expect-warning "Previous mark event here")
#(ly:expect-warning "Two simultaneous mark events, junking this one")

\header {
 title = "Fugue X"
 subtitle = "in 2 voices"
 opus = "BWV 855"
 copyright = "No rights reserved."
 tagline = ""
}

global = {
  \key e \minor
  \time 3/4
}

soprano = \relative c' {
  \global
  
  e16 g b e dis e d! e cis e c! e | % m. 1
  b16 e dis e ais, cis g fis g ais fis e | % m. 2
  d8 b' r16 fis' g fis e d cis e | % m. 3
  d4~ d16 cis b ais b cis ais b | % m. 4
  cis16 fis, ais cis e ais, cis e g fis e g | % m. 5
  fis16 e d cis b ais gis fis d'4~ | % m. 6
  d16 e, gis b d gis, b d f! e d f | % m. 7
  e16 d c b a gis fis e c'4~ | % m. 8 
  c16 a d, e fis g a b c d e fis | % m. 9
  g16 fis a g fis e d c b g'a,fis' | % m. 10
  g,16 b d g fis g f! g e g es g | % m. 11
  d16 g fis g cis, e bes a bes cis a g | % m. 12
  fis8 d' r16 a' b a g fis e g | % m. 13
  fis4~ fis16 e d cis d e cis d | % m. 14
  e16 d cis b a gis e' d cis b a g! | % m. 15
  fis8 d fis a d fis, | % m. 16
  d'16 c b a g fis d' c b a g f! | % m. 17
  e8 c e g c e, | % m. 18
  e'16 d c b a gis e' d c b a gis | % m. 19
  a16 c e a r e f! e d c b d | % m. 20
  c4~ c16 b a gis a fis dis b' | % m. 21
  e,16 g b e dis e d! e cis e c! e | % m. 22
  b16 e dis e ais, cis g fis g ais fis e | % m. 23
  dis8 b' r16 b' c b a g fis a | % m. 24
  g4~ g16 fis e dis e fis d! e | % m. 25
  cis8 a' r16 a bes a g f! e g | % m. 26
  f!4~ f16 e d cis d e c! d | % m. 27
  b8 g' r16 a, b c d e f! d | % m. 28
  e16 d g f! e d cis bes a bes g a | % m. 29
  f!8 d' r16 a' bes a g f! e g | % m. 30
  f!4~ f16 e d c d c d e | % m. 31
  a,16 c e a gis a g! a fis! a f! a | % m. 32
  e16 a gis a dis, fis c b c dis b a | % m. 33
  gis8 e gis b e gis, | % m. 34
  e'16 d cis b a gis e' d cis b a g! | % m. 35
  fis8 d fis a d fis, | % m. 36
  d'16 c b a g fis d' c b a g fis | % m. 37
  b16 a g fis e dis b' a g fis e dis | % m. 38
  e16 g b e dis e d! e cis e c! e | % m. 39
  b16 a g c b a b gis a g! a fis | % m. 40
  g16 fis g a dis, fis c b c dis b a | % m. 41
  gis32 (b e16) r8 r4 r \bar "|." \override Staff.RehearsalMark #'break-visibility = #begin-of-line-invisible \mark\markup{\musicglyph #"scripts.ufermata"} | % m. 42
  
}

bass = \relative c {
  \global
  
  R2. | % m. 1
  R2. | % m. 2
  b16 d fis b ais b a! b gis b g! b | % m. 3
  fis16 b ais b eis, gis d cis d eis cis b | % m. 4
  ais8 fis' r16 fis' g fis e d cis e | % m. 5
  d4~ d16 cis b ais b cis a! b | % m. 6
  gis,8 e' r16 e' f! e d c b d | % m. 7
  c4~ c16 b a gis a b g! a | % m. 8
  fis,8 d' r16 e fis g a b c a | % m. 9
  b16 a c b a g fis e d e c d | % m. 10
  b8 g' r16 d' e d c b a c | % m. 11
  b4~ b16 a g fis g fis g a | % m. 12
  d,16 fis a d cis d c! d b d bes d | % m. 13
  a16 d cis d gis, b f! e f gis e d | % m. 14
  cis8 a cis e a cis, | % m. 15
  a'16 g fis e d cis a' g fis e d c! | % m. 16
  b8 g b d g b, | % m. 17
  g'16 f! e d c b g' f e d c b | % m. 18
  e16 d c b a gis e' d c b a gis | % m. 19
  a16 c e a gis a g! a fis! a f! a | % m. 20
  e16 a gis a dis, fis c b c dis b a | % m. 21
  g8 e' r16 b' c b a g fis a | % m. 22
  g4~ g16 fis e dis e fis dis e | % m. 23
  fis16 b, dis fis a dis, fis a c b a c | % m. 24
  b16 a g fis e dis cis b g'4~ | % m. 25
  g16 a, cis e g cis, e g bes a g bes | % m. 26
  a16 g f! e d cis b a f'4~ | % m. 27
  f16 d g, a b c d e f g a b | % m. 28
  c16 b e d cis bes a g f! d' e, cis' | % m. 29
  d,16 f! a d cis d c! d b! d bes d | % m. 30
  a16 d cis d gis, b f! e f gis e d | % m. 31
  c8 a' r16 e' f! e d c b d | % m. 32
  c4~ c16 b a gis a fis dis fis | % m. 33
  b16 a gis fis e dis b' a gis fis e d! | % m. 34
  cis8 a cis e a cis, | % m. 35
  a'16 g fis e d cis a' g fis e d c! | % m. 36
  b8 g b d g b, | % m. 37
  b'16 a g fis e dis b' a g fis e dis | % m. 38
  e16 b g e r b'' c b a g a fis | % m. 39
  g16 fis e e' dis e d! e cis e c! e | % m. 40
  b8 a b\noBeam r b, r | % m. 41
  e,8 r r4 r \bar "|." \override Staff.RehearsalMark #'direction = #DOWN \mark\markup{\musicglyph #"scripts.dfermata"} | % m. 42
  
}

\score {
  \new StaffGroup 
  <<
    \new Staff = "soprano" 
      \soprano
    
    \new Staff = "bass" \with {
      \consists "Mark_engraver" }
      { \clef bass \bass }
      
  >>
  
\layout {
  indent = 0.0
  }
  
}
