\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue XIX"
  subtitle = "in 3 voices"
  opus = "BWV 888"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key a \major
  \time 4/4
}

soprano = \relative c'' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  r8 a16 b cis b a cis b d cis8~ cis16 e d8~ | % m. 5
  d16 fis e d e b cis d cis d e fis e a gis fis | % m. 6
  gis16 fis e dis e4~ e8. dis16 e8. dis16 | % m. 7
  e8. cis16 d!4~ d8. bis16 cis4~ | % m. 8
  cis8. ais16 b!4~ b16 eis, fis gis a! gis fis a | % m. 9
  gis16 b a8~ a16 cis b8~ b16 d! cis b cis gis a b | % m. 10
  a2~ a16 cis b a b fis gis a | % m. 11
  gis2~ gis8. gis16 cis8. dis16 | % m. 12
  bis8 r dis4~ dis16 bis cis dis cis fis e dis | % m. 13
  e2~ e16 g! fis e fis cis d! e | % m. 14
  d2~ d16 fis e d e b cis d | % m. 15
  cis16 d cis b a gis fis e d fis gis b e, gis ais cis | % m. 16
  b4~ b8. b16 a4~ a8. a16 | % m. 17
  gis4~ gis8. g!16 fis4 r | % m. 18
  fis4 r r2 | % m. 19
  r16 cis' d e fis e d fis d g! fis8~ fis16 a! g8~ | % m. 20
  g!16 b a gis a e fis g! fis2~ | % m. 21
  fis16 fis e dis e b cis d! cis2~ | % m. 22
  cis16 c! b ais b fis gis a! gis8. a16 b8. cis16 | % m. 23
  d8. dis16 e8. eis16 fis2~ | % m. 24
  fis16 dis e fis e a gis fis gis fis e dis e b cis d! | % m. 25
  cis16 gis a b a d cis b cis b a gis a e fis g! | % m. 26
  fis16 a b cis d4~ d16 gis, a b cis b a cis | % m. 27
  b16 d cis8~ cis16 e d8~ d16 fis e dis e b cis d! | % m. 28
  cis8. b16 gis8. \prall a16 a2 \fermata \bar "|." | % m. 29
    
}

mezzo = \relative c' {
  \global
  
  R1 | % m. 1
  r2 r8 e16 fis gis fis e gis | % m. 2
  fis16 a gis8~ gis16 b a8~ a16 cis b a b fis gis a | % m. 3
  gis16 b a gis a e fis g! fis a gis fis gis dis e fis | % m. 4
  e4. fis8 gis8. gis16 a8. a16 | % m. 5
  b4 r8 gis a gis a b | % m. 6
  b4~ b16 a gis b a fis b8~ b16 gis cis8~ | % m. 7
  cis8 fis,~ fis8. fis16 e4~ e8. e16 | % m. 8
  dis4~ dis8. d!16 cis4. dis8 | % m. 9
  eis8. eis16 fis8. fis16 gis4 r | % m. 10
  r16 gis fis eis fis cis dis e! dis2~ | % m. 11
  dis16 gis, cis dis e dis cis e dis fis e8~ e16 gis fis8~ | % m. 12
  fis16 a gis fis gis dis e fis e8. gis,16 ais8 bis | % m. 13
  cis16 dis cis bis cis gis ais b! ais8 r r4 | % m. 14
  r16 cis' b ais b fis gis a! gis2 | % m. 15
  a8 r r4 r2 | % m. 16
  r8 r16 fis g!4~ g8. eis16 fis4~ | % m. 17
  fis8. dis16 e!4~ e8 d!16 cis b ais gis fis | % m. 18
  d'16 cis b ais b e d cis d d' cis b cis gis a! b | % m. 19
  a4. b8 cis8. cis16 d8. dis16 | % m. 20
  e2~ e16 e d cis d a b cis | % m. 21
  b2~ b16 b a gis a e fis g! | % m. 22
  fis2~ fis16 b, e fis gis fis e g! | % m. 23
  fis16 a gis!8~ gis16 b a8~ a16 cis b a b fis gis a | % m. 24
  gis16 a gis fis gis dis e fis e8 r r4 | % m. 25
  r16 d cis b cis gis a b a8 r r4 | % m. 26
  r4 r16 a' gis fis e4~ e8. fis16 | % m. 27
  gis8. gis16 a8. a16 b4 r | % m. 28
  r16 a gis fis e d cis b cis2 \fermata \bar "|." | % m. 29
    
}

bass = \relative c' {
  \global
  
  r8 a16 b cis b a cis b d cis8~ cis16 e d8~ | % m. 1
  d16 fis e d e b cis d cis8 a b cis | % m. 2
  d8 b16 e cis8 fis16 cis dis8 e4 dis8 | % m. 3
  e8. b16 cis8 a d8. a16 b8 gis | % m. 4
  cis16 d cis b a8. a16 e'8. e,16 fis8. fis'16 | % m. 5
  gis4 r8 e a, b cis dis | % m. 6
  e8 e,16 fis gis fis e gis fis a gis8~ gis16 b a8~ | % m. 7
  a16 cis b a b fis gis a gis b a gis a e fis gis | % m. 8
  fis16 a gis fis gis dis eis fis eis cis dis eis fis8. fis16 | % m. 9
  cis'8. cis,16 d8. dis16 e!8. gis16 eis8 cis | % m. 10
  fis8. gis16 a8 fis b,8. cis16 dis8 b | % m. 11
  e8. dis16 cis8. cis'16 bis gis cis b! a gis a fis | % m. 12
  gis8. dis16 bis8 gis cis2~ | % m. 13
  cis8 r r4 r8 r16 cis' ais8 fis | % m. 14
  b8. fis16 d8 b e8. b16 gis8 e | % m. 15
  a,16 e' a b cis b a cis b d cis8~ cis16 e d8~ | % m. 16
  d16 fis e dis e b cis d! cis e d cis d a b cis | % m. 17
  b16 d cis b cis gis ais b ais fis gis ais gis cis b ais | % m. 18
  b16 cis d e d g! fis e fis gis a! b a d cis b | % m. 19
  cis16 a b cis d8. d,16 a'8. ais16 b8. b,16 | % m. 20
  c!8. c'!16 cis8. a16 d cis b a b fis gis a | % m. 21
  gis16 a gis fis gis b a gis a gis fis e fis cis dis e | % m. 22
  dis16 e dis cis dis fis e dis e8. e,16 e'8. ais,16 | % m. 23
  b8. b'16 cis8. cis,16 d!8. d'16 dis8. dis,16 | % m. 24
  e8 r r4 r16 fis gis a gis b a gis | % m. 25
  a4 r r16 b, cis d cis e d cis | % m. 26
  d16 cis b a b fis gis a gis e fis gis a b cis a | % m. 27
  e'8. e,16 fis8. fis'16 gis a gis fis e d! cis b | % m. 28
  a8 d e4 a,2 \fermata \bar "|." | % m. 29
    
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
