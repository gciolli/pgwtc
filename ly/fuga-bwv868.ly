\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue XXIII"
  subtitle = "in 4 voices"
  opus = "BWV 868"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key b \major
  \time 4/4
}

soprano = \relative c'' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  r8 b ais b cis4 fis,8 gis16 ais | % m. 5
  b8 cis16 dis e8 dis cis2~ | % m. 6
  cis8 b16 ais b8 cis16 b ais gis fis e dis8 b' | % m. 7
  ais8 b16 cis dis eis fis8~ fis eis16 dis eis4 | % m. 8
  fis8 cis16 b cis8 dis bis4 cis8 dis | % m. 9
  e16 dis e fis e dis cis b ais4 b8 cis | % m. 10
  dis8 dis e fis gis16 a! gis fis e dis cis b | % m. 11
  ais!8 b16 cis dis8 cis b4~ b16 cis dis8 | % m. 12
  e16 fis gis8~ gis fis16 e dis4 r16 dis cis b | % m. 13
  ais4 b8 cis dis2~ | % m. 14
  dis8 gis, ais b cis ais b cis | % m. 15
  dis2 cis~ | % m. 16
  cis8 b16 cis dis eis fis8~ fis16 eis fis gis eis8. fis16 | % m. 17
  fis8 fis gis fis e4 b'!8 ais16 gis | % m. 18
  fis8 e16 dis cis8 dis e2 | % m. 19
  dis4 r8 fis~ fis16 eis fis gis ais gis ais b | % m. 20
  eis,4 fis~ fis8 e!16 dis cis b cis dis | % m. 21
  e16 fis gis8~ gis16 gis fis e dis4~ dis16 cis8 b16~ | % m. 22
  b8 ais16 b cis dis e8~ e dis16 e fis gis a!8~ | % m. 23
  a16 gis a gis fis e dis cis bis8 cis dis4~ | % m. 24
  dis8 cis16 bis cis4~ cis16 e dis cis bis4 | % m. 25
  cis8 gis16 fis gis8 a! eis4 fis8 gis | % m. 26
  a!16 gis a b a gis fis e dis4 e8 fis | % m. 27
  gis8 gis ais! b cis2~ | % m. 28
  cis8 b r4 r2 | % m. 29
  R1 | % m. 30
  r8 fis' dis e fis4 b,8 dis16 eis | % m. 31
  fis8 gis16 ais b8 ais gis2 | % m. 32
  fis4~ fis8 e16 dis cis dis e8~ e16 gis fis e | % m. 33
  dis1 \fermata \bar "|." | % m. 34
   
}

alto = \relative c' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  r8 fis dis e fis4 b,8 dis16 eis | % m. 3
  fis8 gis16 ais b8 ais gis2 \trill | % m. 4
  fis4 r8 gis16 fis e dis cis b ais8 fis' | % m. 5
  dis8 e16 fis gis ais b8~ b ais16 gis ais b ais gis | % m. 6
  fis4. e8~ e dis16 cis dis e fis gis | % m. 7
  cis,4 r8 dis'16 cis b8 gis r16 dis' cis b | % m. 8
  ais4 r16 gis fis e dis4 e8 fis | % m. 9
  gis2~ gis8 cis, dis e | % m. 10
  fis8 b cis dis e r r4 | % m. 11
  r4 r8 fis, dis e16 fis gis ais b8~ | % m. 12
  b8 ais16 gis ais4~ ais8 gis16 fisis gis8 ais | % m. 13
  fisis4 gis8 ais b16 ais b cis b ais gis fis! | % m. 14
  eis4 fis8 gis ais cis, dis e! | % m. 15
  fis8 fis eis fis gis4 cis,8 dis16 eis | % m. 16
  fis8 gis16 ais b8 ais gis4~ gis16 ais b8 | % m. 17
  ais4 bis8 dis~ dis cis r4 | % m. 18
  r16 dis, e fis gis fis e dis cis4 cis'~ | % m. 19
  cis8 b dis cis b4 fis'8 eis!16 dis | % m. 20
  cis8 b16 ais gis8 ais b2~ | % m. 21
  b4 ais8 cis~ cis16 cis b ais gis8 fis | % m. 22
  e4~ e8. fis16 gis ais b8~ b cis16 dis | % m. 23
  e8 r r4 r2 | % m. 24
  R1 | % m. 25
  R1 | % m. 26
  R1 | % m. 27
  R1 | % m. 28
  r8 b ais b cis4 fis,8 gis16 ais | % m. 29
  b8 cis16 dis e8 dis cis2~ | % m. 30
  cis8 b16 ais b8 cis16 b ais gis fis e dis8 b' | % m. 31
  ais8 b16 cis dis eis fis8~ fis e!16 dis e4~ | % m. 32
  e8 dis16 cis b2 ais4 | % m. 33
  b1 \fermata \bar "|." | % m. 34
   
}

tenor = \relative c' {
  \global
  
  r8 b ais b cis4 fis,8 gis16 ais | % m. 1
  b8 cis16 dis e8 dis cis2 \trill | % m. 2
  b4. cis16 b ais gis fis e dis8 b' | % m. 3
  ais8 b16 cis dis eis fis8~ fis e!16 dis e b e8~ | % m. 4
  e8 dis cis b~ b ais16 gis ais8 dis | % m. 5
  gis,4 r8 gis'16 fis e8 cis r fis16 e | % m. 6
  dis8 cis b gis fis4 r | % m. 7
  R1 | % m. 8
  R1 | % m. 9
  R1 | % m. 10
  r2 r8 b ais! b | % m. 11
  cis4 fis,8 gis16 ais b8 cis16 dis e8 dis | % m. 12
  cis2 b8 r r4 | % m. 13
  r8 dis16 e dis cis b ais gis4 r | % m. 14
  r8 cis16 dis cis b ais gis fis e! fis gis fis e dis cis | % m. 15
  b8 r r4 r8 b' ais gis | % m. 16
  fis4~ fis16 gis fis eis dis cis dis b cis4~ | % m. 17
  cis8 fis16 e! fis gis ais bis cis8 b!16 ais gis fis gis ais | % m. 18
  b2~ b16 b ais gis fis e dis cis | % m. 19
  b16 cis dis e fis gis ais fis gis cis, dis eis fis b, b'8~ | % m. 20
  b16 ais b cis dis cis b ais gis2~ | % m. 21
  gis8 fis16 e fis4~ fis r | % m. 22
  R1 | % m. 23
  r8 \clef treble e' dis e fis4 bis,!8 cis16 dis | % m. 24
  e8 fis16 gis a!8 gis fis4~ fis16 a gis fis | % m. 25
  e4 r16 cis b a! gis4 a8 b | % m. 26
  cis2~ cis8 fis, gis a! | % m. 27
  b8 b cis dis e16 b' ais gis fis e dis cis | % m. 28
  dis8 r r4 r16 \clef bass dis cis b ais gis fis e | % m. 29
  fis16 dis e fis gis ais b8~ b4 ais8. gis16 | % m. 30
  fis4. e8~ e dis16 cis dis8 gis | % m. 31
  cis,4 r r2 | % m. 32
  r2 r8 cis'16 dis cis b ais gis | % m. 33
  fis1 \fermata \bar "|." | % m. 34
    
}

bass = \relative c {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  R1 | % m. 5
  R1 | % m. 6
  r8 fis dis e fis4 b,8 dis16 eis | % m. 7
  fis8 gis16 ais b8 ais gis2 -\parenthesize \trill | % m. 8
  fis4 r r8 gis16 a! gis fis e dis | % m. 9
  cis8 cis' dis e fis fis,16 gis fis e dis cis | % m. 10
  b16 ais b cis b a! gis fis e8 r r4 | % m. 11
  r8 e' dis e16 fis gis4. fis8 | % m. 12
  e4 fis b8 b,16 ais b8 cis | % m. 13
  dis4 r8 cis b gis ais b | % m. 14
  cis4 r8 b ais2~ | % m. 15
  ais16 ais b cis b ais gis fis eis8 eis' fis eis | % m. 16
  dis4. cis8 b gis cis cis, | % m. 17
  fis8 r r4 r2 | % m. 18
  R1 | % m. 19
  R1 | % m. 20
  r2 r8 b ais b | % m. 21
  cis4 fis,8 gis16 ais b8 cis16 dis e8 dis | % m. 22
  cis2 b8 b'16 cis b a! gis fis | % m. 23
  e4 a! dis,16 cis' bis ais gis fis e dis | % m. 24
  cis16 b! a! gis fis e dis cis dis8 e16 fis gis8 gis | % m. 25
  cis,4 r r8 cis'16 d! cis b a! gis | % m. 26
  fis8 fis' gis a! b b,16 cis b a! gis fis | % m. 27
  e16 b' e fis e dis cis b ais!8 fis gis ais | % m. 28
  b16 cis dis e fis gis ais b e,2 | % m. 29
  dis4 cis8 gis'16 fis e dis cis dis e gis fis e | % m. 30
  dis4 gis, fis2~ | % m. 31
  fis4 r16 gis ais b cis dis e! fis gis ais b gis | % m. 32
  ais16 gis fis e dis cis dis e fis2 | % m. 33
  b,1 \fermata \bar "|." | % m. 34
   
}

\score {
  \new StaffGroup 
  <<
    \new Staff = "soprano" 
      \soprano
    
    \new Staff = "alto" 
      \alto
    
    \new Staff = "tenor" 
      { \clef bass \tenor }
    
    \new Staff = "bass"
      { \clef bass \bass }
      
  >>
  
\layout {
  indent = 0.0
  }

}
