\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue XIV"
  subtitle = "in 3 voices"
  opus = "BWV 883"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key fis \minor
  \time 4/4
}

soprano = \relative c' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  r4 r8 fis e cis a'4~ | % m. 4
  a8 gis16 fis gis4~ gis8 cis fis,4~ | % m. 5
  fis8 e16 dis e8 cis dis2 | % m. 6
  cis8 cis' fis,4~ fis8 b e,4~ | % m. 7
  e8 a4 gis fis eis8 | % m. 8
  fis4 r8 b gis e! r a | % m. 9
  fis8 dis r gis eis cis fis4~ | % m. 10
  fis4 eis fis8 a b d | % m. 11
  r8 gis, a cis r fis,16 gis gis8. \trill fis32 gis | % m. 12
  a4 r8 dis bis gis r cis | % m. 13
  fis4 dis gis8 e fis a | % m. 14
  r8 d, e gis r cis, b a | % m. 15
  gis4 r8 cis a fis d'4~ | % m. 16
  d8 cis16 b cis4~ cis8 fis b,4~ | % m. 17
  b8 a16 gis a8 fis gis2 | % m. 18
  fis4. b8 e, gis a cis | % m. 19
  fis,4 gis a r | % m. 20
  r2 r8 d cis8. b16 | % m. 21
  ais8 fis b2 ais8 a!~ | % m. 22
  a8 b a8. gis16 fis2 | % m. 23
  e4. a8 b4. gis8 | % m. 24
  cis4. a8 d2~ | % m. 25
  d8 d cis8. b16 a4 fis'~ | % m. 26
  fis8 e d8. cis16 d8 b e4~ | % m. 27
  e8 fis d b cis4 r8 ais | % m. 28
  b8 d cis8. b16 a!4. d8 | % m. 29
  g,!4. cis8 fis,4 r | % m. 30
  r8 eis fis a dis, fis b4~ | % m. 31
  b8 a16 gis a8 cis fis, a d4~ | % m. 32
  d8 d cis8. b16 a2~ | % m. 33
  a8 gis16 fis gis8 dis e e' dis8. cis16 | % m. 34
  bis8 fis' e dis e4. dis!16 cis | % m. 35
  gis'8 bis, cis2 bis4 | % m. 36
  cis4 r8 cis b16 cis b a b a gis b | % m. 37
  a16 b a gis a gis fis a gis8 cis fis, ais | % m. 38
  b8 fis b2 a!4~ | % m. 39
  a4 gis2 fis4 | % m. 40
  gis8 bis cis e r e dis fis~ | % m. 41
  fis8 bis, cis4~ cis8 fisis, gis4~ | % m. 42
  gis8 ais dis, fisis gis4 r16 a! b gis | % m. 43
  a16 b a gis a gis fis a gis a b gis cis4~ | % m. 44
  cis8 cis b4~ b16 b a gis a b cis a | % m. 45
  d16 e d cis d cis b d cis d cis b cis b a cis | % m. 46
  b2~ b16 gis ais b ais b cis ais | % m. 47
  b16 ais b cis b cis d! b cis b cis d cis d e! cis | % m. 48
  d16 e d cis d cis b d g,!2~ | % m. 49
  g!16 g fis e fis e d fis b,4 cis | % m. 50
  d8 fis b4~ b8 ais16 gis ais4 | % m. 51
  b4 r8 e a,!4~ a8 d | % m. 52
  g,!4~ g8 c! ais fis~ fis16 b cis d | % m. 53
  eis,8 cis' fis,4~ fis8 eis16 fis gis a b gis | % m. 54
  b16 a gis fis d'4~ d8 cis16 b cis4~ | % m. 55
  cis8 fis b,4~ b8 a16 gis a8 fis | % m. 56
  gis2 fis8 cis' fis a | % m. 57
  gis2~ gis8 a! gis fis | % m. 58
  e2~ e8 e dis fis | % m. 59
  bis,4. dis8 gis,4 r8 cis | % m. 60
  b16 cis b a b a gis b a b a gis a gis fis a | % m. 61
  gis16 a gis fis gis fis e gis fis gis fis e fis e dis fis | % m. 62
  e4 r r8 g! fis e | % m. 63
  d4 r r8 fis e d | % m. 64
  cis4~ cis8 cis fis4~ fis8 eis16 fis | % m. 65
  gis8 fis16 gis a8 r r4 r8 cis | % m. 66
  a8 fis d'4~ d8 cis16 b cis4~ | % m. 67
  cis8 fis b,4~ b8 a16 gis a8 fis | % m. 68
  gis2~ gis16 fis8 eis16~ eis fis8 gis16 | % m. 69
  cis,8 fis4 eis8 fis2 \fermata \bar "|." | % m. 70
    
}

mezzo = \relative c' {
  \global
  
  r4 r8 cis a fis d'4~ | % m. 1
  d8 cis16 b cis4~ cis8 fis b,4~ | % m. 2
  b8 a16 gis a8 fis gis2 | % m. 3
  fis8 gis a b cis4 r8 dis | % m. 4
  e4 r8 e ais,4 r8 bis | % m. 5
  cis4 r8 cis~ cis bis16 ais bis8 gis | % m. 6
  e'4. a,8 d4. gis,8 | % m. 7
  cis4 b a gis | % m. 8
  fis4 r r e | % m. 9
  a4 d!8 b gis4 r8 a | % m. 10
  b8 d r gis, a cis r fis, | % m. 11
  gis8 b r a d4 b | % m. 12
  e8 cis dis fis r bis, cis e | % m. 13
  r8 ais,16 bis bis8. \trill ais32 bis cis4 r8 fis | % m. 14
  d!8 b r e cis a r cis | % m. 15
  b8 a gis4 fis r8 \clef treble b' | % m. 16
  e,4. e8 a4. gis16 fis | % m. 17
  gis8 eis fis4~ fis8 e!16 dis e4~ | % m. 18
  e8 d!16 cis d2 cis4~ | % m. 19
  cis8 d b e cis4 r | % m. 20
  r8 cis b8. a16 gis4. cis8 | % m. 21
  fis,8 fis' e8. d16 cis8 g'! fis8. e16 | % m. 22
  dis8 b e2 dis8 d!~ | % m. 23
  d8 cis16 b cis8 fis d b e4~ | % m. 24
  e8 cis fis4~ fis8 a gis8. fis16 | % m. 25
  e8 b' a gis fis e d cis | % m. 26
  b4 fis'~ fis8 d cis8. b16 | % m. 27
  ais8 fis b2 ais8 fis' | % m. 28
  d8 b g'!4~ g8 fis16 e fis4~ | % m. 29
  fis8 b e,4~ e8 d16 cis d8 b | % m. 30
  cis2 b4. gis'8 | % m. 31
  e2 d4. b'8 | % m. 32
  gis8 eis r gis~ gis fis16 eis fis8 dis | % m. 33
  bis2 cis8 r r4 | % m. 34
  r8 dis' cis8. bis!16 cis8 b! a fis | % m. 35
  bis,8 dis gis, gis' fis16 gis fis e fis e dis fis | % m. 36
  e16 fis e dis e dis cis e dis8 gis cis, eis | % m. 37
  fis16 d! cis b cis a d8~ d gis, ais cis | % m. 38
  fis,8 r r fis' e16 fis e dis e dis cis e | % m. 39
  dis16 e dis cis dis cis bis dis cis dis cis bis cis e dis cis | % m. 40
  bis8 dis e gis r gis fis a~ | % m. 41
  a8 gis16 fis gis8 ais dis,16 e dis cis dis cis b! dis | % m. 42
  cis16 dis cis b cis b ais cis b cis dis b e4~ | % m. 43
  e4 dis e r16 b' a gis | % m. 44
  fis4. fis8 e4 r | % m. 45
  r8 a gis4~ gis8 cis, fis4~ | % m. 46
  fis16 a gis fis gis fis eis gis fis8 cis d4~ | % m. 47
  d8 dis e4~ e8 eis fis4~ | % m. 48
  fis2~ fis16 fis e d e d cis e | % m. 49
  \clef bass a,2~ a16 a g! fis g fis e g | % m. 50
  fis8 r r \clef treble b e4 r8 fis | % m. 51
  d8 b g'!4~ g8 fis16 e fis4~ | % m. 52
  fis8 b e,4~ e8 d16 cis d8 b | % m. 53
  cis2 b4 r | % m. 54
  r4 r16 cis' b a gis4~ gis16 b a gis | % m. 55
  fis8 a gis8. fis16 eis8 cis fis4~ | % m. 56
  fis4 eis fis r | % m. 57
  r8 gis cis e dis2~ | % m. 58
  dis8 gis, cis b a2~ | % m. 59
  a8 dis, gis fis~ fis16 fis e dis cis dis e cis | % m. 60
  dis4 r8 e cis e dis8. cis16 | % m. 61
  bis8 gis cis2 bis4 | % m. 62
  cis8 gis cis e ais,4 b!8 cis~ | % m. 63
  cis8 fis, b d gis,4 a8 b~ | % m. 64
  b8 a16 gis a4~ a8 b16 cis d4~ | % m. 65
  d16 cis dis eis fis gis a fis b cis b a b a gis a | % m. 66
  fis8 r r fis e16 fis e d e d cis e | % m. 67
  d16 e d cis d cis b d cis d cis b cis b a cis | % m. 68
  b16 cis b a b a gis b a8 b cis d | % m. 69
  a4 gis fis2 \fermata \bar "|." | % m. 70
  
}

bass = \relative c {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  R1 | % m. 5
  R1 | % m. 6
  R1 | % m. 7
  r2 r4 r8 cis | % m. 8
  a8 fis d'4~ d8 cis16 b cis4~ | % m. 9
  cis8 fis b,4~ b8 a16 gis a8 fis | % m. 10
  gis2 fis4 d'~ | % m. 11
  d4 cis b r8 e | % m. 12
  cis8 a fis'2 e4 | % m. 13
  dis4 r8 gis e cis a'!4~ | % m. 14
  a4 gis~ gis8 cis, fis4~ | % m. 15
  fis4 eis fis8 a b d | % m. 16
  gis,8 e a cis fis, dis gis b | % m. 17
  eis,8 cis fis a b gis cis b | % m. 18
  ais8 fis b a! gis e fis a | % m. 19
  d,8 b e e, a a' gis8. fis16 | % m. 20
  eis8 cis fis2 eis8 e!~ | % m. 21
  e8 d cis b e cis fis fis, | % m. 22
  b4 r8 cis a cis' b8. a16 | % m. 23
  gis8 e a2 gis8 e | % m. 24
  a4. fis8 b4. gis8 | % m. 25
  cis2~ cis8 cis b8. a16 | % m. 26
  gis4 ais b g! | % m. 27
  fis4 r8 g! e cis fis4~ | % m. 28
  fis4 e8 d cis a d cis | % m. 29
  b8 d cis8. b16 ais8 fis b4~ | % m. 30
  b8 a!16 gis a4~ a8 gis16 fis gis8 e! | % m. 31
  cis'8 cis, cis'4~ cis8 b16 a b8 gis | % m. 32
  eis8 gis' eis cis fis gis a fis | % m. 33
  gis8 gis, r gis' e cis a'4~ | % m. 34
  a8 gis16 fis gis4~ gis8 cis fis,4~ | % m. 35
  fis8 e16 dis e8 cis dis4 gis | % m. 36
  cis,4 r r2 | % m. 37
  r4 r8 fis e16 fis e d e d cis e | % m. 38
  d16 e d cis d cis b d cis8 fis, fis' a | % m. 39
  b,8 b' e, gis a, gis a a' | % m. 40
  gis16 a gis fis gis fis e gis fis gis fis e fis e dis fis | % m. 41
  e16 fis e dis e dis cis e fisis,8 ais b! e | % m. 42
  ais,8 cis dis dis, gis gis'16 fis gis8 cis, | % m. 43
  fis,8 fis'16 e fis8 b, e16 fis gis e a b cis a | % m. 44
  d16 e d cis d cis b d cis d cis b cis b a cis | % m. 45
  b4. e8 a,4. d8 | % m. 46
  gis,8 b eis, cis fis4. fis8 | % m. 47
  g!4. gis8 a4. ais8 | % m. 48
  b8 fis b, d e fis g! e | % m. 49
  cis8 a d fis, g! b e, a | % m. 50
  d16 cis d e d g! fis g cis, b cis d cis fis e fis | % m. 51
  b,16 cis d b e d cis b cis e d cis d e fis d | % m. 52
  e16 d e fis g! fis e g fis gis ais fis b b, b'8~ | % m. 53
  b16 a! gis b a gis fis a gis a gis fis eis fis gis eis | % m. 54
  fis8 gis16 a b a gis fis e fis e d e d cis e | % m. 55
  d16 e d cis d cis b d cis d cis b cis b a cis | % m. 56
  b16 cis b a b a gis b a gis fis gis a b cis dis | % m. 57
  e16 dis cis dis e fis gis ais bis ais bis cis bis cis dis bis | % m. 58
  cis16 dis e dis cis b! a gis fis e fis gis fis gis a fis | % m. 59
  gis16 fis gis a gis fis e dis e8 cis a'4~ | % m. 60
  a8 gis16 fis gis4~ gis8 cis fis,4~ | % m. 61
  fis8 e16 dis e8 cis dis2 | % m. 62
  cis16 bis cis dis e fis g! e fis g fis e d! cis b! ais | % m. 63
  b16 fis b cis d e fis d e fis e d cis b a gis | % m. 64
  a16 e a b cis d e cis d e d cis b a gis fis | % m. 65
  eis8 cis fis2 eis4 | % m. 66
  fis16 fis' gis a b cis b a gis4 r8 ais | % m. 67
  b8 a! gis8. fis16 eis8 cis fis4~ | % m. 68
  fis4 eis fis8 d a b | % m. 69
  cis4 cis, fis2 \fermata \bar "|." | % m. 70
   
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
      { \clef bass \mezzo }
    
    \new Staff = "bass"
      { \clef bass \bass }
      
  >>
  
\layout {
  indent = 0.0
  }

}
