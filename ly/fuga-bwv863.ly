\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue XVIII"
  subtitle = "in 4 voices"
  opus = "BWV 863"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key gis \minor
  \time 4/4
}

soprano = \relative c'' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  r4 gis fisis8 gis16 ais b8 ais | % m. 5
  gis8 cisis dis fis, gis gis ais ais | % m. 6
  dis,8 eis fis4~ fis8 e!16 fis gis8 fisis16 gis | % m. 7
  ais8 gis16 ais b8 gis ais r fisis r | % m. 8
  gis8 ais b4 cis8 r ais r | % m. 9
  b8 dis16 cisis dis4 eis8 r cisis r | % m. 10
  dis8 eis fis4~ fis8 e!16 fis gis8 fisis16 gis | % m. 11
  ais8 gis16 ais b8 gis ais r fisis r | % m. 12
  gis8 \once \override Accidental #'restore-first = ##t fis!16 e dis8 e fis r fis r | % m. 13
  e8 dis16 cis bis8 cis dis r bis r | % m. 14
  cis4 r r8 cis4 dis8 | % m. 15
  e4 dis cis2~ | % m. 16
  cis8 fisis gis b, ais fisis' gis cis, | % m. 17
  b8 gis' \once \override Accidental #'restore-first = ##t fis! ais, gis fis' eis gis, | % m. 18
  fis4 r r2 | % m. 19
  R1 | % m. 20
  R1 | % m. 21
  R1 | % m. 22
  R1 | % m. 23
  r4 dis' cisis8 dis16 eis fis!8 eis | % m. 24
  dis8 gisis ais cis, dis dis eis eis | % m. 25
  ais,8 eis' fis4~ fis8 ais b4~ | % m. 26
  b8 e, dis4 cis2~ | % m. 27
  cis4 b~ b8 ais16 gis ais fis bis8 | % m. 28
  cis2~ cis8 b16 ais b gis cisis8 | % m. 29
  dis8 \once \override Accidental #'restore-first = ##t cis!16 b ais8 b cis r ais r | % m. 30
  b8 ais16 gis fisis8 gis ais r fisis r | % m. 31
  gis4 r r8 gis4 ais8 | % m. 32
  b4 ais gis2~ | % m. 33
  gis4 fis e2~ | % m. 34
  e8 dis b'2 a!4~ | % m. 35
  a4 r8 gis ais ais bis bis | % m. 36
  cis4 r8 cis bis cis16 dis e8 dis | % m. 37
  cis8 fisis gis b, cis cis dis dis | % m. 38
  gis,8 bis cis2 b!4~ | % m. 39
  b4 ais8 gis fisis gis4 fisis8 | % m. 40
  gis1 \fermata \bar "|." | % m. 41
    
}

alto = \relative c' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  r4 dis bis8 cis16 dis e8 dis | % m. 3
  cis8 fisis gis b, cis cis dis dis | % m. 4
  gis,4 cis~ cis8 b16 cis dis8 cisis16 dis | % m. 5
  eis8 dis16 eis fis8 dis eis r cisis r | % m. 6
  dis4 r8 ais gis4 r8 dis' | % m. 7
  e8 dis r dis e r dis r | % m. 8
  dis4 r8 fis gis r fis r | % m. 9
  fis4 r8 ais b r ais r | % m. 10
  fis8 gis ais b16 ais gis8 cis4 dis8 | % m. 11
  e4 dis cis2~ | % m. 12
  cis4 b a!2~ | % m. 13
  a4 gis fis2~ | % m. 14
  fis8 e16 fis gis8 fis16 e dis8 e16 fis gis8 fisis16 gis | % m. 15
  ais8 gis16 ais b8 gis ais r fisis r | % m. 16
  gis8 r r4 r r8 fisis | % m. 17
  gis4 r8 \once \override Accidental #'restore-first = ##t fis! eis4 r8 eis~ | % m. 18
  eis8 dis16 cisis dis4 bis8 cis16 dis e!8 dis | % m. 19
  cis8 fisis gis b, cis cis dis dis | % m. 20
  gis,8 fisis' gis4~ gis8 fis16 eis fis dis gisis8 | % m. 21
  ais2~ ais8 gisis16 fisis gisis eis ais8 | % m. 22
  bis2~ bis8 ais16 gisis ais bis cis8~ | % m. 23
  cis8 bis16 ais gisis ais fisis gisis ais2~ | % m. 24
  ais8 bis cis ais b!4~ b16 ais, b cis | % m. 25
  dis4 cis8 ais' b4 r16 dis e! fis | % m. 26
  gis8 r r fis, gis ais16 b e,4~ | % m. 27
  e8 dis16 cis dis b eis8 fis2~ | % m. 28
  fis8 e16 dis e cis fisis8 gis2~ | % m. 29
  gis4 fis e2~ | % m. 30
  e4 dis cis2~ | % m. 31
  cis8 b16 cis dis8 cis16 b ais8 bis16 cis dis8 cisis16 dis | % m. 32
  eis8 dis16 eis fis8 dis eis r cisis r | % m. 33
  dis8 \once \override Accidental #'restore-first = ##t cis!16 b ais8 b cis r ais r | % m. 34
  b4 r8 b cis cis dis dis | % m. 35
  e16 b cis dis e8 gis fis2~ | % m. 36
  fis16 dis e fis gis8 fis16 e dis8 gis4 fisis16 gis | % m. 37
  ais8 gis16 ais b8 gis a! r a r | % m. 38
  dis,8 r r gis ais! r gis fis! | % m. 39
  e8 dis cis4~ cis8 b ais4 | % m. 40
  bis1 \fermata \bar  "|." | % m. 41
    
}

tenor = \relative c' {
  \global
  
  r4 gis fisis8 gis16 ais b8 ais | % m. 1
  gis8 cisis dis fis, gis gis ais ais | % m. 2
  dis,8 eis fis4~ fis8 e!16 fis gis8 fisis16 gis | % m. 3
  ais8 gis16 ais b8 gis ais r fisis r | % m. 4
  gis8 \once \override Accidental #'restore-first = ##t fis! e4 dis8 gis4 ais8 | % m. 5
  b4 ais gis2~ | % m. 6
  gis8 fis16 gis ais8 fis dis e r b'! | % m. 7
  cis4 r8 dis cis r ais r | % m. 8
  b4 r8 b e r cis r | % m. 9
  dis4 r8 dis gis r eis r | % m. 10
  dis8 r dis4 bis8 cis16 dis e8 dis | % m. 11
  cis8 fisis gis b, cis cis dis dis | % m. 12
  gis,4 r8 gis a! a b b | % m. 13
  e,4 r8 e fis fis gis gis | % m. 14
  cis,8 dis e fis gis4. b!8 | % m. 15
  cis4 b8 dis e r dis r | % m. 16
  dis8 r gis,4 fisis8 gis16 ais b8 ais | % m. 17
  gis8 cisis dis fis, gis gis ais ais | % m. 18
  dis,8 eis fis4~ fis8 e!16 fis gis8 fisis16 gis | % m. 19
  ais8 gis16 ais b8 gis ais r fisis r | % m. 20
  gis4 r8 b ais gis ais bis | % m. 21
  eis,8 dis' eis fisis bis, ais bis cis | % m. 22
  dis2~ dis8 cis16 bis cis8 ais~ | % m. 23
  ais16 ais dis cis bis ais bis8 ais bis16 cisis dis8 eis | % m. 24
  \clef treble fis4~ fis16 gis fis eis dis cis b! ais gis4 | % m. 25
  fis8 gis ais cis dis2 | % m. 26
  cis4. b8 b r ais r | % m. 27
  b4 r r2 | % m. 28
  R1 | % m. 29
  R1 | % m. 30
  R1 | % m. 31
  r4 \clef bass gis fisis8 gis16 ais bis8 ais | % m. 32
  gis8 cisis dis fis, gis gis ais ais | % m. 33
  dis,4 r8 dis e e fis fis | % m. 34
  b,16 dis e fis gis2 fis4 | % m. 35
  e4 r8 b' cis cis dis dis | % m. 36
  gis,2~ gis8 ais16 bis cis8 dis | % m. 37
  e4 dis e8 r fis r | % m. 38
  bis,8 r r e dis r dis4 | % m. 39
  cis4 r8 cis, dis2 | % m. 40
  dis1 \fermata \bar "|." | % m. 41
    
}

bass = \relative c {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  R1 | % m. 5
  R1 | % m. 6
  r4 dis bis8 cis16 dis e8 dis | % m. 7
  cis8 fisis gis b, cis cis dis dis | % m. 8
  gis,4 r8 dis' e e fis fis | % m. 9
  b,4 r8 fis' gis gis ais ais | % m. 10
  dis,8 r r4 r2 | % m. 11
  R1 | % m. 12
  R1 | % m. 13
  R1 | % m. 14
  r4 cis bis8 cis16 dis e8 dis | % m. 15
  cis8 fisis gis b, cis cis dis dis | % m. 16
  gis,8 ais b cis dis4 r8 dis | % m. 17
  eis4 dis2 cisis4 | % m. 18
  dis4 r16 dis, e! fis gis8 cis4 dis8 | % m. 19
  e4 dis cis2~ | % m. 20
  cis8 b16 ais b gis cisis8 dis2~ | % m. 21
  dis8 cis16 bis cis ais disis8 eis!2~ | % m. 22
  eis8 dis16 cis dis bis eis8 fis4~ fis8. eis16 | % m. 23
  dis4 eis ais, r | % m. 24
  R1 | % m. 25
  r4 fis' dis8 e!16 fis gis8 fis | % m. 26
  e8 ais b dis, e e fis fis | % m. 27
  b,8 ais gis cis fis, e! dis gis | % m. 28
  cis8 b ais dis gis, fis eis ais | % m. 29
  dis,4 r8 dis' e e fis fis | % m. 30
  b,4 r8 b cis cis dis dis | % m. 31
  gis,8 ais b cis dis fis~ fis16 gis fis eis | % m. 32
  dis16 cisis bis ais gis fis gis ais b!4 ais8 gis | % m. 33
  fis8 ais dis,4 r2 | % m. 34
  r4 r8 gis a! a b b | % m. 35
  e,4 e'2 dis4 | % m. 36
  cis8 dis e fis gis2~ | % m. 37
  gis2. fis4~ | % m. 38
  fis4 r8 e fisis, fisis gis gis | % m. 39
  cis,8 dis e4 dis2 | % m. 40
  gis1 \fermata \bar "|." | % m. 41
    
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
