\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue XIII"
  subtitle = "in 3 voices"
  opus = "BWV 882"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key fis \major
  \time 2/2
  \partial 2
}

soprano = \relative c'' {
  \global
  
  r2
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  r2 bis4. \trill ais16 bis | % m. 4
  cis4 r gis ais | % m. 5
  b4. ais16 gis fis8 eis fis gis | % m. 6
  ais8 bis cis ais dis4 fis, | % m. 7
  fis4 eis8 dis cis4 b' | % m. 8
  b4 ais r a! | % m. 9
  a!4 gis r gis8 ais | % m. 10
  b4 dis, eis2 | % m. 11
  fis8 eis fis gis ais gis ais fis | % m. 12
  dis4 gis~ gis8 fis gis eis | % m. 13
  fis8 ais bis cisis dis eis fis dis | % m. 14
  eis2 fisis | % m. 15
  gis4 dis8 cis b4 e!~ | % m. 16
  e4 dis8 cis dis4 gis~ | % m. 17
  gis8 fis eis dis cis b cis ais | % m. 18
  fis4 b~ b8 ais b gis | % m. 19
  ais8 b cis dis eis fis16 eis dis8 eis | % m. 20
  fis4 r cis dis | % m. 21
  e!4. dis16 cis b8 ais! b cis | % m. 22
  dis8 eis! fis dis gis4 b, | % m. 23
  b4 \prall ais fis' ais, | % m. 24
  ais4 \prall gis eis' gis, | % m. 25
  gis4 \prall fis r b | % m. 26
  b4 ais r ais | % m. 27
  ais4 gis gis' bis, | % m. 28
  bis4 ais fisis' ais, | % m. 29
  ais4 gis r cis | % m. 30
  cis4 bis r bis | % m. 31
  bis4 ais dis8 cis dis4 | % m. 32
  gis,4. ais8 b!4 cis | % m. 33
  fis,4. gis8 ais gis ais bis | % m. 34
  cis4 ais bis2 | % m. 35
  cis4 r r2 | % m. 36
  r4 cis8 dis e! dis fis cis | % m. 37
  b8 ais b cis dis cis e! gis, | % m. 38
  fis4 b~ b8 ais gis gis' | % m. 39
  cis,8 eis fis ais cisis,4. \trill bis16 cisis | % m. 40
  dis4 r ais bis | % m. 41
  cis4. bis16 ais gis8 fisis gis ais | % m. 42
  bis8 cisis dis bis eis4 gis, | % m. 43
  gis4 \prall fis8 eis fis4 gis | % m. 44
  eis2 fisis | % m. 45
  gis2 r4 e'!~ | % m. 46
  e4 dis8 cis dis4 gis~ | % m. 47
  gis8 fis eis dis cis bis cis eis | % m. 48
  fis8 b,! b'4~ b8 ais b gis | % m. 49
  ais4 fis b4. fis8 | % m. 50
  gis2 ais | % m. 51
  b4 dis,8 b cis dis e!4 | % m. 52
  e!4 dis r d! | % m. 53
  d!4 cis r cis8 dis | % m. 54
  e!2~ e8 b ais cis~ | % m. 55
  cis8 fis, b ais b4 b | % m. 56
  b4 ais r ais | % m. 57
  ais4 gis e'! gis, | % m. 58
  gis4 fis dis' fis, | % m. 59
  fis4 eis! r cis' | % m. 60
  cis4 bis r bis | % m. 61
  bis4 ais fis' ais, | % m. 62
  ais4 gis eis' gis, | % m. 63
  gis4 fis r b | % m. 64
  b4 ais r a! | % m. 65
  a!4 gis r gis | % m. 66
  fis4 b2 ais8 gis | % m. 67
  fis4 r r2 | % m. 68
  R1 | % m. 69
  R1 | % m. 70
  R1 | % m. 71
  R1 | % m. 72
  R1 | % m. 73
  R1 | % m. 74
  R1 | % m. 75
  r2 eis'4. \trill dis16 eis | % m. 76
  fis4 r cis dis | % m. 77
  e!4. dis16 cis b!8 ais b cis | % m. 78
  dis8 eis! fis dis gis4 b, | % m. 79
  b4 ais ais' cis, | % m. 80
  cis4 \prall b b' dis, | % m. 81
  dis4 cis8 dis eis fis gis eis | % m. 82
  fis4. gis8 eis4. fis8 | % m. 83
  fis2. \fermata \bar "|." | % m. 84
    
}

mezzo = \relative c' {
  \global
  
  eis4. \trill dis16 eis 
  fis4 r cis dis | % m. 1
  e!4. dis16 cis b8 ais b cis | % m. 2
  dis8 eis! fis dis gis4 b, | % m. 3
  b4 \prall ais8 gis fis4 fis' | % m. 4
  fis4 eis r e! | % m. 5
  e!4 dis r dis8 eis! | % m. 6
  fis4 ais, bis2 \trill | % m. 7
  cis4 gis cis2~ | % m. 8
  cis4. dis8 e!4 fis | % m. 9
  b,4. cis8 dis4 eis | % m. 10
  fis4 b,2 ais8 gis | % m. 11
  fis2 r4 cis' | % m. 12
  bis2 cisis | % m. 13
  dis2 r4 b'!~ | % m. 14
  b4 ais8 gis ais4 dis~ | % m. 15
  dis8 cis b ais gis fisis gis e! | % m. 16
  cis4 fis~ fis8 eis! fis dis | % m. 17
  eis4 cis fis e! | % m. 18
  dis2 eis! | % m. 19
  fis4 r gis2 | % m. 20
  cis,4. cis8 fis2 | % m. 21
  b,4. b8 e!2 | % m. 22
  dis2 cis~ | % m. 23
  cis2 r4 fis | % m. 24
  fis4 eis r eis | % m. 25
  eis4 dis b' dis, | % m. 26
  dis4 cis ais' cis, | % m. 27
  cis4 bis r gis' | % m. 28
  gis4 fisis r fisis | % m. 29
  fisis4 eis cis' eis, | % m. 30
  eis4 dis bis'! dis, | % m. 31
  dis4 cis r fis | % m. 32
  fis4 eis r e! | % m. 33
  e!4 dis r dis | % m. 34
  cis4 fis2 eis8 dis | % m. 35
  gis8 fis gis b! eis,4. \trill dis16 eis | % m. 36
  fis4 r cis dis | % m. 37
  e!4. dis16 cis b8 ais! b cis | % m. 38
  dis8 eis! fis dis gis4 b, | % m. 39
  b4 \prall ais r gis' | % m. 40
  gis4 fisis r fis! | % m. 41
  fis4 eis r eis8 fisis! | % m. 42
  gis4 bis, cisis2 | % m. 43
  dis8 ais bis cisis dis cisis dis b! | % m. 44
  gis4 cis~ cis8 b cis ais | % m. 45
  b8 dis eis fisis gis ais b gis | % m. 46
  ais2 bis | % m. 47
  cis4 gis8 fis eis4 ais~ | % m. 48
  ais4 gis8 fis gis4 cis~ | % m. 49
  cis8 b ais gis fis e! fis ais | % m. 50
  b8 e,! e'!4~ e8 dis e cis | % m. 51
  dis b fis gis ais4. \trill gis16 ais | % m. 52
  b4 r fis gis | % m. 53
  a!4. gis16 fis e!8 dis! e fis | % m. 54
  gis8 ais! b gis cis4 e,! | % m. 55
  e!4 dis b' dis, | % m. 56
  dis4 cis ais' cis, | % m. 57
  cis4 b r e! | % m. 58
  e!4 dis r dis | % m. 59
  dis4 cis cis' eis, | % m. 60
  eis4 dis bis'! dis, | % m. 61
  dis4 cis r fis | % m. 62
  fis4 eis r eis | % m. 63
  eis4 dis gis8 fis gis4 | % m. 64
  cis,4. dis8 e!4 fis | % m. 65
  b,4. cis8 dis cis dis eis! | % m. 66
  fis4 dis eis2 | % m. 67
  fis8 eis fis ais cis ais fis e! | % m. 68
  dis8 cis dis fis gis ais b fis | % m. 69
  eis8 dis eis gis bis cis16 bis ais8 bis | % m. 70
  cis4 r gis ais | % m. 71
  b4. ais16 gis fis8 eis fis gis | % m. 72
  ais8 bis cis ais dis4 fis, | % m. 73
  fis4 \prall eis cis' e,! | % m. 74
  e!4 \prall dis b' dis, | % m. 75
  dis4 \prall cis r b' | % m. 76
  b4 ais r a! | % m. 77
  a!4 gis r gis8 ais! | % m. 78
  b4 r eis2 | % m. 79
  fis2 r4 ais, | % m. 80
  ais4 b r b | % m. 81
  bis4 cis r b! | % m. 82
  ais4. b8 gis4. ais8 | % m. 83
  ais2. \fermata \bar "|." | % m. 84
    
}

bass = \relative c {
  \global
  
  r2
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  R1 | % m. 5
  R1 | % m. 6
  R1 | % m. 7
  r2 eis4. \trill dis16 eis | % m. 8
  fis4 r cis dis | % m. 9
  e!4. dis16 cis b8 ais! b cis | % m. 10
  dis8 eis! fis dis gis4 b, | % m. 11
  b4 ais8 gis fis4 fis'~ | % m. 12
  fis4 eis8 dis eis4 ais, | % m. 13
  dis,4 dis'~ dis8 cisis dis b | % m. 14
  gis4 cis~ cis8 b cis ais | % m. 15
  b8 dis eis fisis gis4 cis | % m. 16
  ais2 bis | % m. 17
  cis4 b! ais2~ | % m. 18
  ais4 gis8 fis gis4 cis | % m. 19
  fis,4eis8 dis cis4 b' | % m. 20
  b4 ais r a! | % m. 21
  a!4 gis r gis8 ais | % m. 22
  b4 dis, eis2 | % m. 23
  fis8 eis fis gis ais fis dis fis | % m. 24
  b8 ais b gis cisis bis cisis ais | % m. 25
  dis8 ais dis, cisis dis b gis b | % m. 26
  e!8 dis e cis fisis eis fisis dis | % m. 27
  gis8 fisis gis ais bis ais bis gis | % m. 28
  cis8 bis cis ais disis cisis disis bis | % m. 29
  eis8 bis eis, disis eis cis ais cis | % m. 30
  fis8 eis fis dis gisis fisis gisis eis | % m. 31
  ais8 gisis ais bis bis4. \trill ais16 bis | % m. 32
  cis4 r gis ais | % m. 33
  b4. ais16 gis fis8 eis! fis gis | % m. 34
  ais8 bis cis ais dis4 fis, | % m. 35
  fis4 eis8 dis cis eis gis b | % m. 36
  b4 \prall ais  r a! | % m. 37
  a!4 gis r gis8 ais | % m. 38
  b4 dis, eis2 | % m. 39
  fis4. fis8 eis dis eis ais, | % m. 40
  dis8 cisis dis eis fis dis gisis, gisis' | % m. 41
  ais8 gisis ais bis cis4. bis16 ais | % m. 42
  gis4. fis8 eis dis eis ais | % m. 43
  dis,2 r4 b!~ | % m. 44
  b4 ais8 gis ais4 dis, | % m. 45
  gis4 gis'~ gis8 fisis gis e! | % m. 46
  cis4 fis~ fis8 eis! fis dis | % m. 47
  eis8 gis ais bis cis2 | % m. 48
  dis2 eis | % m. 49
  fis4 e! dis2~ | % m. 50
  dis4 cis8 b cis4 fis, | % m. 51
  b2 r | % m. 52
  r8 ais b cis dis b eis,! eis'! | % m. 53
  fis8 eis, fis gis a!4. gis16 fis | % m. 54
  cis'4 gis ais fis | % m. 55
  b4 b,8 cis dis b gis b | % m. 56
  e!8 dis e cis fisis eis fisis dis | % m. 57
  gis8 fisis gis fisis gis e! cis e | % m. 58
  a!8 gis a fis bis ais bis gis | % m. 59
  cis8 gis cis, dis eis cis ais cis | % m. 60
  fis8 eis fis dis gisis fisis gisis eis | % m. 61
  ais8 gisis ais gisis ais fis dis fis | % m. 62
  b!8 ais b gis cisis bis cisis ais | % m. 63
  dis8 ais dis, eis eis4. \prall dis16 eis | % m. 64
  fis4 r cis dis | % m. 65
  e!4. dis16 cis b8 ais! b cis | % m. 66
  dis8 eis! fis dis gis4 b, | % m. 67
  b4 ais ais' cis, | % m. 68
  cis4 b b' dis, | % m. 69
  dis4 cis8 dis dis4. \trill cis16 dis | % m. 70
  eis8 gis ais b cis e,! dis cis | % m. 71
  dis8 fis gis ais b cis16 b ais8 b | % m. 72
  cis8 dis eis fis eis dis cis bis | % m. 73
  cis8 cis, cis' b! ais b cis ais | % m. 74
  b8 b, b' ais gis ais b gis | % m. 75
  eis8 fis gis eis cis eis gis cis | % m. 76
  fis,8 eis fis gis a! fis bis, bis' | % m. 77
  cis8 bis cis dis e! e,! dis cis | % m. 78
  b8 cis b ais gis fis gis cis | % m. 79
  fis,8 eis fis ais cis ais fis' eis | % m. 80
  dis8 cis dis fis gis dis b' gis | % m. 81
  eis8 dis eis gis cis gis eis' cis | % m. 82
  dis4 b cis cis, | % m. 83
  \set Score.measureLength = #(ly:make-moment 3 4 )
  fis,2. \fermata \bar "|." | % m. 84
    
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
