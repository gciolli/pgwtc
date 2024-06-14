\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue VIII"
  subtitle = "in 3 voices"
  opus = "BWV 853"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key dis \minor
  \time 4/4
}

soprano = \relative c'' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  r2 ais4 dis~ | % m. 3
  dis8 fis eis dis cis dis eis4 | % m. 4
  ais,4 dis4. cis8 bis4 | % m. 5
  ais8 bis cis4~ cis4. b!8 | % m. 6
  ais4 dis~ dis8 cisis dis eis | % m. 7
  fis4 eis8 fis gis eis fis gis | % m. 8
  ais8 gis fis eis fis4 eis8 dis | % m. 9
  cisis8 dis4 cisis8 dis cis b4~ | % m. 10
  b8 ais16 gis ais8 b16 cis dis8 eis16 fis eis8. dis16 | % m. 11
  cisis4 dis8 cis b ais gis fis | % m. 12
  eis8 fis gis4~ gis8 fis16 eis fis8 gis | % m. 13
  ais4 b8 cis16 dis gis,4 r8 ais | % m. 14
  b8 cis dis eis fis gis ais b~ | % m. 15
  b8 cis, fis2 eis4~ | % m. 16
  eis4 dis8 cis bis cisis dis eis | % m. 17
  fis4 eis8 dis cis dis eis fis | % m. 18
  cis4 bis ais r | % m. 19
  ais4 eis'~ eis8 fis eis dis | % m. 20
  cis8 dis eis4 ais, r8 dis~ | % m. 21
  dis8 cis bis4 ais r8 gis'~ | % m. 22
  gis8 fis eis4~ eis16 dis eis fis dis8. eis16 | % m. 23
  eis4 ais~ ais8 b ais \once\override Accidental #'restore-first = ##t gis! | % m. 24
  fisis8 gis ais4 dis, gis~ | % m. 25
  gis8 fis eis2 dis4~ | % m. 26
  dis4 gis4. ais8 gis fis | % m. 27
  eis8 fis gis4 cis, fis~ | % m. 28
  fis8 e! dis gis cis, fis4 eis8 | % m. 29
  fis4 cis~ cis8 b cis dis | % m. 30
  eis8 dis cis4 gis' cis,~ | % m. 31
  cis8 dis eis4 fis2~ | % m. 32
  fis8 eis dis cis bis4 cisis | % m. 33
  dis2 cis | % m. 34
  b8 cis dis e! ais, b cis dis | % m. 35
  b8 ais gis fis eis4 fis8 gis | % m. 36
  ais4 bis8 cisis dis4. cis!8 | % m. 37
  bis8 cisis dis2 cisis4 | % m. 38
  dis4 cisis8 dis eis gis fis eis | % m. 39
  dis8 \once\override Accidental #'restore-first = ##t cis! b ais16 b ais8 bis cisis dis | % m. 40
  eis8 ais, ais' gis fis gis ais b | % m. 41
  eis,4 fisis gis8 fis! eis dis | % m. 42
  cisis4 dis~ dis8 cis! b ais | % m. 43
  b8 ais gis4~ gis8 fis16 eis fis8 gis | % m. 44
  ais4 dis,~ dis8 cisis dis eis8 | % m. 45
  fis8 eis dis4 ais' dis,~ | % m. 46
  dis8 eis fisis4 gis gis' | % m. 47
  dis4. cis8 dis4. eis8 | % m. 48
  fis4. eis8 dis4 ais' | % m. 49
  dis,4. dis8 cisis dis eis cisis | % m. 50
  dis8 eis fis gis ais ais, dis eis | % m. 51
  cisis4 r ais dis~ | % m. 52
  dis8 e! dis cis b cis dis4 | % m. 53
  gis,8 ais16 b ais8 b cis4 fis,~ | % m. 54
  fis8 eis fis gis ais gis fis4 | % m. 55
  e'!2~ e8 cisis dis4~ | % m. 56
  dis8 fis eis! dis cisis eis ais4~ | % m. 57
  ais8 b ais gis fis gis ais4 | % m. 58
  dis,4 gis~ gis8 fis eis4 | % m. 59
  dis4 cis b ais | % m. 60
  gis4 fis eis r8 bis' | % m. 61
  cisis8 dis eis4~ eis8 dis \once\override Accidental #'restore-first = ##t cis!16 b cis fisis | % m. 62
  gis16 ais b4 ais16 gis ais8 dis, eis fisis | % m. 63
  gis4 r ais dis,~ | % m. 64
  dis8 b cis dis e! dis cis4 | % m. 65
  gis'4 dis~ dis8 eis fisis4 | % m. 66
  gis8 ais gis \once\override Accidental #'restore-first = ##t fis! eis fis gis4~ | % m. 67
  gis8 fis eis fis16 gis ais gis ais4 gis16 fis | % m. 68
  eis8 fis4 eis8 fis cis fis4~ | % m. 69
  fis8 gis fis e! dis e fis4 | % m. 70
  b,4 e!~ e8 dis cis4 | % m. 71
  b4 e!~ e8 cisis dis4~ | % m. 72
  dis4 gis~ gis8 gis fisis fis!~ | % m. 73
  fis8 fis eis e! dis4 cis | % m. 74
  b4. b8 ais4. fis'8 | % m. 75
  gis,2~ gis8 fis16 eis fis8 gis | % m. 76
  ais4 r eis'2 | % m. 77
  ais2. b4 | % m. 78
  ais4 gis fis gis | % m. 79
  ais2 dis, | % m. 80
  gis2. fis4 | % m. 81
  e!2 dis~ | % m. 82
  dis8 b gis4~ gis8 ais b4~ | % m. 83
  b8 gis eis4~ eis8 fis gis4~ | % m. 84
  gis8 eis cisis4~ cisis8 cisis dis eis | % m. 85
  fis8 fisis gis ais b bis cisis dis | % m. 86
  dis4 cisis dis2 \fermata \bar "|." | % m. 87
   
}

mezzo = \relative c' {
  \global
  
  dis4 ais'4. b8 ais gis | % m. 1
  fis8 gis ais4 dis, gis~ | % m. 2
  gis8 fis eis4 dis4. eis8 | % m. 3
  fis4 gis ais4. gis8 | % m. 4
  fis8 eis fis eis16 dis eis8 ais4 gisis8 | % m. 5
  ais4~ ais8 \once\override Accidental #'restore-first = ##t gis! fisis dis gis4~ | % m. 6
  gis8 \once\override Accidental #'restore-first = ##t fis! eis dis eis4 fis8 gis | % m. 7
  ais8 bis cisis dis eis cisis dis eis | % m. 8
  fis8 eis dis cisis dis cis! b4 | % m. 9
  ais4 gis fis gis8 fis | % m. 10
  eis4. dis16 eis fis8 gis16 ais gis8. fis32 gis | % m. 11
  ais8 gis fis eis dis4 cisis8 dis | % m. 12
  eis8 dis cisis bis cisis4 dis8 eis | % m. 13
  fis4 gis8 fis eis cis fis4~ | % m. 14
  fis4 eis2 dis4 | % m. 15
  cis8 eis fis gis ais b cis b | % m. 16
  ais8 gis fis eis dis eis fis ais | % m. 17
  dis8 cis bis2 ais4~ | % m. 18
  ais4 gisis ais, eis'~ | % m. 19
  eis8 fis eis dis cis dis eis4 | % m. 20
  ais,4 r8 dis~ dis cis bis4 | % m. 21
  ais r8 gis'~ gis fis eis4 | % m. 22
  dis4 r8 cis ais' gisis ais4 | % m. 23
  gisis4 ais dis4. eis8 | % m. 24
  dis4. cis8 bis4. cis8 | % m. 25
  dis4 gis, cis2 | % m. 26
  bis4 r gis cis~ | % m. 27
  cis8 dis cis b! ais b cis4 | % m. 28
  fis,4 b~ b8 ais gis4 | % m. 29
  fis8 eis fis gis ais gis ais bis | % m. 30
  cis8 b!4 ais8 b ais gis b | % m. 31
  ais2~ ais8 bis cis dis | % m. 32
  gis,4 ais~ ais8 gis fis eis | % m. 33
  dis8 fis b4~ b8 ais gis fisis | % m. 34
  gis2. fisis4 | % m. 35
  gis4 dis~ dis8 cisis dis eis | % m. 36
  fis8 eis dis4 ais' dis,~ | % m. 37
  dis8 eis fis4 gis2 | % m. 38
  fis8 gis ais bis cisis! ais, bis cisis | % m. 39
  dis4. eis8 fis4 eis8 dis | % m. 40
  cisis8 dis eis4~ eis8 dis16 cisis dis4~ | % m. 41
  dis8 \once\override Accidental #'restore-first = ##t cis! b ais b dis gis4~ | % m. 42
  gis8 fis eis dis eis4 fisis | % m. 43
  gis8 \once\override Accidental #'restore-first = ##t fis! eis dis cisis4 dis8 eis | % m. 44
  fis8 eis fis gis ais gis fis eis16 dis | % m. 45
  cisis8 gis'4 fis16 eis fisis8 gis ais cis,! | % m. 46
  b8 cis dis e! dis4 gis,~ | % m. 47
  gis8 fisis gis ais b ais gis4 | % m. 48
  dis'4 ais~ ais8 b \once\override Accidental #'restore-first = ##t cis!4~ | % m. 49
  cis8 cis' b ais gis fis gis eis | % m. 50
  fis8 gis ais gis~ gis fis16 eis fis8 gis | % m. 51
  ais4 ais, dis4. e!8 | % m. 52
  dis8 cis b cis dis4 \clef bass gis,8 fis | % m. 53
  eis4 cis' fis,4. eis8 | % m. 54
  fis8 gis ais gis fis4 b8 ais | % m. 55
  gis4 \clef treble cis' fis,2 | % m. 56
  b8 ais gis fis eis dis cisis eis | % m. 57
  ais,8 gis' fis eis dis4 dis'8 cis | % m. 58
  b8 ais gis b ais dis4cis8~ | % m. 59
  cis8 b4 ais gis fis8~ | % m. 60
  fis8 eis4 dis8 cisis eis ais4~ | % m. 61
  ais8 b! ais gis fis gis ais4 | % m. 62
  dis,4 gis~ gis8 fisis eis e! | % m. 63
  dis8 b' ais gis fisis dis16 eis fisis8 gis16 ais | % m. 64
  b4. ais8 gis fisis gis ais | % m. 65
  dis,8 eis fisis gis ais16 b cis4 b16 ais | % m. 66
  b4 r gis2 | % m. 67
  cis2. dis4 | % m. 68
  cis4 b ais b | % m. 69
  cis2 fis, | % m. 70
  b2. ais4 | % m. 71
  gis2 fis4 b~ | % m. 72
  b8 dis \once\override Accidental #'restore-first = ##t cis! b ais b cis4 | % m. 73
  gis4 cis~ cis8 b ais4~ | % m. 74
  ais8 dis, gis4~ gis8 gis fis4~ | % m. 75
  fis8 fis e! dis cisis4 dis~ | % m. 76
  dis8 cisis16 bis cisis8 eis ais4. b!8 | % m. 77
  ais4. gis8 fis4. gis8 | % m. 78
  ais2 dis,4 gis~ | % m. 79
  gis8 fis eis4 fisis8 ais, dis4~ | % m. 80
  dis8 e! dis cis b cis dis4 | % m. 81
  \clef bass gis,4 cis~ cis8 b ais4 | % m. 82
  gis4. ais8 b ais gis fis | % m. 83
  eis4. fis8 gis fis eis dis | % m. 84
  cisis4. dis8 eis4 fis8 gis | % m. 85
  \clef treble ais8 ais b cis dis dis eis \once\override Accidental #'restore-first = ##t fis! | % m. 86
  eis8 b' ais gis fisis2 \fermata \bar "|." | % m. 87
  
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
  dis4 ais'4. b8 ais gis | % m. 8
  fis8 gis ais4 dis, gis~ | % m. 9
  gis8 fis eis4 dis dis' | % m. 10
  cisis4 cis! bis b! | % m. 11
  ais8 ais, dis4~ dis8 fis eis dis | % m. 12
  cisis8 dis eis4 ais, dis~ | % m. 13
  dis8 cis b2 ais4 | % m. 14
  gis8 ais b cis dis eis fis gis | % m. 15
  ais8 gis ais b cis4 cis, | % m. 16
  fis,4 fis'~ fis8 eis dis cis | % m. 17
  bis8 ais gisis eis ais bis cis dis | % m. 18
  eis8 dis eis eis, ais gisis ais bis | % m. 19
  cis8 bis cis dis eis dis cis dis16 eis | % m. 20
  fis16 eis fis8 gisis,4 ais bis | % m. 21
  cis8 ais eis' dis cisis dis4 cisis8 | % m. 22
  dis8 bis' gisis ais fis eis fis4 | % m. 23
  eis4 r fis b~ | % m. 24
  b8 cis b ais gis fisis gis ais | % m. 25
  bis4 cis8 b! ais gis fisis dis | % m. 26
  gis8 ais gis \once\override Accidental #'restore-first = ##t fis! eis fis eis dis | % m. 27
  cis8 b' ais gis fis gis fis e! | % m. 28
  dis8 cis b gis ais fis cis' cis, | % m. 29
  fis8 gis ais gis fis fis' eis dis | % m. 30
  cis4 fis2 eis4 | % m. 31
  fis8 eis dis cisis dis4 ais8 bis | % m. 32
  cis4 fis, gis ais | % m. 33
  b8 ais gis fis e!4 dis | % m. 34
  gis8 ais b cis dis4 r8 dis | % m. 35
  gis8 ais b gis ais gis fis eis | % m. 36
  dis4 gis4. fisis16 eis fisis4 | % m. 37
  gis8 \once\override Accidental #'restore-first = ##t fis! eis dis eis fis gis ais | % m. 38
  dis,16 dis' cis b ais4~ ais8 fis gis ais | % m. 39
  b!8 ais gis4 dis' ais~ | % m. 40
  ais8 bis cisis4 dis8 b! fis gis | % m. 41
  ais4 dis, gis8 ais b4 | % m. 42
  ais2 r | % m. 43
  r2 ais,4 dis,~ | % m. 44
  dis8 cisis dis eis fis eis dis4 | % m. 45
  ais'4 dis,~ dis8 eis fisis4 | % m. 46
  gis4 ais b8 cis b ais | % m. 47
  gis8 ais b e?16 cis gis'4 b,8 cis | % m. 48
  dis8 cisis dis eis fis gis eis fisis | % m. 49
  gis8 ais gis \once\override Accidental #'restore-first = ##t fis! eis dis cisis b' | % m. 50
  ais8 gis fis eis dis \once\override Accidental #'restore-first = ##t cis! b4 | % m. 51
  ais4 dis4. e!8 dis cis | % m. 52
  b8 cis dis4 gis,8 ais b4 | % m. 53
  cis4 fis,~ fis8 eis fis gis | % m. 54
  ais8 gis fis e'! dis2~ | % m. 55
  dis8 cis b ais b4 b'8 ais | % m. 56
  gis8 ais b gis ais4. gis8 | % m. 57 
  fis8 gis ais4 dis,8 eis fisis4 | % m. 58
  gis8 \once\override Accidental #'restore-first = ##t fis! eis dis cisis dis ais ais' | % m. 59
  b8 dis , eis fisis gis eis cisis dis | % m. 60
  bis8 cisis dis dis, ais'4 r | % m. 61
  ais'2 dis~ | % m. 62
  dis4 e! dis cis | % m. 63
  b4 cis dis2 | % m. 64
  gis,2 cis~ | % m. 65
  cis4 b ais2 | % m. 66
  gis4 cis~ cis8 dis cis b | % m. 67
  ais8 b cis4 fis, b~ | % m. 68
  b8 ais gis4 fis8 e! dis16 cis dis e | % m. 69
  ais,8 fis gis ais b cis dis e!16 fis | % m. 70
  gis8 ais16 b cis,8 dis16 e! fis,8 fis'4 fisis8 | % m. 71
  gis8 fisis gis ais b ais gis fis! | % m. 72
  eis8 fis eis dis cisis cisis'16 dis e!8 ais, | % m. 73
  b8 bis \prallmordent cis ais fisis gis4 fisis8 | % m. 74
  gis8 \once\override Accidental #'restore-first = ##t fis! e! dis cis cisis dis ais | % m. 75
  b8 bis cis gis ais2~ | % m. 76
  ais8 e' ais4~ ais8 b ais gis | % m. 77
  fis8 gis ais4 dis,8 eis16 fis gis4~ | % m. 78
  gis8 fis eis cisis' dis8 eis16 fis eis8 dis | % m. 79
  cisis8 dis4 cis!8~ cis dis16 e! dis8 cis | % m. 80
  b8 cis b ais gis fis e! dis | % m. 81
  cis cis'16 b ais8 gis fisis gis4 fis!8 | % m. 82
  eis2~ eis8 fis eis dis | % m. 83
  cisis2~ cisis8 dis cisis bis | % m. 84
  ais2~ ais8 gis fis eis | % m. 85
  dis8 cis' b ais gis fis' eis dis | % m. 86
  ais'4 ais, dis,2 \fermata \bar "|." | % m. 87 
  
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
