\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue XVIII"
  subtitle = "in 3 voices"
  opus = "BWV 887"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key gis \minor
  \numericTimeSignature
  \time 6/8
}

soprano = \relative c'' {
  \global
  
  gis8 ais b ais dis dis, | % m. 1
  gis8 b ais b ais gis | % m. 2
  ais b cis b e gis, | % m. 3
  ais8 cis b cis b ais | % m. 4
  b4 bis8 cis4.~ | % m. 5
  cis4. bis | % m. 6
  cis4 cisis8 dis4.~ | % m. 7
  dis4. cisis | % m. 8
  dis8 cis4~ cis8 ais bis | % m. 9
  cis8 b!4~ b8 gis ais | % m. 10
  b4.~ b8 ais cis | % m. 11
  fisis,8 dis' gis, cis4.~ | % m. 12
  cis8 e dis cis b ais | % m. 13
  b4 dis8 gis4.~ | % m. 14
  gis4 fisis8 gis4 b8 | % m. 15
  cis,4 dis8 e dis cis | % m. 16
  dis4. cis | % m. 17
  b4. ais | % m. 18
  gis8 r r r4. | % m. 19
  r8 r dis' dis4 cisis8 | % m. 20
  dis4 r8 r4. | % m. 21
  r8 r e dis4 cis8 | % m. 22
  b4 gis8 ais4 bis8 | % m. 23
  cis8 gis cis~ cis ais bis | % m. 24
  e4. dis~ | % m. 25
  dis8 cis a'!~ a fisis gis | % m. 26
  cis,2. | % m. 27
  b2. | % m. 28
  ais4.~ ais4 cisis8 | % m. 29
  dis8 eis fis~ fis eis gis | % m. 30
  cisis,8 eis ais, dis4.~ | % m. 31
  dis4. cisis | % m. 32
  dis4 r8 gis,4 r8 | % m. 33
  fis4 r8 r4. | % m. 34
  R2. | % m. 35
  R2. | % m. 36
  r4. r8 r e | % m. 37
  fis8 a! gis a gis fis | % m. 38
  e8 gis16 fis gis ais! b4.~ | % m. 39
  b4. ais | % m. 40
  b4. cis | % m. 41
  dis4. e8 dis cis | % m. 42
  b8 dis fis~ fis dis eis~ | % m. 43
  eis8 ais, dis~ dis bis cisis | % m. 44
  dis8 eis fis eis ais ais, | % m. 45
  dis8 fis eis fis eis dis | % m. 46
  eis8 fis gis fis b dis, | % m. 47
  eis8 gis fis gis fis eis | % m. 48
  fis4 fisis8 gis4 fis8 | % m. 49
  e4 eis8 fis4 e!8 | % m. 50
  dis4. cis~ | % m. 51
  cis4 b8~ b ais cis | % m. 52
  fisis,4 ais8~ ais dis, gis~ | % m. 53
  gis4 cis8 fisis,4. | % m. 54
  gis4 dis8 cis b ais | % m. 55
  b4 r8 r r b' | % m. 56
  cis8 dis e dis cis b | % m. 57
  cis8 ais4~ ais dis8 | % m. 58
  dis2.~ | % m. 59
  dis4. cisis | % m. 60
  dis4 dis8 cisis4 cis8 | % m. 61
  bis4 b!8 ais4 bis8 | % m. 62
  cis4 cisis8 dis4.~ | % m. 63
  dis4. cisis \prall | % m. 64
  dis4 \once \override Accidental #'restore-first = ##t cis!8 b4 ais8 | % m. 65
  gis4 cis8~ cis ais bis | % m. 66
  cis8 gis ais b! cis b | % m. 67
  ais8 dis cis b e dis | % m. 68
  cis8 dis e ais, dis cis | % m. 69
  b8 cis dis gis, cis dis | % m. 70
  e4. dis | % m. 71
  cis4. b8 gis'4 | % m. 72
  fis8 e dis~ dis cis16 bis cis8 | % m. 73
  dis2.~ | % m. 74
  dis8 gis, cis~ cis b16 ais b8 | % m. 75
  ais2.~ | % m. 76
  ais4 gis8 cis4. | % m. 77
  b4. ais~ | % m. 78
  ais8 gis gis' fisis4 fis8 | % m. 79
  eis4 e!8 dis4 eis8 | % m. 80
  fis4 fisis8 gis4.~ | % m. 81
  gis4. fisis~ | % m. 82
  fisis8 eis fisis gis4 gisis8 | % m. 83
  ais2.~ | % m. 84
  ais8 fis gis ais4.~ | % m. 85
  ais8 gis4~ gis8 eis fis | % m. 86
  eis4 fis8 gis4.~ | % m. 87
  gis8 fis4~ fis8 dis e | % m. 88
  dis4 cis8 b4. | % m. 89
  ais4 bis8 cis16 dis e8 dis | % m. 90
  gis,4. cis~ | % m. 91
  cis8 b cis dis16 cis b ais b e | % m. 92
  ais,4 r8 e'4 ais8 | % m. 93
  dis,4 gis8 cis,8. e16 dis cis | % m. 94
  b4.~ b8 ais16 gis fisis eis | % m. 95
  dis8 dis' gis,~ gis fisis cis' | % m. 96
  b4 r8 r4. | % m. 97
  R2. | % m. 98
  R2. | % m. 99
  R2. | % m. 100
  R2. | % m. 101
  R2. | % m. 102
  dis8 eis fis eis ais ais, | % m. 103
  dis8 fis eis fis eis dis | % m. 104
  eis8 fis gis fis b dis, | % m. 105
  eis8 gis fis gis fis eis | % m. 106
  fis4. eis4 e!8 | % m. 107
  dis4. cisis4 cis8 | % m. 108
  b8 dis gis~ gis fis16 e fis8~ | % m. 109
  fis8 b, e~ e dis16 cis dis8 | % m. 110
  cis8 dis e dis4 d!8 | % m. 111
  cis4 c!8 b4 cis8 | % m. 112
  d!4 dis8 e4.~ | % m. 113
  e4. dis | % m. 114
  e4 gis,8 ais4 bis8 | % m. 115
  cis4 a!8 fis4 gis8 | % m. 116
  e4 r8 r4. | % m. 117
  r4. r8 r b'~ | % m. 118
  b8 ais bis cis4.~ | % m. 119
  cis8 bis cisis dis4.~ | % m. 120
  dis8 cisis dis eis4.~ | % m. 121
  eis8 dis16 cisis dis8~ dis cis16 b cis8~ | % m. 122
  cis8 dis cis b e dis | % m. 123
  cis8 dis e ais, dis cis | % m. 124
  b4 r8 r4. | % m. 125
  r8 r cis, fis4 gis8~ | % m. 126
  gis8 fis e dis gis b | % m. 127
  eis,4. ais~ | % m. 128
  ais4 gis8~ gis4 fisis8 | % m. 129
  gis4 r8 r dis' cis | % m. 130
  dis8 cis b cis b ais | % m. 131
  b4.~ b8 ais gis | % m. 132
  ais4.~ ais8 gis fisis | % m. 133
  gis4.~ gis8 eis fisis | % m. 134
  gis8 ais b ais dis dis, | % m. 135
  gis8 b ais b ais gis | % m. 136
  ais8 b cis b e gis, | % m. 137
  ais8 cis b cis b ais | % m. 138
  b8 cis dis~ dis cis b | % m. 139
  ais8 eis' ais, ais4.~ | % m. 140
  ais8 e'! dis dis4.~ | % m. 141
  dis8 cis b \grace dis16 (cis8) b ais | % m. 142
  gis2. \bar "|." | % m. 143
    
}

mezzo = \relative c' {
  \global
  
  R2. | % m. 1
  R2. | % m. 2
  R2. | % m. 3
  R2. | % m. 4
  dis8 eis fis eis ais ais, | % m. 5
  dis8 fis eis fis eis dis | % m. 6
  eis8 fis gis fis b dis, | % m. 7
  eis8 gis fis gis fis eis | % m. 8
  fis4 fisis8 gis4 fis8 | % m. 9
  e4 eis8 fis4 e!8 | % m. 10
  dis8 gis gis, cis4.~ | % m. 11
  cis4 b8 ais gis' fisis! | % m. 12
  gis4.~ gis4 fisis8 | % m. 13
  gis4 r8 r r b | % m. 14
  cis8 dis e dis cis b~ | % m. 15
  b8 ais gis fis4.~ | % m. 16
  fis8 fis b~ b gis ais~ | % m. 17
  ais8 dis, gis~ gis eis fisis | % m. 18
  gis8 ais b ais dis dis, | % m. 19
  gis8 b ais b ais gis | % m. 20
  ais8 b cis b e gis, | % m. 21
  ais8 cis b cis b ais | % m. 22
  b8 b, e~ e cis dis | % m. 23
  gis,4. r | % m. 24
  r8 r a'! fis4 gis8 | % m. 25
  e4. dis | % m. 26
  eis8 fisis gis ais4.~ | % m. 27
  ais8 gis \once \override Accidental #'restore-first = ##t fis! eis gis dis | % m. 28
  cisis8 dis eis fis4.~ | % m. 29
  fis8 gis ais gis4.~ | % m. 30
  gis4. fis8 ais dis, | % m. 31
  eis8 fis gis~ gis ais eis | % m. 32
  fis8 eis dis~ dis cisis eis~ | % m. 33
  eis8 dis cisis dis4 b'8 | % m. 34
  \once \override Accidental #'restore-first = ##t cis,!4 b'8 ais gis fis | % m. 35
  gis8 b, ais b gis' b, | % m. 36
  ais4 bis8 cis4.~ | % m. 37
  cis4. bis4.| % m. 38 
  cis4 r8 r dis gis | % m. 39
  cis,8 e dis e dis cis | % m. 40
  dis4 gis8 eis4 fisis8 | % m. 41
  b4. ais | % m. 42
  dis,4 cisis8 dis4 gis8 | % m. 43
  fis4. eis | % m. 44
  dis8 cisis bis cisis fis4~ | % m. 45
  fis8 dis cisis dis gis4~ | % m. 46
  gis4 eis8 cis dis fis | % m. 47
  b4. ais~ | % m. 48
  ais8 cis4~ cis8 ais bis | % m. 49
  cis8 b!4~ b8 gis ais | % m. 50
  b4. ais~ | % m. 51
  ais8 dis, gis e4.~ | % m. 52
  e8 dis cis b4. | % m. 53
  ais8 cis e~ e dis cis | % m. 54
  b8 ais gis~ gis4 fisis8 | % m. 55
  gis4 dis'8 gis4.~ | % m. 56
  gis4 fisis8 gis4.~ | % m. 57
  gis4 gis8 fisis4 r8 | % m. 58
  r8 r ais b4 fisis8 | % m. 59
  gis8 ais b~ b ais gis~ | % m. 60
  gis8 fisis r r4. | % m. 61
  R2. | % m. 62
  R2. | % m. 63
  R2. | % m. 64
  R2. | % m. 65
  r8 r gis fisis4 fis8 | % m. 66
  eis4 e!8 dis4 eis8 | % m. 67
  fis4 fisis8 gis4.~ | % m. 68
  gis4. fisis \prall | % m. 69
  gis4 \once \override Accidental #'restore-first = ##t fis!8 e4 dis8 | % m. 70
  cis8 e a!~ a gis16 fis gis8~ | % m. 71
  gis8 cis, fis~ fis e16 dis e8 | % m. 72
  dis8 gis fis e a! gis | % m. 73
  fis8 gis a! dis, gis fis | % m. 74
  e2.~ | % m. 75
  e8 cis fis~ fis e16 dis e8 | % m. 76
  dis4. e | % m. 77
  dis4. cis | % m. 78
  b4 cis8~ cis ais bis | % m. 79
  cis4.~ cis8 b!4 | % m. 80
  b8 ais4 r8 \clef bass dis, fis~ | % m. 81
  fis8 eis b'~ b ais16 gis ais fisis | % m. 82
  gis4 bis8 eis4 r8 | % m. 83
  r8 \clef treble ais bis cis4 cisis8 | % m. 84
  dis4.~ dis8 dis cis | % m. 85
  bis4 cis8 dis4.~ | % m. 86
  dis8 cis4 cis8 ais b | % m. 87
  ais4 b8 cis4.~ | % m. 88
  cis8 b ais~ ais fis gis~ | % m. 89
  gis8 fis4~ fis4.~ | % m. 90
  fis8 eis fisis gis16 ais b8 ais | % m. 91
  dis,4. gis~ | % m. 92
  gis8 fisis dis'~ dis cis4~ | % m. 93
  cis8 b4~ b8 ais4~ | % m. 94
  ais8 gis16 fis e dis cis4.~ | % m. 95
  cis4 b8 ais16 gis ais8 dis, | % m. 96
  gis4 gis'8 fisis4 fis8 | % m. 97
  eis4 e!8 dis4 eis8 | % m. 98
  fis4 fisis8 gis4.~ | % m. 99
  gis4. fisis | % m. 100
  gis8 b dis, e dis cis | % m. 101
  b16 dis gis8 fis eis ais gis | % m. 102
  fis4 dis8 cisis4 cis8 | % m. 103
  bis4 b!8 ais4 bis8 | % m. 104
  cis4 cisis8 dis4.~ | % m. 105
  dis4. cisis | % m. 106
  dis8 ais' dis~ dis cis16 b cis8~ | % m. 107
  cis8 fis, b~ b ais16 gis ais8 | % m. 108
  b4. ais4 a!8 | % m. 109
  gis4. fisis4 fis8 | % m. 110
  e8 fis gis fis b b, | % m. 111
  e8 gis fis gis fis e | % m. 112
  fis8 gis a! gis cis e, | % m. 113
  fis8 a! gis a gis fis | % m. 114
  gis4 gis8 fisis4 fis8 | % m. 115
  eis4 e!8 dis4.~ | % m. 116
  dis8 cis a'!~ a gis16 fis gis8~ | % m. 117
  gis8 cis, fis~ fis eis16 dis eis8 | % m. 118
  fis4.~ fis8 eis fisis | % m. 119
  gis4.~ gis8 fisis gisis | % m. 120
  ais4.~ ais4 gis8 | % m. 121
  fis8 r r r r ais | % m. 122
  dis,4.~ dis8 cis b | % m. 123
  ais8 b cis dis4 \clef bass dis,8 | % m. 124
  gis8 ais b ais dis dis, | % m. 125
  gis8 b ais b ais gis | % m. 126
  ais8 b cis b e gis, | % m. 127
  ais8 cis b cis b ais | % m. 128
  b4. ais8 dis cis | % m. 129
  dis8 cis b cis b ais | % m. 130
  b8 ais gis ais gis fisis | % m. 131
  gis4.~ gis8 \once \override Accidental #'restore-first = ##t cis! b | % m. 132
  cis8 b ais b4.~ | % m. 133
  b4. ais | % m. 134
  dis,4 \clef treble gis'8 fisis4 fis8 | % m. 135
  eis4 e!8 dis4 eis8 | % m. 136
  fis4 fisis8 gis4.~ | % m. 137
  gis4. fisis | % m. 138
  gis2.~ | % m. 139
  gis4.~ gis8 fisis eis | % m. 140
  fisis2. | % m. 141
  gis4.~ gis4 fisis8 | % m. 142
  gis2. \bar "|." | % m. 143
    
}

bass = \relative c' {
  \global
  
  R2. | % m. 1
  R2. | % m. 2
  R2. | % m. 3
  R2. | % m. 4
  R2. | % m. 5
  R2. | % m. 6
  R2. | % m. 7
  R2. | % m. 8
  R2. | % m. 9
  R2. | % m. 10
  R2. | % m. 11
  R2. | % m. 12
  gis8 ais b ais dis dis, | % m. 13
  gis8 b ais b ais gis | % m. 14
  ais8 b cis b e gis, | % m. 15
  ais8 cis b cis b ais | % m. 16
  b4 gis8 eis4 fisis8 | % m. 17
  gis4 e!8 cis4 dis8 | % m. 18
  e4 eis8 fis4.~ | % m. 19
  fis4. eis | % m. 20
  fis4 fisis8 gis4.~ | % m. 21
  gis4. fisis | % m. 22
  gis4. \once \override Accidental #'restore-first = ##t fis! | % m. 23
  e4. dis | % m. 24
  cis8 gis' cis~ cis ais! bis | % m. 25
  cis4. b!~ | % m. 26
  b8 ais gis fisis ais dis, | % m. 27
  gis4 gis,8 gis'4.~ | % m. 28
  gis8 fis eis dis fis ais, | % m. 29
  b2. \trill | % m. 30
  ais2. | % m. 31
  R2. | % m. 32
  dis8 eis fis eis ais ais, | % m. 33
  dis8 fis eis fis eis dis | % m. 34
  eis8 fis gis fis b dis, | % m. 35
  eis8 gis fis gis fis eis | % m. 36
  fis8 e! dis e a! cis, | % m. 37
  dis4 r8 gis,4 r8 | % m. 38
  cis8 e cis gis'4 fis8 | % m. 39
  e8 fis gis cis,4 fis8 | % m. 40
  b,8 fis' b~ b gis ais~ | % m. 41
  ais8 dis, gis~ gis eis fisis | % m. 42
  gis4 ais8 b4 cisis,8 | % m. 43
  dis4 b8 gis4 ais8 | % m. 44
  dis4. r8 r fis | % m. 45
  b4. r8 r bis | % m. 46
  cis4 b!8 ais4 a!8 | % m. 47
  gis8 b dis, cisis ais cisis | % m. 48
  dis4 r8 gis,4 r8 | % m. 49
  cis4 r8 fis,4 r8 | % m. 50
  b8 dis gis~ gis fisis ais | % m. 51
  b,4 r8 cis4 r8 | % m. 52
  dis4 fisis,8 gis4 b8 | % m. 53
  cis8 e ais, dis4 dis,8 | % m. 54
  gis8 ais b ais dis dis, | % m. 55
  gis8 b ais b ais gis | % m. 56
  ais8 b cis b e gis, | % m. 57
  ais8 cis b cis b ais | % m. 58
  b8 dis fisis, gis b dis, | % m. 59
  e2. \trill | % m. 60
  dis4 dis'8 eis gis fisis | % m. 61
  gis8 dis gis~ gis \once \override Accidental #'restore-first = ##t fis!16 eis fis8 | % m. 62
  eis8 ais gis fis b ais | % m. 63
  gis8 ais b eis, ais gis | % m. 64
  fisis8 gis ais dis, gis fis | % m. 65
  e8 fis e dis e dis | % m. 66
  cis4 cis,8 gis' fisis gis | % m. 67
  dis4 r8 r4. | % m. 68
  R2. | % m. 69
  R2. | % m. 70
  r8 r cis'' bis4 b!8 | % m. 71
  ais4 a!8 gis4 ais8 | % m. 72
  b4 bis8 cis4.~ | % m. 73
  cis4. bis | % m. 74
  cis4 b!8 ais4 gis8 | % m. 75
  fis4 e8 dis4 cis8 | % m. 76
  b8 fis' b~ b ais16 gis ais8~ | % m. 77
  ais8 dis, gis~ gis fisis16 eis fisis8 | % m. 78
  gis4 e!8 dis4. | % m. 79
  ais'8 gis fisis gis4 cisis,8 | % m. 80
  dis4 ais8 b4 bis8 | % m. 81
  cis4 cisis8 dis4 disis8 | % m. 82
  eis4. r8 eis dis | % m. 83
  cis8 cis' bis ais \clef treble ais' gis | % m. 84
  fis8 dis eis fis4 fisis8 | % m. 85
  gis8 \clef bass gis, ais b!4 bis8 | % m. 86
  cis8 cis, dis e!4 eis8 | % m. 87
  fis8 fis, gis a!4 ais8 | % m. 88
  b4 cis8 dis4 eis8 | % m. 89
  fis4 gis8 ais4 bis8 | % m. 90
  cis8 cis, dis eis4 fisis8 | % m. 91
  gis4 ais8 b16 cis dis8 cis | % m. 92
  dis2.~ | % m. 93
  dis2.~ | % m. 94
  dis4 r8 r4. | % m. 95
  R2. | % m. 96
  gis,,8 ais b ais dis dis, | % m. 97
  gis8 b ais b ais gis | % m. 98
  ais8 b cis b e gis, | % m. 99
  ais8 cis b cis b ais | % m. 100
  b8 dis gis~ gis fisis16 eis fisis8 | % m. 101
  gis8 b dis~ dis cisis16 bis cisis8 | % m. 102
  dis4 dis,8 ais'4 fisis8 | % m. 103
  gis4 cisis,8 dis4. | % m. 104
  ais'4.~ ais8 gis fis | % m. 105
  gis4. ais | % m. 106
  dis,4 b8 gis4 ais8 | % m. 107
  b4 dis8 eis4 fisis8 | % m. 108
  gis4 b8 cis4 dis8 | % m. 109
  e4 cis8 ais4 bis8 | % m. 110
  cis8 bis ais b! ais gis | % m. 111
  a!4 dis,8 e4 e,8 | % m. 112
  b'4 bis8 cis4 gis'8 | % m. 113
  a!4 cis8 fis,4 b8 | % m. 114
  e,8 b' e~ e dis16 cis dis8~ | % m. 115
  dis8 gis, cis~ cis bis16 ais bis8 | % m. 116
  cis8 cis, cis' bis4 b!8 | % m. 117
  ais4 a!8 gis4 cis8 | % m. 118
  fis,8 e dis cis b! ais | % m. 119
  gis8 gis' ais b! cis b | % m. 120
  ais8 ais, bis cisis bis cisis | % m. 121
  dis8 gis fis eis ais gis | % m. 122
  fisis8 dis fisis gis4.~ | % m. 123
  gis4. fisis | % m. 124
  gis4 gis,8 fisis4 fis8 | % m. 125
  eis4 e!8 dis4 eis8 | % m. 126
  fis4 fisis8 gis4.~ | % m. 127
  gis4. fisis8 dis' cis | % m. 128
  dis8 cis b cis b ais | % m. 129
  b8 ais gis dis'4.~ | % m. 130
  dis4 gis,8 e'4.~ | % m. 131
  e8 dis cisis dis4.~ | % m. 132
  dis2.~ | % m. 133
  dis8 cis b cis b ais | % m. 134
  b8 cis16 b ais gis dis4.~ | % m. 135
  dis4 cis8 gis'4 cis,8 | % m. 136
  fis4 dis8 gis4 b8 | % m. 137
  e4 cis8 ais4 dis8 | % m. 138
  gis,4 b8 e, e' dis | % m. 139
  cisis4.~ cisis8 ais bis | % m. 140
  \once \override Accidental #'restore-first = ##t cis!4.~ cis8 b! ais | % m. 141
  b8 ais gis dis'4 dis,8 | % m. 142
  gis2. \bar "|." | % m. 143
    
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
