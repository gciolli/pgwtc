\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue VIII"
  subtitle = "in 4 voices"
  opus = "BWV 877"
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
  R1 | % m. 3
  R1 | % m. 4
  R1 | % m. 5
  R1 | % m. 6
  R1 | % m. 7
  R1 | % m. 8
  r8 ais ais ais gisis ais bis4~ | % m. 9
  bis8 ais dis4. cis16 bis! eis8 dis | % m. 10
  cis4. dis16 e! ais,8 dis4 cis8~ | % m. 11
  cis8 b ais4. gis8 gis'4~ | % m. 12
  gis8 fis4 eis16 dis cisis8 eis fis cisis | % m. 13
  dis16 \once \override Accidental #'restore-first = ##t cis! b ais gis8 ais b ais gis4~ | % m. 14
  gis8 fis eis e! dis4. cis16 dis | % m. 15
  e!4. dis16 eis fis4. eis16 fisis | % m. 16
  gis8 r r4 r2 | % m. 17
  R1 | % m. 18
  R1 | % m. 19
  R1 | % m. 20
  r8 dis' dis dis cisis dis eis4~ | % m. 21
  eis8 dis gis4. fis16 eis ais8 gis | % m. 22
  fis8 ais dis,2~ dis8 cisis | % m. 23
  dis8 r r4 r2 | % m. 24
  R1 | % m. 25
  R1 | % m. 26
  r2 r8 cis cis cis | % m. 27
  bis8 cis dis4. cis8 fis4~ | % m. 28
  fis8 e!16 dis gis8 fis eis cis fis4~ | % m. 29
  fis4 eis fis4. e!8 | % m. 30
  dis4. cis16 b ais4 ais'~ | % m. 31
  ais8 gis16 fisis gis2~ gis8 fisis | % m. 32
   gis4. fis8 eis2~ | % m. 33
   eis8 dis16 cis b8 ais gisis4 ais~ | % m. 34
   ais4 gisis ais4. gis!8 | % m. 35
   fisis8 cis' b ais~ ais gis cis4~ | % m. 36
   cis8 b16 ais b8 e! cisis ais gis'4~ | % m. 37
   gis8 fis16 eis fis8 b gisis eis ais16 gis fisis eis | % m. 38
   dis4~ dis16 fis eis dis cisis eis dis cis! bis dis cis b! | % m. 39
   ais4. gis8~ gis16 eis fis ais dis ais fis'8 | % m. 40
   eis8 r r b' ais r r gis | % m. 41
   cisis,8 r r eis ais, dis16 cisis dis8 gis, | % m. 42
   fis4 eis dis8 dis' dis dis | % m. 43
   cisis8 dis eis4. dis8 gis4~ | % m. 44
   gis8 fis16 eis ais8 gis fis cisis! dis4~ | % m. 45
   dis4 cisis dis2 \bar "|." | % m. 46
     
}

alto = \relative c' {
  \global
  
  r8 dis dis dis cisis dis eis4~ | % m. 1
  eis8 dis gis4. fis16 eis ais8 gis | % m. 2
  fis8 eis dis cis16 dis eis4. dis16 eis | % m. 3
  fis4. eis16 fisis gis4. fisis16 gisis | % m. 4
  ais8 bis cis4. fisis,8 gis ais | % m. 5
  dis,8 gis4 fisis8 b4. ais16 gis | % m. 6
  fis8 gis ais b eis,16 fis eis dis cisis dis eis fis | % m. 7
  gis16 ais gis fis eis4. fis16 gis fis8 eis | % m. 8
  dis8 cisis dis16 eis fis8 eis4. fis16 eis | % m. 9
  dis8 cis r cis' bis ais gis16 ais bis8~ | % m. 10
  bis8 ais16 gisis ais8 gis! fisis8 ais dis,16 eis fisis gis | % m. 11
  ais8 gis r4 r2 | % m. 12
  R1 | % m. 13
  R1 | % m. 14
  R1 | % m. 15
  R1 | % m. 16
  r8 gis gis gis eis! fis gis4~ | % m. 17
  gis8 fis b4. ais16 gis cis8 b | % m. 18
  ais8 gis16 fis eis fisis gis8~ gis fisis16 gis ais4~ | % m. 19
  ais8 gis16 fisis gis8 ais16 b cis2~ | % m. 20
  cis8 b16 ais b gis fis! eis fis8. fis16 eis fis gis8~ | % m. 21
  gis8 fis r fis' eis r r eis | % m. 22
  dis8 cis4 b16 ais gis dis gis8 gis gis | % m. 23
  fisis8 gis ais4. gis8  cis4~ | % m. 24
  cis8 b16 ais dis8 cis b ais gis4~ | % m. 25
  gis8 fis eis4. ais16 gis fis8 e! | % m. 26
  dis8 dis'~ dis16 b ais gis ais4. a!8 | % m. 27
  gis4 a!8 gis16 fis e!8 fis16 gis a4 | % m. 28
  gis4. a!16 b cis4. dis8 | % m. 29
  cis8 b b b ais! b cis4~ | % m. 30
  cis8 b e!4. dis16 cis fis8 e | % m. 31
  dis4 r8 gis, ais b cis4~ | % m. 32
  cis8 b16 ais b4. ais16 gis ais4~ | % m. 33
  ais8. gis16 fis8 eis dis16 eis fis8 eis fis16 disis | % m. 34
  eis4. dis8 cis dis eis e! | % m. 35
  dis4. cis8 b4 ais8 ais' | % m. 36
  dis,8 gis16 fisis gis4~ gis8 fis! eis eis' | % m. 37
  ais,8 dis16 cisis dis4. cis!16 bis cis4~ | % m. 38
  cis16 cis b ais b4 ais gis~ | % m. 39
  gis16 gis fis eis dis fis eis dis cisis8 dis r gisis | % m. 40
  ais8 r r cisis fis r r b, | % m. 41
  ais8 r r eis fis fis16 eis dis8 eis~ | % m. 42
  eis8 dis4 cisis8 dis fis16 eis fis ais gis fis | % m. 43
  eis16 gis fis ais b ais b8~ b ais r16 dis cisis! bis | % m. 44
  <<
    { s2. r8 b! | ais2 ais }
    \\
    { cisis8 dis16 eis fis8 cisis dis gis, fis16 eis fis8~ | fis8 gis16 fis eis fis gis8 fisis2 }
  >> \bar "|." | % m. 45-46
  
}

tenor = \relative c' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  r8 ais ais ais gisis ais bis4~ | % m. 3
  bis8 ais dis4. cis16 bis eis8 dis | % m. 4
  cis8 bis ais \clef treble ais' dis,4. cis8 | % m. 5
  b!4 ais gis8 gis' cisis,4 | % m. 6
  dis8 \clef bass eis, fis gis ais4. gis16 ais | % m. 7
  b4. ais16 bis cis4. bis16 cisis | % m. 8
  dis8 gis, fis16 gis ais bis cis8 bis16 ais gisis4~ | % m. 9
  gisis8 ais16 gisis ais8 r r dis bis eis | % m. 10
  eis4 r r8 cis b! cis | % m. 11
  fisis,8 \clef treble e'!4 dis16 cisis dis4. b'8 | % m. 12
  ais8 dis, gis4. cisis,8 dis eis | % m. 13
  ais,8 dis4 cisis8 fis4. eis16 dis | % m. 14
  cisis8 dis4 cis!8~ cis16 \clef bass b ais gis fisis gis ais b | % m. 15
  cis16 dis cis b ais gis fis eis dis4 r8 ais' | % m. 16
  dis16 e! dis cis b cis dis8~ dis16 cis b ais gis ais b gis | % m. 17
  ais8 dis gis, gis' cis,2~ | % m. 18
  cis8 cis cis cis ais b cis4~ | % m. 19
  cis8 b e!4. dis16 cis \once \override Accidental #'restore-first = ##t fis!8 e | % m. 20
  dis8 fisis, gis gisis ais4. gis16 ais | % m. 21
  b4. ais16 bis cis!4. bis16 cisis | % m. 22
  dis4~ dis16 e! dis cis b4. ais16 gis | % m. 23
  ais8 \clef treble e'! e e dis eis fisis gis | % m. 24
  ais8 gis fisis4 gis8 fis!4 eis16 dis | % m. 25
  cisis8 dis4 cisis16 bis cisis4 cis!~ | % m. 26
  cis8 b16 ais b8 e! cis ais dis e! | % m. 27
  dis8 cis4 bis8 cis4. dis8 | % m. 28
  bis8 cis d!4 cis8 dis16 eis fis gis a!8~ | % m. 29
  a8 gis16 fis gis4 fis8 r r4 | % m. 30
  R1 | % m. 31
  r8 dis dis dis cis dis e!4~ | % m. 32
  e8 dis gis4. fis16 eis ais8 gis | % m. 33
  fis8. eis16 dis8 cis bis4 cis | % m. 34
  bis8 cis16 ais bis4 ais8 bis cis4~ | % m. 35
  cis8 \clef bass fisis, gis ais dis, gis4 fisis8 | % m. 36
  b4. ais16 gis ais8 \clef treble dis4 cisis8 | % m. 37
  fis4. eis16 dis eis8 ais4 gis8 | % m. 38
  fisis8 dis gis4. fis!8~ fis16 fis eis dis | % m. 39
  cisis16 eis dis cis! bis8 b! ais4 r8 bis | % m. 40
  eis8 r r eis ais, r r dis | % m. 41
  eis8 r r \clef bass cisis dis ais16 gis ais8 b | % m. 42
  ais4~ ais8. gis16 fis8 ais ais ais | % m. 43
  b8 ais gis4. ais8 eis4~ | % m. 44
  eis8 fis16 gis dis8 eis fis b ais dis,16 eis | % m. 45
  fis8 eis16 dis ais'8 eis' dis2 \bar "|." | % m. 46
  
}

bass = \relative c {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  R1 | % m. 5
  R1 | % m. 6
  r8 dis dis dis cisis dis eis4~ | % m. 7
  eis8 dis gis4. fis16 eis ais8 gis | % m. 8
  fis8 eis dis cis16 dis eis4. dis16 eis | % m. 9
  fis4. eis16 fisis gis4. fisis16 gisis | % m. 10
  ais8 bis cis4. fisis,8 gis ais | % m. 11
  dis,8 gis4 fisis!8 b4. ais16 gis | % m. 12
  dis'8 cis b4 ais4. gis8 | % m. 13
  fis4 eis dis8 cis b b' | % m. 14
  ais8 ais, ais ais fisis gis ais4~ | % m. 15
  ais8 gis cis4. b16 ais dis8 cis | % m. 16
  b8. ais16 gis ais b gis cis8 dis eis! cis | % m. 17
  fis4. eis16 dis eis gis cis, dis eis fis gis eis | % m. 18
  fis8 eis16 dis cis dis e! cis dis4~ dis16 dis, eis! fisis | % m. 19
  gis4. fisis16 gis ais4. gis16 ais | % m. 20
  b2 ais8 bis cisis bis16 cisis | % m. 21
  dis8 cisis16 dis eis4. dis16 eis fis4~ | % m. 22
  fis8 eis16 fisis gis4. fis8 e!4 | % m. 23
  dis4 cis b4. ais16 gis | % m. 24
  fisis8 dis' dis dis cisis dis eis4~ | % m. 25
  eis8 dis gis4. fis16 eis ais8 gis | % m. 26
  fisis8 dis gis4. fisis16 eis! fisis8 fis!~ | % m. 27
  fis8 e! fis gis a!4. gis16 fis | % m. 28
  gis8 ais! b!4. ais16 gis ais8 bis | % m. 29
  cis4 cis, fis16 eis fis gis ais fis gis ais | % m. 30
  b16 cis dis b cis b ais gis fis gis fis e! dis cis b ais | % m. 31
  b8 b'16 ais b8 cis16 b ais4 a! | % m. 32
  gis4~ gis16 fis eis dis cisis4. bis16 ais | % m. 33
  dis8 dis, dis' eis fis eis16 dis cis bis ais cis | % m. 34
  eis,2 ais8 r r4 | % m. 35
  R1 | % m. 36
  R1 | % m. 37
  R1 | % m. 38
  R1 | % m. 39
  r2 r8 dis dis dis | % m. 40
  cisis8 dis eis4. dis8 gis4~ | % m. 41
  gis8 fis16 eis ais8 gis fis dis16 eis fis8 gis | % m. 42
  ais4 ais, dis2~ | % m. 43
  dis8 dis16 cisis dis fis eis dis cisis8 fis r16 b,16 ais gis | % m. 44
  ais8. gis16 fis8 ais dis eis fis gisis, | % m. 45
  ais2 dis, \bar "|." | % m. 46
    
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
