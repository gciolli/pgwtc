\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue III"
  subtitle = "in 3 voices"
  composer = "BWV 848"
  tagline = ""
  copyright = "No rights reserved"
}

global = {
  \key cis \major
  \time 4/4
  \set Timing.beamExceptions = #'()
  \set Timing.baseMoment = #(ly:make-moment 1/4)
  \set Timing.beatStructure = #'(1 1 1 1)

}

soprano = \relative c'' {
  \global
  
  r4 r8 gis ais16 gis fis gis eis'8 cis | % m. 1
  gis8 fis16 eis fis8 dis' eis, cis' dis, bis' | % m. 2
  cis,8 cis'16 bis cis dis eis fis gis fisis eis fisis gis fis eis dis | % m. 3
  eis4~ eis16 dis cis eis dis cis bis dis cis bis cis ais | % m. 4
  \grace ais16 bis8 dis fis!2 eis4~ | % m. 5
  eis4 dis~ dis8 cis bis dis~| % m. 6
  dis8 gis, eis'4~ eis8 eis16 cis ais4~ | % m. 7
  ais8 ais dis4~ dis8 dis16 bis gis4~ | % m. 8
  gis8 gis cis4~ cis16 bis cis ais ais'4~ | % m. 9
  ais16 gis fisis ais gis dis eis fis! eis dis cis dis bis'8 gis | % m. 10
  dis8 cis16 bis cis8 ais' bis, gis' ais, fisis' | % m. 11
  gis,16 ais bis cis bis ais gis bis eis dis cisis eis gis fis eis gis \prall | % m. 12
  b!8 ais16 gis fis eis dis cisis dis fis eis dis cisis eis ais, gis | % m. 13
  fis16 eis dis cisis dis eis fis gis ais gis fis gis ais gis fis eis | % m. 14
  fis4~ fis16 eis dis fis eis dis cis eis dis cis dis bis | % m. 15
  cis16 eis fis eis cis'4~ cis16 bis cis ais ais'4~ | % m. 16
  ais16 dis, eis dis bis4~ bis16 ais bis gis gis'4~ | % m. 17
  gis16 cis,, dis cis ais'4~ ais16 gis ais fisis fisis'4~ | % m. 18
  fisis16 disis eis cis ais2 gis4~ | % m. 19
  gis4 fisis~ fisis8 eis disis fisis | % m. 20
  bis16 ais gis bis ais gis fisis ais gis eis cis' ais bis gis fis'! disis | % m. 21
  disis16 eis aisis bis disis,8. eis16 eis4 r8 eis | % m. 22
  b!16 ais gis ais eis'8 cisis gis fis16 eis fis8 dis' | % m. 23
  a!16 gis fis gis dis'8 bis fis eis16 dis eis gis cis dis | % m. 24
  eis dis cis dis bis'8 gis dis cis16 bis cis8 ais' | % m. 25
  bis,8 gis' ais, fisis' gis,16 dis eis fisis gis ais bis gis | % m. 26
  cis16 bis ais bis cis bis ais gis ais4~ ais16 gis fis ais | % m. 27
  gis16 fis eis gis fis eis dis fis eis8 gis cis4 | % m. 28
  r8 e!16 cis ais4 r8 ais16 cis e4 | % m. 29
  dis8 gis cis, fisis gis, gis'16 fisis gis ais b! gis | % m. 30
  eis16 fis gis eis cis dis eis cis ais bis cis ais fis' gis ais fis | % m. 31
  dis16 eis fis dis bis cis dis bis gis ais bis gis eis' fis gis eis | % m. 32
  cisis16 dis eis cisis ais bis cisis ais fis gis ais fis dis' eis fis dis | % m. 33
  bis16 cis dis bis gis ais bis gis eis fis gis eis cis eis gis ais | % m. 34
  b!16 ais gis ais fis'8 cis ais gis16 fis gis cis, eis gis | % m. 35
  ais16 gis fis gis eis'8 cis gis fis16 eis fis bis, dis fis | % m. 36
  gis16 fis eis fis dis'8 bis fis eis16 dis eis gis ais bis | % m. 37
  cis16 dis eis dis fis eis dis cis gis'2~ \downprall | % m. 38
  gis16 gis fis gis eis gis dis gis cis, gis' bis, ais bis dis eis fis | % m. 39
  gis16 fis eis gis dis gis cis, gis' bis, gis' ais, gis ais cis dis eis | % m. 40
  fis16 eis dis eis cis dis bis dis ais dis gis, fisis gis bis cis dis | % m. 41
  eis16 fis gis ais bis,8 gis ais16 gis fis gis eis'8 cis | % m. 42
  gis8 fis16 eis fis8 dis' eis, cis' dis, bis' | % m. 43
  cis,8 cis'16 bis cis dis eis fis gis fis eis fis gis fis eis dis | % m. 44
  eis4~ eis16 dis cis eis dis cis bis dis cis bis cis ais | % m. 45
  bis8 dis fis2 eis4~ | % m. 46
  eis4 dis~ dis8 cis bis dis~ | % m. 47
  dis8 gis, cis b! ais2~ | % m. 48
  ais8 ais16 fis dis4~ dis8 dis gis4~ | % m. 49
  gis8 gis16 eis cisis4~ cisis8 ais fis'4~ | % m. 50
  fis16 eis fis dis dis'4~ dis16 cis bis dis cis gis ais b! | % m. 51
  ais16 gis fis gis eis'8 cis gis fis16 eis fis8 dis' | % m. 52
  eis,8 cis' dis, bis' cis,16 eis fis gis ais bis cis dis | % m. 53
  e!16 dis cis dis e fisis gis ais cis, bis ais gis dis'8 fis,! | % m. 54
  eis8 <cis gis' cis> dis bis' cis2 \fermata \bar "|." | % m. 55
  
}

alto = \relative c' {
  \global
  
 R1 | % m. 1
 R1 | % m. 2
 r4 r8 cis eis16 dis cis dis bis'8 gis | % m. 3
 dis8 cis16 bis cis8 ais' bis, gis' ais, fisis' | % m. 4
 gis,8 gis'16 fisis gis ais bis gis cis bis ais bis cis bis ais gis | % m. 5
 ais4~ ais16 gis fis ais gis fis eis gis fis eis fis dis | % m. 6
 eis4 r8 gis cis4. cis16 ais | % m. 7
 dis,4 r8 fisis bis4. bis16 gis | % m. 8
 cis,4 r8 eis ais4~ ais16 cis dis ais | % m. 9
 bis4 r8 gis~ gis16 fisis eis fisis gis fis!eis dis | % m. 10
 eis4~ eis16 dis cis eis dis cis bis dis cis bis cis ais | % m. 11
 bis8 dis gis4 r8 \clef bass b,!16 gis eis4 | % m. 12
 r8 eis16 gis b!4 ais8 b! ais cisis, | % m. 13
 dis4  r r2 | % m. 14
 r1 | % m. 15
 \clef treble r4 r8 eis'8 ais4~ ais16 cis dis ais | % m. 16
 bis8 r r dis, gis4~ gis16 bis cis gis | % m. 17
 ais8 r r cis, fisis4~ fisis16 ais bis fisis | % m. 18
 gis4 r8 \clef bass bis, cis16 bis ais bis gis'8 eis | % m. 19
 bis8 ais16 gis ais8 fisis' gis, eis' fisis, disis' | % m. 20
 eis,4. disis8 eis disis eis ais | % m. 21
 gis8 fisis16 eis fisis8 ais  gis16 ais bis gis eis fis! gis eis | % m. 22
 cisis16 dis eis cisis ais cisis eis ais dis, fis ais cisis dis eis fis dis | % m. 23
 bis16 cis dis bis gis4~ gis16 gis ais bis cis8 r | % m. 24
 \clef treble r4 bis'2 ais4~ | % m. 25
 ais8 gis fisis ais dis,16 r r8 r \clef bass gis, | % m. 26
 ais16 gis fis gis eis'8 cis gis fis16 eis fis8 dis' | % m. 27
 eis,8 cis' dis, bis' cis4 \clef treble r8 gis''16 eis | % m. 28
 cis4 r8 cis16 ais fisis4 r8 cis' | % m. 29
 b!4 ais gis8 r r4 | % m. 30
 \clef bass r8 b,!16 gis eis8 r r ais16 fis dis8 r | % m. 31
 r8 ais'16 fis dis8 r r gis16 eis cisis8 r | % m. 32
 r8 gis'16 eis cisis8 r r fis'16 dis bis8 r | % m. 33
 r8 fis'16 dis bis8 r r2 | % m. 34
 r1 | % m. 35
 r1 | % m. 36
 r1 | % m. 37
 r1 | % m. 38
 r1 | % m. 39
 r1 | % m. 40
 r1 | % m. 41
 r1 | % m. 42
 r1 | % m. 43
 \clef treble r4 r8 cis16 dis eis dis cis dis bis'8 gis | % m. 44
 dis8 cis16 bis cis8 ais' bis, gis' ais, fisis' | % m. 45
 gis,8 gis'16 fis gis ais bis gis cis bis ais bis cis bis ais gis | % m. 46
 ais4~ ais16 gis fis ais gis fis eis gis fis eis fis dis | % m. 47
 eis2~ eis8 cis fis eis | % m. 48
 fis4. fis16 dis gis,4 r8 bis | % m. 49
 eis4. eis16 cisis fis,4 r8 ais | % m. 50
 dis4~ dis16 fis gis dis eis4 r8 \clef bass cis~ | % m. 51
 cis16 bis ais bis cis bis ais gis ais4~ ais16 gis fis ais | % m. 52
 gis16 fis eis gis fis eis dis fis eis gis ais bis cis bis ais gis | % m. 53
 fisis16 gis ais fisis dis4~ dis16 dis eis fis! gis ais bis gis | % m. 54
 \clef treble cis4 <fis ais>8 <fis gis> <cis eis gis>2 \bar "|." | % m. 55
 
}

bass = \relative c' {
  \global
  
 R1 | % m. 1
 R1 | % m. 2
 R1 | % m. 3
 R1 | % m. 4
 r4 r8 gis ais16 gis fis gis eis'8 cis | % m. 5
 gis8 fis16 eis fis8 dis' eis, cis' dis, bis' | % m. 6
 cis,8 cis'16 bis cis dis eis cis ais bis cis ais fisis gis ais fisis | % m. 7
 bis,8 bis'16 ais bis cis dis bis gis ais bis gis eis fisis gis eis | % m. 8
 ais,8 ais'16 gis ais bis cis ais fisis gis ais fisis dis eis fisis dis | % m. 9
 gis,8 gis'16 ais bis8 gis cis4 bis | % m. 10
 ais8 gis fisis4 gis dis | % m. 11
 gis4 r8 dis'16 bis gis4 r8 gis16 eis | % m. 12
 cisis4 r8 eis8 fis gis ais ais, | % m. 13
 dis,4 r8 dis' fis16 eis dis eis cis'8 ais | % m. 14
 eis8 dis16 cis dis8 bis' cis, ais' bis, gisis' | % m. 15
 ais,8 ais'16 gisis ais bis cis ais fisis gis! ais fisis dis eis fisis dis | % m. 16
 gis,8 gis'16 fisis gis ais bis gis eis fisis gis eis cis dis eis cis | % m. 17
 fisis,8 fisis'16 eis fisis gis ais fisis disis eis fisis disis bis cisis disis bis | % m. 18
 eis,16 fisis gis ais bis cisis disis bis eis dis! cis! dis eis dis cis bis | % m. 19
 cis4~ cis16 bis ais cis bis ais gis bis ais gis ais fisis | % m. 20
 gis8 cis fisis, bis eis, ais gis cis | % m. 21
 bis2 eis4 r | % m. 22
 r1 | % m. 23
 r4 r16 gis, ais bis cis4~ cis16 dis eis fisis | % m. 24
 gis16 fisis eis fisis gis fisis eis dis eis4~ eis16 dis cis eis | % m. 25
 dis16 cis bis dis cis bis ais cis bis8 dis fis!4~ | % m. 26
 fis4 eis2 dis4~ | % m. 27
 dis8 cis dis gis, cis16 dis eis fis eis dis cis eis | % m. 28
 ais gis fisis ais cis b! ais cis e!8 dis16 cis b ais gis fis | % m. 29
 gis16 b! ais gis fisis eis dis cis b! cis dis b gis8 gis'16 eis | % m. 30
 cis8 r r cis16 ais fis8 r r fis'16 dis | % m. 31
 bis8 r r bis16 gis eis8 r r eis'16 cisis | % m. 32
 ais8 r r ais16 fis dis8 r r dis'16 bis | % m. 33 
 gis8 r r gis16 eis cis8 r r4 | % m. 34
 r8 b''16 cis, ais' cis, gis' cis, fis cis eis dis eis8 gis, | % m. 35
 r8 ais'16 cis, gis' cis, fis cis eis cis dis cis dis8 gis, | % m. 36
 r8 gis'16 gis, fis' gis, eis' gis, dis' gis, cis bis cis gis eis' dis | % m. 37
 eis16 gis, gis' fis gis gis, ais' gis, bis' cis dis bis gis bis dis eis | % m. 38
 \clef treble fis16 eis dis eis cis'8 gis eis dis16 cis dis gis, bis dis | % m. 39
 eis16 dis cis dis bis'8 gis dis cis16 bis cis gis bis cis | % m. 40
 dis16 cis bis cis ais'8 fisis cis bis16 ais bis gis ais bis | % m. 41
 cis dis eis dis fis eis dis cis fis8 ais,16 bis cis bis ais gis | % m. 42
 \clef bass ais4~ ais16 gis fis ais gis fis eis gis fis eis fis dis | % m. 43
 eis8. gis16 ais bis cis8~ cis4 bis~ | % m. 44
 bis4 ais~ ais8 gis ais dis, | % m. 45
 gis4 r8 gis ais16 gis fis gis eis'8 cis | % m. 46
 gis8 fis16 eis fis8 dis' eis, cis' dis, bis' | % m. 47
 cis,16 gis cis dis eis fis gis eis fis fis, fis' eis fis gis ais fis | % m. 48
 dis16 eis fis dis bis cis dis bis eis,8 eis'16 dis eis fis gis eis | % m. 49
 cisis16 dis eis cisis ais bis cisis ais dis,8 dis'16 cisis dis eis fis dis | % m. 50
 bis16 cis dis bis gis ais bis ais cis,8 cis'16 dis eis8 cis | % m. 51
 fis4 eis dis8 cis bis4 | % m. 52
 cis4 gis ais8 r r4 | % m. 53
 r4 r16 cis bis ais gis4 r | % m. 54
 r8 eis fis gis cis,2 \fermata \bar "|." | % m. 55

}

\paper {
  max-systems-per-page = 5
}

\score {
  \new StaffGroup
  <<
    \new Staff = "soprano" 
      \soprano
    
    \new Staff = "alto" 
      \alto
    
    \new Staff = "bass" 
      { \clef bass \bass }
 
  >>
  
\layout { 
  indent = 0.0
}
 
}
