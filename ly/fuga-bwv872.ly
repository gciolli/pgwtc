\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue III"
  subtitle = "in 3 voices"
  opus = "BWV 872"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key cis \major
  \time 4/4
}

soprano = \relative c'' {
  \global
  
  r2 r8 gis bis gis | % m. 1
  cis8 r b! r ais16 gis fis8~ fis16 gis ais fis | % m. 2
  gis8 r cis r cis r bis r | % m. 3
  r8 cis eis cis gis' r fis r | % m. 4
  eis16 dis cis8~ cis16 dis eis cis r dis fis dis gis8 r | % m. 5
  fis8 r eis8. eis16 fisis8 gis~ gis16 gis fisis8 | % m. 6
  gis8 r r4 r2 | % m. 7
  r8 eis, gis eis ais r gis r | % m. 8
  fis16 gis ais bis cisis dis eis32 dis cisis16 dis8 r r dis | % m. 9
  bis8 dis gis, cis ais r bis r | % m. 10
  cis8 gis' eis gis cis, r r16 b! ais gis | % m. 11
  ais4~ ais16 ais dis8 gis,4~ gis16 gis cis8 | % m. 12
  fis,4~ fis16 eis fis8 eis4~ eis16 dis eis8~ | % m. 13
  eis8. eis16 dis4 eis8 r r eis' \break | % m. 14
  cis eis ais, r r ais' fisis ais | % m. 15
  dis,8 r eis r \once \override Accidental #'restore-first = ##t fis!16 gis ais8~ ais16 gis fis gis | % m. 16
  eis16 fis gis8~ gis16 fis eis fis dis gis fis eis dis cis bis32 cis dis16 | % m. 17
  cis16 ais bis cis dis eis fis32 eis dis16 cis bis ais gis cis8 r | % m. 18
  ais8 r fis' r r16 fis eis dis eis gis cis, eis | % m. 19
  ais,16 eis' dis cis dis fis bis, dis gis, dis' cis bis cis eis gis, b! | % m. 20
  ais8. fis16 gis8. eis16 eis8. dis16 eis4~ | % m. 21
  eis4~ eis16 dis fis dis bis!8 bis16 dis gis8. fis16 \break | % m. 22
  eis8 eis16 gis cis8. b!16 ais8 ais16 cis fis4~ | % m. 23
  fis16 ais, bis! cis dis eis fis32 eis dis16 eis fis eis dis cis bis ais32 bis cis16 | % m. 24
  bis8 gis' eis gis cis,16 eis fis gis ais bis cis ais | % m. 25
  fis16 gis fis eis dis cis bis dis gis,2~ | % m. 26
  gis16 cis, dis eis fis gis ais fis dis eis fis gis ais bis cis ais | % m. 27
  bis8 dis bis dis  gis, cis e! cis | % m. 28
  fis16 a! gis fis e! dis cis32 dis e16 fisis, gis ais! bis cis dis32 e dis16 cis | % m. 29
  bis8 dis gis2 fis4~ | % m. 30
  fis4 eis~ eis8 eis32 dis cis b! ais gis fis eis fis8~ | % m. 31
  fis4~ fis8 eis~ \bar "" \break eis32 eis dis cis dis8~ dis4~ | % m. 32
  dis8 cis~ cis16 cis bis8 cis4~ cis16 gis b! gis | % m. 33
  ais4~ ais16 ais cis ais dis4~ dis16 eis fis8~ | % m. 34
  fis16 eis dis fis~ fis ais gis fis eis2 \fermata \bar "|." | % m. 35
   
}

mezzo = \relative c'' {
  \global
  \mergeDifferentlyDottedOn
  
  R1 | % m. 1
  r8 gis eis gis cis, r dis r | % m. 2
  eis16 fis gis8~ gis16 fis eis gis dis eis fis8~ fis16 eis dis fis | % m. 3
  eis8 r r4 r8 gis bis gis | % m. 4
  cis8 r b! r ais r r16 ais cisis ais | % m. 5
  dis8 r r16 dis \once \override Accidental #'restore-first = ##t cis!8~ cis16 bis8 bis16 ais8. cis16 | % m. 6
  bis8 r r4 r8 gis, bis gis | % m. 7
  cis8 r b! r ais16 bis cisis dis eis fis gis32 fis eis16 | % m. 8
  fis8 r eis r r ais fis ais | % m. 9
  dis,8 fis eis gis~ gis16 gis fis eis dis cis bis32 cis dis16 | % m. 10
  gis,8 r r cis eis cis gis' r | % m. 11
  r16 gis fis eis fis8. fis16~ fis fis eis dis eis8. eis16~ | % m. 12
  eis16 eis dis cis dis8. dis16~ dis dis cis bis cis8. bis16 | % m. 13
  ais16 gisis ais8~ ais16 cis bis ais gisis8 r r4 | % m. 14
  r8 ais cisis ais dis r cis! r | % m. 15
  bis16 ais gis8~ gis16 ais b! gis ais bis cis ais bis cis dis bis | % m. 16
  cis2~ cis8 cis bis eis | % m. 17
  ais,4. dis8 gis,4 r16 gis' eis gis | % m. 18
  cis,8 r r16 \clef bass bis gis bis cis8 gis cis4~ | % m. 19
  cis8 ais fis4~ fis8 gis eis eis'~ | % m. 20
  eis16 cis dis8~ dis16 bis cis8~ cis16 ais bis8 r16 cis gis b! | % m. 21
  ais2 r16 gis8.~ gis16 cis bis8 | % m. 22
  r16 \clef treble cis8.~ cis16 fis eis8 r16 fis8.~ fis16 b! ais dis | % m. 23
  gis,8 r r4 r16 bis, cis dis eis fisis gis fisis | % m. 24
  gis8 r cis,4 eis cis | % m. 25
  fis8 r r fis~ fis16 ais gis fis eis dis cis eis | % m. 26
  ais,4 r8 dis ais dis fis, fis'~ | % m. 27
  fis16 gis fis e! dis cis bis32 cis dis16 e fis e dis cis bis ais32 bis cis16 | % m. 28
  <<
    { s2. r8 fisis! | gis8 r }
    \\
    { bis,8 dis cis e! ais, cis e!4 | dis8 r }
  >> % mm. 29-30/1
  r8 eis'! cisis eis ais, dis | % m. 30/2-
  bis8 dis gis, cis ais16 cis fis,8 r r32 eis dis cis | % m. 31
  <<
    { s16 dis8.~ dis8 cis~ cis~ cis32 cis bis ais bis4~ | 
      bis16 \clef bass gis8 fisis16 gis8. fis!16~ fis fis eis! dis eis4~ | 
      eis16 gis fis eis fis2 r8 r16 gis | 
      ais4 bis <gis cis>2 \fermata \bar "|."}
    \\
    { bis16 dis gis,8~ gis4 s2 | s1 | s1 | s1 }
  >> | % mm. 32-35
    
}

bass = \relative c {
  \global
  
  r8 cis eis cis gis' r fis r | % m. 1
  eis16 dis cis8~ cis16 dis eis cis fis8 r fis, r | % m. 2
  r8 cis' ais cis fis, ais dis, gis | % m. 3
  cis,8 r r cis'' bis dis gis, r | % m. 4
  r8 cis, eis cis fis r eis r | % m. 5
  r16 dis bis dis gisis,8 ais dis, eis cis dis | % m. 6
  r8 gis bis gis cis r bis r | % m. 7
  ais16 bis cis dis eis fis gis32 fis eis16 fis8 r cisis r | % m. 8
  dis8 r r ais' fis ais dis, fisis | % m. 9
  gis16 gis, ais bis cis dis eis32 dis cis16 fis8 dis r gis | % m. 10
  eis8 gis cis, r r cis eis cis | % m. 11
  fis8 ais dis, fis bis, gis' cis, eis | % m. 12
  ais,8 fis' bis, dis gisis, eis ais gis | % m. 13
  fis8 eis fis4 \prall eis8 eis' gisis eis | % m. 14
  ais8 r gis r fisis16 eis dis8~ dis16 eis fisis dis | % m. 15
  gis8 r r4 r2 | % m. 16
  r8 cis, eis cis gis'2~ | % m. 17
  gis8 gis fis4~ fis8 fis eis r | % m. 18
  r16 ais fis ais dis,8 r cis r r16 cis eis cis | % m. 19
  fis8 r r dis bis r r cis | % m. 20
  fis,8 fis' eis ais, dis gis, cis cis, | % m. 21
  fis16 cis' ais cis fis4~ fis8 fis eis gis | % m. 22
  cis,8 b'! ais cis fis, eis dis fis | % m. 23
  bis,8 gis' bis gis cis r ais r | % m. 24
  r16 gis ais bis cis dis eis cis ais8 ais cis ais | % m. 25
  dis8 dis, bis dis eis eis, cis eis | % m. 26
  fis4 dis' fis dis | % m. 27
  gis1~ | % m. 28
  gis1~ | % m. 29
  gis16 ais gis fis eis! dis cisis32 dis eis16 ais, ais' bis cisis dis eis fis32 eis dis16 | % m. 30
  gis16 fis eis dis cis b! ais32 b cis16 fis,8 dis, fis dis | % m. 31
  <<
    { r8 r32 fis'32 eis dis eis dis cis bis cis8 r fisis8 gis4 | 
      eis8 e! dis4 cis2~ | 
      cis2~ cis16 cis bis ais bis4 |
      cis4 dis cis2 \fermata }
    \new Staff \with {
      \remove "Time_signature_engraver"
      alignBelowContext = #"bass"
      \clef bass
  } { \key cis \major 
      gis4~ gis8~ gis32 ais gis fisis gis4~ gis32 ais bis cis dis eis \once \override Accidental #'restore-first = ##t fis! dis | 
      r4 gis, cis,2~ | 
      cis1~ | 
      cis1 \fermata \bar "|." }
    >>| % mm. 32-35
  
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
