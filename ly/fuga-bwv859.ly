\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

#(ly:expect-warning "Previous mark event here")
#(ly:expect-warning "Two simultaneous mark events, junking this one")

\header {
  title = "Fugue XIV"
  subtitle = "in 4 voices"
  opus = "BWV 859"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key fis \minor
  \numericTimeSignature
  \time 6/4
}

soprano = \relative c' {
  \global
  
  R1. | % m. 1
  R1. | % m. 2
  R1. | % m. 3
  R1. | % m. 4
  R1. | % m. 5
  R1. | % m. 6
  R1. | % m. 7
  R1. | % m. 8
  R1. | % m. 9
  R1. | % m. 10
  R1. | % m. 11
  R1. | % m. 12
  R1. | % m. 13
  R1. | % m. 14
  r4 fis gis a2.~ | % m. 15
  a4 gis8 ais b4~ b ais8 gis ais bis | % m. 16
  cis4. b!8 a! cis b a gis2 | % m. 17
  fis8 a b cis d4~ d8 b cis dis e!4~ | % m. 18
  e8 d! d cis cis bis bis4 cis2~ | % m. 19
  cis4 bis2 cis8 dis e fis g!4~ | % m. 20
  g4 cis, fis8 e d cis d2 | % m. 21
  cis4 dis eis fis2.~ | % m. 22
  fis2 e4~ e d2~ | % m. 23
  d4 cis2~ cis4 b2~ | % m. 24
  b8 gis a cis fis dis e!2.~ | % m. 25
  e4 dis8 eis fis4~ fis eis8 dis eis fisis | % m. 26
  gis4. \once\override Accidental #'restore-first = ##t fis!8 e! gis fis e dis!2 | % m. 27
  cis8 d fis gis a4~ a8 fis gis ais b4~ | % m. 28
  b8 a! a gis gis fis fis a16 gis fis8 e! e dis | % m. 29
  dis8 e e d! d cis cis2~ cis8 dis | % m. 30
  eis8 fis gis4 cis, fis2 eis4 | % m. 31
  fis8 e! d cis cis b b fis' b, a a gis | % m. 32
  gis4. b8 a gis fis eis fis a b cis | % m. 33
  d2.~ d8 gis, cis b b a | % m. 34
  a2.~ a8 cis b a a gis | % m. 35
  gis2.~ gis8 b a g! g fis | % m. 36
  fis8 eis fis4 gis! a2.~ | % m. 37
  a4 gis8 ais b4~ b ais8 gis ais bis | % m. 38
  cis4. b!8 a! cis b a gis2 | % m. 39
  fis1. \bar "|." \override Staff.RehearsalMark #'break-visibility = #begin-of-line-invisible \mark\markup{\musicglyph #"scripts.ufermata"} | % m. 40
  
}

alto = \relative c' {
  \global
  
  R1. | % m. 1
  R1. | % m. 2
  R1. | % m. 3
  r4 cis dis e2.~ | % m. 4
  e4 dis8 eis fis4~ fis eis8 dis eis fisis | % m. 5
  gis4. \once\override Accidental #'restore-first = ##t fis!8 e! gis fis e dis2 -\parenthesize \prall | % m. 6
  cis8 e fis gis a4~ a8 fis gis ais b4~ | % m. 7
  b8 b b a! a gis gis fis fis e e dis | % m. 8
  dis8 e e d! d cis cis2~ cis8 dis | % m. 9
  eis8 fis gis4 cis, fis2 eis4 | % m. 10
  fis2. e!4 fis8 gis a4 | % m. 11
  gis2. fis | % m. 12
  e4. fis8 g!4~ g fis8 e d! cis | % m. 13
  d4. e8 fis4~ fis e8 d cis b | % m. 14
  cis4 dis eis fis2.~ | % m. 15
  fis4 gis fis eis fis8 eis fis4 | % m. 16
  gis2 fis4~ fis8 e dis4 cis~ | % m. 17
  cis4. gis'8 a b e,!4. a8 b cis | % m. 18
  fis,2.~ fis8 fis fis e e dis | % m. 19
  dis8 gis gis fis fis e e4 cis' b! | % m. 20
  ais2.~ ais4 b8 a! gis!4~ | % m. 21
  gis4 a8 b a gis fis4. gis8 a fis | % m. 22
  gis8 a b2 cis fis,4 | % m. 23
  b4. e,8 fis gis a2 d,4 | % m. 24
  cis2 a'4 gis8 cis cis b b ais | % m. 25
  ais8 b b a! a gis gis2~ gis8 ais | % m. 26
  bis8 cis dis4 gis, cis2 bis4 | % m. 27
  cis4. b!8 cis dis e4. cis8 dis eis | % m. 28
  fis4 fis, eis fis8 cis fis gis a fis | % m. 29
  b4. fis8 gis a! eis cis fis4 cis~ | % m. 30
  cis8 dis eis4 fis8 e! d! cis b2 | % m. 31
  a8 cis fis2~ fis2.~ | % m. 32
  fis8 fis e2 d4. fis8 gis ais | % m. 33
  b8 a! gis fis eis fis eis fis gis2~ | % m. 34
  gis8 gis fis e e dis dis2.~ | % m. 35
  dis8 fis e d! d cis cis2.~ | % m. 36
  cis4 b2 r8 gis' fis e! e dis | % m. 37
  dis8 e e d! d cis cis2~ cis8 dis | % m. 38
  eis4 fis8 eis fis4~ fis2 eis4 | % m. 39
  cis1. \bar "|." | % m. 40
  
}

tenor = \relative c {
  \global
  
  r4 fis gis a2.~ | % m. 1
  a4 gis8 ais b4~ b ais8 gis ais bis | % m. 2
  cis4. b!8 a! cis b a gis2 \prall | % m. 3
  fis2. r8 cis' cis b b ais | % m. 4
  ais8 b b a! a gis gis2~ gis8 ais | % m. 5
  bis8 cis dis4 gis, cis2 bis4 | % m. 6
  cis4. b!8 cis dis e4. cis8 dis eis | % m. 7
  fis4 cis b cis a fis | % m. 8
  b2. eis,8 cis fis4 cis'~ | % m. 9 
  cis8 dis eis4 fis8 e! d! cis b2~ | % m. 10
  b8 a b cis d4~ d8 cis dis eis fis4~ | % m. 11
  fis8 fis e! dis cis b ais4 bis8 cis dis4~ | % m. 12
  dis8 dis cis4 b ais8 b cis2~ | % m. 13
  cis8 cis b4 a! gis8 a b2~ | % m. 14
  b8 d cis b b a a b cis2 | % m. 15
  b2. cis2.~ | % m. 16
  cis8 dis eis4 fis8 e! d! cis b a gis b | % m. 17
  a4 r r r2. | % m. 18
  R1. | % m. 19
  R1. | % m. 20
  R1. | % m. 21
  R1. | % m. 22
  R1. | % m. 23
  R1. | % m. 24
  R1. | % m. 25
  R1. | % m. 26
  R1. | % m. 27
  R1. | % m. 28
  r4 fis gis a2.~ | % m. 29
  a4 gis8 ais b4~ b ais!8 gis ais bis | % m. 30
  cis4. b!8 a! cis b a gis2  | % m. 31
  fis4 a2~ a4 fis b~ | % m. 32
  b2 cis8 b a2 r4 | % m. 33
  r8 fis b4. a8 gis4. gis8 cis4~ | % m. 34
  cis8 e dis cis cis b b2.~ | % m. 35
  b8 d! cis b b a a2.~ | % m. 36
  a4. a8 gis fis eis b' a gis gis fis | % m. 37
  fis8 gis gis fis fis eis eis4 fis8 eis fis4 | % m. 38
  gis4 cis2 r8 fis, cis' b b ais | % m. 39
  ais1. \bar "|." | % m. 40  
  
}

bass = \relative c {
  \global
  
  R1. | % m. 1
  R1. | % m. 2
  R1. | % m. 3
  R1. | % m. 4
  R1. | % m. 5
  R1. | % m. 6
  R1. | % m. 7
  r4 fis, gis a2.~ | % m. 8
  a4 gis8 ais b4~ b ais8 gis ais bis | % m. 9
  cis4. b!8 a! cis b a gis2 -\parenthesize \prall | % m. 10
  fis4. e8 fis gis a2~ a8 b | % m. 11
  cis8 bis cis dis e4~ e8 fis e dis cis bis | % m. 12
  cis4. cis8 d! e fis4. fis,8 gis ais | % m. 13
  b4. b8 cis d e4. e,8 fis gis | % m. 14
  a8 b a gis gis fis fis fis' fis e e dis | % m. 15
  dis8 e e d! d cis cis2~ cis8 dis | % m. 16
  eis8 fis gis4cis, fis2 eis4 | % m. 17
  fis4. eis8 fis gis a4. fis8 gis ais | % m. 18
  b8 b, b a a gis gis a' a gis gis fis | % m. 19
  fis4 gis gis, cis4. cis8 d! e | % m. 20
  fis8 g! fis e d cis b d gis,! fis fis eis | % m. 21
  eis8 eis' fis gis gis a a d! d cis cis b | % m. 22
  b8 a gis d' cis b ais b b a! a gis | % m. 23
  gis8 fis e b' a gis fis gis gis fis fis eis | % m. 24
  eis4 fis fis, cis'8 dis e! fis g! e | % m. 25
  fis8 fis, b cis d! b cis cis, cis' bis cis e! | % m. 26
  dis8 cis bis gis cis e a fis gis fis gis gis, | % m. 27
  cis4 r r r2. | % m. 28
  R1. | % m. 29
  R1. | % m. 30
  R1. | % m. 31
  r4 fis e dis2.~ | % m. 32
  dis4 e8 d! cis4~ cis d8 e d cis | % m. 33
  b4. cis8 d b cis dis eis2 | % m. 34
  fis2.~ fis8 a gis fis fis e | % m. 35
  e2.~ e8 gis fis e e d | % m. 36
  d2. cis~ | % m. 37
  cis2.~ cis~ | % m. 38
  cis8 b a gis fis a d b cis4 cis, | % m. 39
  fis1. \bar "|." \override Staff.RehearsalMark #'direction = #DOWN \mark\markup{\musicglyph #"scripts.dfermata"} | % m. 40
  
}

\score {
  \new StaffGroup
  <<
    \new Staff = "soprano" 
      \soprano
    
    \new Staff = "alto" 
      \alto
    
    \new Staff = "bass" 
      { \clef bass \tenor }
    
    \new Staff = "left" \with {
      \consists "Mark_engraver" }
      { \clef bass \bass }
      
  >>
  
\layout {
  indent = 0.0
  }

}
