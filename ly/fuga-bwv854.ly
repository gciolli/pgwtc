\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
 title = "Fugue IX"
 subtitle = "in 3 voices"
 opus = "BWV 854"
 copyright = "No rights reserved."
 tagline = ""
}

global = {
  \key e \major
  \time 4/4
}

soprano = \relative c'' {
  \global
  
  R1 | % m. 1
  r4 r8 b cis4 r16 fis, gis ais | % m. 2
  b16 ais b cis dis e fis dis b a! b cis b a b cis | % m. 3
  a16 gis a b a gis a b gis fis gis ais b4~ | % m. 4 
  b4 ais b8 b' dis, fis | % m. 5
  b,2~ b8 a gis e' | % m. 6
  fis4 r16 b, cis dis e dis e fis gis a b gis | % m. 7
  e16 dis e fis e dis e fis dis cis dis e fis gis a!8~ | % m. 8
  a dis, gis4~ gis8 fis16 e dis4 | % m. 9
  e16 dis e fis e dis e fis dis cis dis e fis4~ | % m. 10
  fis8 e16 dis e4~ e8 e dis cis | % m. 11
  bis8 gis cis4~ cis8 dis gis, ais16 bis | % m. 12
  cis16 bis cis dis e fis gis e cis b! cis dis cis b cis dis | % m. 13
  b16 ais b cis dis e fis dis b a! b cis b a b cis | % m. 14
  a16 gis a b cis dis e cis dis cis dis e fis gis a fis | % m. 15
  gis16 fis e dis cis b a gis fis gis a fis dis8 gis | % m. 16
  e8 gis cis b! e16 dis cis b a4~ | % m. 17
  a8 dis, a' gis cis16 b a gis fis4~ | % m. 18
  fis8 e16 dis e fis gis e cis gis' fis e dis8 r | % m. 19
  r16 b cis dis e fis gis e cis'4 r16 fis, gis ais | % m. 20
  b16 ais b cis dis e fis gis a,! gis a b a gis a b | % m. 21
  gis16 fis gis a b cis d!8~ d cis16 b cis4~ | % m. 22
  cis4 b r b~ | % m. 23
  b4 a r a~ | % m. 24
  a4 gis8 e' fis4 r16 b, cis dis | % m. 25
  e16 dis e fis gis a b gis a gis a b a gis a b | % m. 26
  gis16 fis gis a gis fis e dis cis b cis d! cis b cis d | % m. 27
  b16 cis dis! b e2 dis4 | % m. 28
  e1 \fermata \bar "|." | % m. 29
  
}

mezzo = \relative c' {
  \global
  
  r4 r8 e fis4 r16 b, cis dis | % m. 1
  e16 dis e fis gis a b gis e dis e fis e dis e fis | % m. 2
  dis16 cis dis e fis gis a! fis gis8 b, e4~ | % m. 3
  e8 dis fis4~ fis8 b, e dis | % m. 4
  cis2 b8 r r4 | % m. 5
  r8 e' gis, b e, fis4 e8 | % m. 6
  e4 dis8 fis b,4 r8 b' | % m. 7
  cis4 r16 fis, gis ais b ais b cis dis e fis dis | % m. 8
  b16 a! b cis b a b cis a gis a b a gis a b | % m. 9
  gis8 e a2~ a16 bis cis dis | % m. 10
  gis,4. cis8 fis,2~ | % m. 11
  fis4 e8 a dis, fis bis, dis | % m. 12
  gis, r r4 r \clef bass ais~ | % m. 13
  ais4 gis r gis~ | % m. 14
  gis4 fis r fis~ | % m. 15
  fis4 e8 cis' dis4 r16 gis, ais bis | % m. 16
  \clef treble cis16 bis cis dis e fis gis e cis b! cis dis cis b cis dis | % m. 17
  b16 gis a b cis dis e cis a gis a b a gis a b | % m. 18
  gis16 fis gis a gis8 b~ b a16 gis a cis b a | % m. 19
  gis8 r r4 r2 | % m. 20
  r4 r8 e' fis4 r16 b, cis dis | % m. 21
  e16 dis e fis gis a b gis e dis! e fis e dis e fis | % m. 22
  dis16 cis dis e fis gis a fis d! cis d e d cis d e | % m. 23
  cis16 b cis d! e fis gis e cis b cis d cis b cis d | % m. 24
  b16 a b cis b a b cis a gis a b a gis a b | % m. 25
  \clef bass gis8 b e, gis~ gis fis~ fis4 | % m. 26
  fis4 e e8 \clef treble e' a4~ | % m. 27
  a4 gis16 e fis gis a b cis8~ cis16 cis b a | % m. 28
  gis1 \fermata \bar "|." | % m. 29
  
}

bass = \relative c {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  r2 r4 r8 e | % m. 3
  fis4 r16 b, cis dis e dis e fis gis a! b gis | % m. 4
  e16 dis e fis e dis e fis dis cis dis e fis gis a fis | % m. 5
  gis16 fis gis a b cis d! b cis b cis dis e dis cis b | % m. 6
  a16 gis a b a gis a b gis8 fis e dis | % m. 7
  cis8 b' ais fis b r r4 | % m. 8
  r2 r4 r8 b | % m. 9
  cis4 r16 fis, gis a b a b cis dis e dis cis | % m. 10
  bis16 gis ais bis cis dis e cis a! gis a b! a gis a b | % m. 11
  gis16 e fis gis a b cis a fis e fis gis fis e fis gis | % m. 12
  e8 dis cis e ais, gis' fisis dis | % m. 13
  gis,8ais b ais gis fis' eis cis | % m. 14
  fis,8 gis a gis fis e' dis cis | % m. 15
  bis8 gis a e' fis16 e fis8 gis gis, | % m. 16
  cis8 r r4 r2 | % m. 17
  R1 | % m. 18
  r4 r8 e fis4 r16 b, cis dis | % m. 19
  e16 dis e fis gis a b gis e dis e fis e dis e fis | % m. 20
  dis8 fis b, e~ e8 dis16 cis dis8 b' | % m. 21
  cis4 r8 gis a gis a e | % m. 22
  fis8 dis gis fis e fis gis e | % m. 23
  a8 b cis b a e fis cis | % m. 24
  dis8 b e2 dis4~ | % m. 25
  dis4 cis <<
    { fis,8. cis'16 dis8 b }
    \new Staff \with { 
    \clef bass
    \remove "Time_signature_engraver"
    \remove "Accidental_engraver"
    alignBelowContext = #"bass"
    fontSize = #-3
      \override StaffSymbol.staff-space = #(magstep -3)
      \override StaffSymbol.thickness = #(magstep -3)
      firstClef = ##f
    } { fis8. e'16 dis8 b }
  >> | % m. 26
  e8 b cis gis a8. e16 fis8 cis | % m. 27
  dis8 b'' cis4 r16 fis, gis a b8 b, | % m. 28
  e,1 \fermata \bar "|." | % m. 29
   
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
