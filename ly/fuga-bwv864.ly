\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue XIX"
  subtitle = "in 3 voices"
  opus = "BWV864"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key a \major
  \numericTimeSignature
  \time 9/8
}

soprano = \relative c'' {
  \global
  
  a8 r r r gis cis a d b | % m. 1
  e8 cis fis e a d,~ d cis dis | % m. 2
  gis8 e a gis4 fis8 b, e d~ | % m. 3
  d8 cis dis e b e~ e fis4 | % m. 4
  gis4 a8 gis4 fis8 gis a4~ | % m. 5
  a8 gis b e,4 dis8 e4.~ | % m. 6
  e8 d! cis d4 cis8 b4.~ | % m. 7
  b8 a cis fis,4.~ fis16 gis gis8. -\parenthesize \trill fis32 gis | % m. 8
  a8 r r r gis cis a d b | % m. 9
  e8 cis fis e a d,~ d cis dis | % m. 10
  e4.~ e8 d! g! cis, fis d | % m. 11
  b4.~ b8 a d gis, cis b | % m. 12
  a4.~ a8 gis fis d'4.~ | % m. 13
  d8 cis fis~ fis8 e! d cis4.~ | % m. 14
  cis8 b ais b4 cis8 fis, b ais | % m. 15
  b8 d4~ d8 cis fis~ fis e a~ | % m. 16
  a8 gis16 fis gis8~ gis fis e~ e dis fis | % m. 17
  b,4.~ b4 a8~ a gis4~ | % m. 18
  gis8 fis a d, b' gis cis fis,4 | % m. 19
  e8 e' b cis fis cis d4.~ | % m. 20
  d8 b e cis fis d gis e a | % m. 21
  fis4 b8 e, a4~ a8 gis4 | % m. 22
  a8 r r r4. r | % m. 23
  R1*9/8 | % m. 24
  e,8 r r r cis fis e a fis | % m. 25
  b8 gis cis a d cis fis dis4 | % m. 26
  e16 fis gis e gis a b8 r r r4. | % m. 27
  R1*9/8 | % m. 28
  r8 b16 a gis fis e fis gis e d cis d cis d fis e d | % m. 29
  cis8a d cis a' d, b e cis | % m. 30
  d8 r r r e, a fis b cis~ | % m. 31
  cis8 d g!~ g fis e a, d cis | % m. 32
  d8 fis16 e d cis b8 e4~ e8 d4~ | % m. 33
  d8 cis4 r16 b ais! b cis d e d e fis g! e | % m. 34
  ais16 fis b a! g! fis e d cis b ais b g' fis e d cis b | % m. 35
  b8 d4~ d8 b e cis fis dis | % m. 36
  b4.~ b8 gis cis a4.~ | % m. 37
  a8 b4~ b8 a eis! fis4. | % m. 38
  fis8 dis gis eis16 cis dis eis fis gis a fis gis a b cis | % m. 39
  d!16 b cis dis eis fis d! e! d cis b a gis b a gis fis eis | % m. 40
  b'4.~ b16 a b gis a8~ a16 b gis8. fis16 | % m. 41
  fis8 cis fis e4 a8 fis b gis | % m. 42
  cis8 a d~ d cis b e, a4~ | % m. 43
  a8 gis b e, a4 gis8 cis a | % m. 44
  d4. cis8 fis4~ fis8 e d~ | % m. 45
  d8 cis4~ cis8 b a~ a gis b | % m. 46
  e4.~ e4 d8~ d cis4~ | % m. 47
  cis8 b d gis, e' cis fis b,4 | % m. 48
  a8 e'16 d cis b a b cis a g! fis g fis g b a g | % m. 49
  fis8 d a' gis! e' a, fis' b, gis' | % m. 50
  a8 e4~ e8 d fis b,4.~ | % m. 51
  b8 a g! fis4. b8 e, a | % m. 52
  d,4.~ d8 cis a'~ a a gis | % m. 53
  \set Score.measureLength = #(ly:make-moment 6 8) a2. \fermata \bar "|." | % m. 54
  
}

mezzo = \relative c' {
  \global
  
  R1*9/8 | % m. 1
  e8 r r r cis fis e a fis | % m. 2
  b8 gis cis b e a,~ a gis b | % m. 3
  e,8 a fis gis4. cis8 a d! | % m. 4
  b8 e d~ d cis4 b8 e fis | % m. 5
  b,8 e d~ d cis16 b cis a gis8 cis a | % m. 6
  fis8 b a~ a gis a d,4.~ | % m. 7
  d8 cis e a,4. d4.~ | % m. 8
  d8 cis fis b, e gis~ gis fis gis~ | % m. 9
  gis8 a b cis4. b4 a8~ | % m. 10
  a8 gis cis fis, b4~ b8 a!4 | % m. 11
  gis4.~ gis8 fis4~ fis8 eis gis~ | % m. 12
  gis8 fis e! d4 cis8~ cis b b' | % m. 13
  eis,8 fis4 gis8 ais b e,!4. | % m. 14
  d4 g!8 fis e4 d cis8 | % m. 15
  d8 fis b gis4 cis8 b e cis | % m. 16
  b4.~ b8 a cis fis,4.~ | % m. 17
  fis8 e gis cis,4. fis8 b, e | % m. 18
  cis4. b4 e8~ e4 dis8 | % m. 19
  e8 r r r a e fis b fis | % m. 20
  gis4 r8 e a fis b gis cis | % m. 21
  a8 d4~ d8 cis fis b,8. fis'16 e d | % m. 22
  cis16 d e cis b a gis a b gis fis e fis e fis a gis fis | % m. 23
  gis16 b a gis a b cis b cis d e fis gis b a gis fis a | % m. 24
  gis16 a b fis e d cis8 r r r cis a | % m. 25
  d8 b e cis fis e a fis4 \trill | % m. 26
  e8 r r r dis, gis e a fis | % m. 27
  b8 gis cis b e a,~ a fis b | % m. 28
  gis8 r r r4. r | % m. 29
  R1*9/8 | % m. 30
  r8 a, d~ d cis fis d g! e | % m. 31
  a8 fis b a d g,!~ g fis e | % m. 32
  fis8 r r r16 d' cis b ais gis! fis cis' b a! g! fis | % m. 33
  e16 b' ais gis fis e d8 e4 fis8 b4 | % m. 34
  cis8 fis,4 g!8 e fis b, ais4 | % m. 35
  r8 fis' b gis4.~ gis8 cis a~ | % m. 36
  a8 gis dis e4.~ e8 cis fis | % m. 37
  dis8 gis eis cis4.~ cis8 a d! | % m. 38
  b4.~ b8 a b eis, r r | % m. 39
  R1*9/8 | % m. 40
  r8 gis' dis eis cis fis~ fis eis4 | % m. 41
  fis8 r r r gis, cis a d b | % m. 42
  e8 cis fis e a d,~ d cis dis | % m. 43
  e8 b d!~ d cis dis e4 fis8~ | % m. 44
  fis8 b e,~ e d cis b gis' b | % m. 45
  e,4.~ e8 d fis b,4.~ | % m. 46
  b8 a cis fis4. b8 e, a | % m. 47
  fis4. e4 a8~ a a gis | % m. 48
  a8 r r r4. r | % m. 49
  R1*9/8 | % m. 50
  r8 cis4~ cis8 b a~ a gis e | % m. 51
  cis8 d e~ e a, d~ d cis4~ | % m. 52
  cis8 b d gis, e' cis fis b,4 | % m. 53
  \set Score.measureLength = #(ly:make-moment 6 8) cis2. \fermata \bar "|." | % m. 54
   
}

bass = \relative c' {
  \global
  
  R1*9/8 | % m. 1
  R1*9/8 | % m. 2
  R1*9/8 | % m. 3
  a8 r r r gis cis a d! b | % m. 4
  e8 cis fis e fis d~ d cis dis | % m. 5
  e,8 r r r cis fis e a fis | % m. 6
  b8 gis cis b e a,~ a gis b | % m. 7
  cis,4.~ cis8 a d b e e, | % m. 8
  a8 a'4~ a8 gis e fis b, e | % m. 9
  cis8 fis d a' fis b e, a fis | % m. 10
  cis'4 b8 ais b e, fis4.~ | % m. 11
  fis8 eis gis a,4 b8 cis4 cis,8 | % m. 12
  fis8 r r r eis' a fis b gis | % m. 13
  cis8 a d cis fis b,~ b gis ais | % m. 14
  b8 b, e d g! e fis4 fis,8 | % m. 15
  b8 r r r e a gis cis a | % m. 16
  dis8 b e a,4.~ a8 b fis | % m. 17
  gis4.~ gis8 cis,8 fis dis e4 | % m. 18
  a,4.~ a8 gis cis a b4 | % m. 19
  e,8 r r r4. r | % m. 20
  R1*9/8 | % m. 21
  R1*9/8 | % m. 22
  a8 r r r gis cis a d b | % m. 23
  e8 cis fis e a d,~ d cis dis | % m. 24
  e8 fis gis a r r r a, d | % m. 25
  b8 e cis fis d a' fis b16 a gis fis | % m. 26
  gis16 a b gis fis e dis e fis dis cis b cis b cis e dis cis | % m. 27
  dis16 fis e dis e fis gis fis gis a b cis dis fis e dis cis b | % m. 28
  e8 e, a gis e' a, fis' b, gis' | % m. 29
  a16 fis e d cis b a b cis a g! fis g fis g b a g | % m. 30
  fis16 g! a fis e d a'8. fis16 g a b a g b a g | % m. 31
  fis16 e d fis e d cis a b cis d e fis g! a fis g a | % m. 32
  d,8 r r r cis fis d g! e | % m. 33
  ais8 fis b gis! cis ais d b e~ | % m. 34
  e8 d cis b g! d e fis fis, | % m. 35
  b16 fis' b a! gis! fis e fis e d cis b a b a gis fis e | % m. 36
  dis16 b' e dis cis b cis gis' cis b a gis fis gis fis e dis cis | % m. 37
  b16 cis b a gis fis eis cis' fis e! d! cis d e! d cis b a | % m. 38
  gis16 a gis fis eis dis cis8 a' d! cis fis dis | % m. 39
  gis8 eis a fis b gis cis a d~ | % m. 40
  d16 e d cis b a gis8 eis fis b, cis cis, | % m. 41
  fis8 a fis cis'4 a8 d b e | % m. 42
  cis8 fis d a' a, b cis fis b, | % m. 43
  e4 r8 r cis fis e a fis | % m. 44
  b8 gis cis a d fis, gis e gis | % m. 45
  a8 a, cis d,4 d'8~ d e b | % m. 46
  cis4.~ cis8 fis, b gis a4 | % m. 47
  d,8 d'4~ d8 cis fis d e e, | % m. 48
  a8 a d cis a' d, b' e, cis' | % m. 49
  d16 cis b a gis fis e fis gis e d cis d cis d fis e d | % m. 50
  cis16 b a b cis a d cis d e cis d e d e fis d e | % m. 51
  fis16 gis fis e d cis d e d cis a gis b a gis fis e d | % m. 52
  fis16 a gis fis e d cis cis' d e fis gis a d, e8 e, | % m. 53
  \set Score.measureLength = #(ly:make-moment 6 8) a2. \fermata \bar "|." | % m. 54
   
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
