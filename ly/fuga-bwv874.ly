\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

#(ly:expect-warning "Previous mark event here")
#(ly:expect-warning "Two simultaneous mark events, junking this one")

\header {
  title = "Fugue V"
  subtitle = "in 4 voices"
  opus = "BWV 874"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key d \major
  \time 2/2
}

soprano = \relative c'' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  r8 d d d g,4 b~ | % m. 5
  b8 e, a g fis2 | % m. 6
  gis2 a4 r8 e | % m. 7
  a8 g fis2 e8 d | % m. 8
  cis4 d~ d8 b'16 a b8 d, | % m. 9
  cis4 r r8 e a g | % m. 10
  fis2 r8 b b b | % m. 11
  e,4 g~ g8 cis, fis e | % m. 12
  d8 fis b a gis b e d | % m. 13
  cis4 r r8 d d d | % m. 14
  g,4 b~ b8 e, a g | % m. 15
  fis4 r8 a d c! b d | % m. 16
  g4 fis8 e a g fis e | % m. 17
  d4 cis8 b e d cis b | % m. 18
  a8 cis b a gis! e a4~ | % m. 19
  a4. gis8 a2~ | % m. 20
  a8 fis b a g4 r | % m. 21
  r8 fis' fis fis b,4 d!~ | % m. 22
  d8 gis, cis b ais4 b8 cis | % m. 23
  d2. cis4~ | % m. 24
  cis2~ cis8 b16 a gis8 fis | % m. 25
  eis2~ eis8 cis fis4~ | % m. 26
  fis4 eis fis r8 a' | % m. 27
  a8 a d,4 fis4. b,8 | % m. 28
  e8 d cis4 r8 a d c! | % m. 29
  b4 r8 b e d c!4~ | % m. 30
  c2 b~ | % m. 31
  b8 a4 g8~ g8 fis16 e fis4 | % m. 32
  g8 r r4 r8 g' g g | % m. 33
  cis,4 e~ e8 a, d c! | % m. 34
  b2~ b8 e, a g! | % m. 35
  fis4 cis'~ cis8 fis, b a! | % m. 36
  g2~ g4. e8 | % m. 37
  a8 g fis a d cis b d | % m. 38
  g2~ g8 e a g | % m. 39
  fis8 g a4~ a8 d, g fis | % m. 40
  e2. d8 c! | % m. 41
  b2~ b8 e, a g | % m. 42
  fis4 e fis8 gis a4~ | % m. 43
  a4 gis a8 a' a a | % m. 44
  d,4 fis~ fis8 b, e d | % m. 45
  cis2 c!~ | % m. 46
  c8 a d c! b2~ | % m. 47
  b4 a2.~ | % m. 48
  a4 g~ g8 e a g | % m. 49
  fis4 e d2 \bar "|." \override Staff.RehearsalMark #'break-visibility = #begin-of-line-invisible \mark\markup{\musicglyph #"scripts.ufermata"} | % m. 50
    
}

alto = \relative c'' {
  \global
  
  R1 | % m. 1 
  r2 r8 a a a | % m. 2
  d,4 fis~ fis8 b, e d | % m. 3
  cis8 e a g fis4 a~ | % m. 4
  a8 d, g fis e fis g4~ | % m. 5
  g4 fis8 e d4 r | % m. 6
  r4 r8 \clef bass b e d cis4~ | % m. 7
  cis4 r8 a d cis b4 | % m. 8
  a4 gis8 fis e2 | % m. 9
  r8 e' e e a,4 c!~ | % m. 10
  c8 fis, b a g b e d | % m. 11
  cis8 b ais b cis e d cis | % m. 12
  b4 r r2 | % m. 13
  r8 \clef treble a' a a d,4 fis~ | % m. 14
  fis8 b, e d cis2 | % m. 15
  r8 a d c! b d g fis | % m. 16
  e8 d cis!4 d8 e fis4~ | % m. 17
  fis4 gis2 a8 e | % m. 18
  a8 gis fis4 e d8 cis | % m. 19
  b8 cis d4~ d8 cis fis e | % m. 20
  dis4 r r8 b e d! | % m. 21
  cis8 r r4 r8 b' b b | % m. 22
  e,!4 g!~ g8 cis, fis e | % m. 23
  d8 fis b a gis2~ | % m. 24
  gis8 cis, fis e d4. cis8 | % m. 25
  b2~ b8 a16 b cis8 d | % m. 26
  cis4. b8 a r r4 | % m. 27
  r8 a' a a d, r r4 | % m. 28
  r8 e a g fis4 r8 d | % m. 29
  g8 fis e4 r r8 e | % m. 30
  a8 g fis d g fis e d | % m. 31
  c!4 b e d~ | % m. 32
  d8 r r b' b b e,4 | % m. 33
  g4. cis,8 fis e d4~ | % m. 34
  d4 r8 b e d cis4~ | % m. 35
  cis4 r8 cis fis e d4~ | % m. 36
  d4 r8 b e d cis4~ | % m. 37
  cis8 cis fis e d fis b a | % m. 38
  g8 b e d cis2 | % m. 39
  d8 e fis4~ fis8 b, e d | % m. 40
  cis4 r8 e, a g fis e | % m. 41
  d4 cis!8 d e d cis e~ | % m. 42
  e8 d4 cis8 c! b a c | % m. 43
  d8 e16 f! e4~ e8 r r fis'! | % m. 44
  fis8 fis b,4 d4. gis,8 | % m. 45
  cis8 b a4~ a8 g fis e | % m. 46
  fis2 r8 d g fis | % m. 47
  e8 d cis4 r8 a d c! | % m. 48
  b4. b8 e d cis e~ | % m. 49
  e8 d4 cis8 a2 \bar "|." | % m. 50
    
}

tenor = \relative c' {
  \global
  
  r8 d d d g,4 b~ | % m. 1
  b8 e, a g fis4 d | % m. 2
  r8 fis b a gis2 | % m. 3
  a4 cis r8 a d cis | % m. 4
  b2 r8 b e d | % m. 5
  cis2 r8 a d cis | % m. 6
  b2 r8 e, a g | % m. 7
  fis8 e d4 r8 d g fis | % m. 8
  e4 d8 cis b2 | % m. 9
  e2~ e8 g fis e | % m. 10
  dis2 e8 d! g4~ | % m. 11
  g4 cis,8 d e g fis4~ | % m. 12
  fis8 a! gis fis e2~ | % m. 13
  e8 e a g fis e d4 | % m. 14
  e8 fis g4~ g8 g fis e | % m. 15
  d4 r r2 | % m. 16
  r8 e a g fis a d cis | % m. 17
  b4 r r8 b cis4~ | % m. 18
  cis8 a d cis b4 a8 gis | % m. 19
  fis4 e~ e r | % m. 20
  r8 b' b b e,4 g~ | % m. 21
  g8 cis, fis e dis eis16 fis eis4 | % m. 22
  r8 b' ais cis~ cis e d cis | % m. 23
  b2~ b8 gis! cis b | % m. 24
  a2~ a8 fis b a | % m. 25
  gis2~ gis8 fis16 gis a4 | % m. 26
  gis8 fis gis4 fis8 r r4 | % m. 27
  r2 r8 d g fis | % m. 28
  e4 r8 a d c! b4~ | % m. 29
  b8 a g4 r8 e a g | % m. 30
  fis8 e d fis b a g4~ | % m. 31
  g8 a b e a,2 | % m. 32
  g8 d' d d g,4 b~ | % m. 33
  b8 e, a g fis2~ | % m. 34
  fis8 b, e d cis! d e4~ | % m. 35
  e8 cis fis e d r r fis | % m. 36
  b8 a g4 r2 | % m. 37
  R1 | % m. 38
  R1 | % m. 39
  r8 d' d d g,4 b~ | % m. 40
  b8 e, a g fis g a4~ | % m. 41
  a8 d, g fis e4 r | % m. 42
  R1 | % m. 43
  r8 b' b b e, r r4 | % m. 44
  r8 d' d d gis,4 b~ | % m. 45
  b8 e, a g fis4. a8 | % m. 46
  d8 c! b a g2~ | % m. 47
  g8 e a g fis2~ | % m. 48
  fis8 b, e d cis d e4 | % m. 49
  r8 d a' g fis2 \bar "|." | % m. 50
    
}

bass = \relative c' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  R1 | % m. 5
  r8 a a a d,4 fis~ | % m. 6
  fis8 b, e d cis b a4 | % m. 7
  r8 a d cis b a g4~ | % m. 8
  g8 fis b a gis2 | % m. 9
  a4 g! fis2 | % m. 10
  b1~ | % m. 11
  b2. ais4 | % m. 12
  b4 r r8 d cis b | % m. 13
  a4 r8 a b4 r8 b | % m. 14
  e,4 r8 e' a,4 r8 a | % m. 15
  d,4 r r2 | % m. 16
  R1 | % m. 17
  r8 b' e d cis e a gis | % m. 18
  fis8 e d4~ d8 cis fis e | % m. 19
  d4 e a, r8 fis | % m. 20
  b4 r r8 d!cis b | % m. 21
  ais4 r8 ais a!4 r8 gis | % m. 22
  cis4 r8 e fis, fis' gis ais! | % m. 23
  b8 a! gis fis eis2 | % m. 24
  r8 fis fis fis b,4 d~ | % m. 25
  d8 gis, cis b a4. b8 | % m. 26
  cis2 fis,8 a' a a | % m. 27
  d,4 fis~ fis8 b, e d | % m. 28
  cis8 b a4 r2 | % m. 29
  r8 b e d c! b a4~ | % m. 30
  a4 b~ b8 d g fis | % m. 31
  e8 fis g e c! a d c | % m. 32
  b8 d g fis e2~ | % m. 33
  e8 d cis a d c! b a | % m. 34
  g4 gis a2 | % m. 35
  ais2 b~ | % m. 36
  b8 b e d cis e a g | % m. 37
  fis8 a d cis b a g fis | % m. 38
  e8 d cis b a g fis e | % m. 39
  d8 r r d' e4 r8 e | % m. 40
  a8 g fis e d e fis4~ | % m. 41
  fis8 b, e d cis b a4~ | % m. 42
  a8 a' a a dis, e f!4~ | % m. 43
  f8 b, e d cis a d cis | % m. 44
  b8 r r b' b b e,4 | % m. 45
  g!4. cis,8 fis e d4~ | % m. 46
  d4 r8 d g fis e d | % m. 47
  cis!4 r8 a d c! b a | % m. 48
  g2 a~ | % m. 49
  a2 d, \bar "|." \override Staff.RehearsalMark #'direction = #DOWN \mark\markup{\musicglyph #"scripts.dfermata"} | % m. 50
    
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
    
    \new Staff = "bass" \with {
      \consists "Mark_engraver" }
      { \clef bass \bass }
      
  >>
  
\layout { 
  indent = 0.0
  }

}
