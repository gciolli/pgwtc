\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue I"
  subtitle = "in 4 voices"
  composer = "BWV 846"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key c \major
  \time 4/4
}
 
soprano = \relative c' {
  \global
 
  R1 | % m. 1
  r2 r8 g' a b | % m. 2 
  c8. d32 c b8 e a, d~ d16 e d c | % m. 3
  b16 g a b c b c d e d e fis g8 b, | % m. 4
  c8 a d16 c b a g8. g16 f e f g | % m. 5
  a16 g a b c2 b4 | % m. 6
  r8 c d e f8. g32 f e8 a | % m. 7
  d,8 g~ g16 a g f e8 a~ a16 b a g | % m. 8
  f2 e8. fis16 g4~ | % m. 9
  g4 fis g16 f! e d c d c b | % m. 10
  a16 c b a g8 r r16 c b a gis8 e' | % m. 11
  d8 c16 b a gis a b c fis, a gis b8 a16 b | % m. 12
  c8 f e d~ d c16 b b8. a16 | % m. 13
  a4 r r2 | % m. 14
  r r8 g a b | % m. 15
  c8. d32 c b8 c d e f!8. g32 f | % m. 16
  e8 a d, g~ g16 a g f e8 a | % m. 17
  d,8 bes' a g16 f g f g e f g g \prall f32 g | % m. 18
  a16 cis, d g e8. d16 d8 r r4 | % m. 19
  r2 r4 r8 g, | % m. 20
  a8 b c8. d32 c b8 e a, d~ | % m. 21
  d16 e d c b c d e f g a g f e d  c | % m. 22
  b4 c8 d g, c4 b8  | % m. 23
  c4 b8 bes a d4 c8  | % m. 24
  d8 e f4~ f16 a g f e f e d \noBreak | % m. 25
  c2 r16 g32a b!16 c d e f8~ \noBreak | % m. 26
  f32 c d e f16 g a8. b16 c2 \fermata \bar "|." | % m. 27
  
}

alto = \relative c' {
  \global
 
  r8 c d e f8. g32 f e8 a | % m. 1
  d,8 g~ g16 a g f e f e d c d c b | % m. 2
  a8 fis' g4~ g8 fis16 e fis8 d | % m. 3
  g8 f! e d c r r g'~ | % m. 4
  g8 f16 e f4~ f16 f e8 d4 | % m. 5
  c8 f r16 g f e f8 d g4~ | % m. 6
  g4 r r2 | % m. 7
  R1 | % m. 8
  r8 g a b c8. d32 c b8 e | % m. 9
  a,8 d~ d16 e d c b8 r r d, | % m. 10
  e8 fis g8. a32 g fis8 b e, a~ | % m. 11
  a16 b a gis fis8 f e d~ d16 e fis gis | % m. 12
  a16 gis a b gis fis gis a b8 r r4 | % m. 13
  r8 c, d e f8. g32 f e8 a | % m. 14
  d,8 g~ g16 a g f e8 e fis g~ | % m. 15
  g8 fis gis a~ a g! a b | % m. 16
  c8. d32 c b8 e a, d~ d16 e d c | % m. 17
  b8 g' cis, d e cis d e | % m. 18
  a,8 r r e fis g a8. b32 a | % m. 19
  g8 c fis, b~ b16 c b a g fis e d | % m. 20
  e4 d~ d16 a' g f! e g f a | % m. 21
  g4~ g16 a bes8 c4 d8  g, | % m. 22
  g4. f8~ f e d4 | % m. 23
  e8 a4 g8~ g f g a | % m. 24
  bes8. c32 bes a8 d g, c~ c16 d c bes | % m. 25
  a bes a g f g f e d4~ d8. g16 | % m. 26
  a4 r16 f' d8 <g e>2 \fermata \bar "|." | % m. 27
  
}

tenor = \relative c {
  \global
 
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  r8 g' a b c8. d32 c b8 e | % m. 4
  a,8 d~ d16 e d c b8 c~ c bes | % m. 5
  a8 d g, c r16 a b c d4 | % m. 6
  g,4 r8 g a b c8. d32 c | % m. 7
  b8 e a, d~ d16 e d c! b!8 e~ | % m. 8
  e4 d c16 b c a e' d c b | % m. 9
  c16 a b c d c b a g4 r | % m. 10
  R1 | % m. 11
  r8 e fis gis a8. b32 a gis8 c | % m. 12
  fis,8 b~ b16 c b a gis8 a4 gis8 | % m. 13
  a4 r8 g a b c8. d32 c | % m. 14
  b8 e a, d~ d g, d'4 | % m. 15
  c8 a e'4 d8 r r4 | % m. 16
  r8 a b cis d8. e32 d c!8 f | % m. 17
  b,8 e~ e16 f e d cis8 r r4 | % m. 18
  r8 a b cis d8. e32 d c!8 fis | % m. 19
  b,8 e~ e16 fis e d c4~ c~ | % m. 20
  c16 d c b a g a fis g8 b c d | % m. 21
  e8. f32 e d8 g c, f~ f16 g f e | % m. 22
  d4 e8 d~ d g, g4~ | % m. 23
  g8 c, d e f8. g32 f e8 a | % m. 24
  d,8 g~ g16 a g f e d e f g a bes g | % m. 25
  a e f g a b! c a b2| % m. 26
  c1 \fermata \bar "|." | % m. 27 
  
}

bass = \relative c {
  \global
 
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  r2 r8 c d e  | % m. 5
  f8. g32 f e8 a d, g~ g16 a g f | % m. 6
  e16 f e d c d c b a8 d a' fis | % m. 7
  g16 a bes g cis,8 d a'4 e | % m. 8
  a16 b c d c b a g c8 r r4 | % m. 9
  r2 r8 g, a b | % m. 10
  c8. d32 c b8 e a, d~ d16 e d c | % m. 11
  b8 e4 d8 c f!4 e8~ | % m. 12
  e8 d4 e8 f!8 e16 d e4 | % m. 13 
  a4 r r2 | % m. 14
  r8 g, a b c8. d32 c b8 e | % m. 15
  a,8 d~ d16 e d c b8 bes a g | % m. 16
  a8 fis' g e d4 e8 f! | % m. 17
  g8. a32 g f8 bes e, a~ a16 b a g | % m. 18
  f16 e f d g8 a d,2~ | % m. 19
  d16 e d c b a g fis e8 e' fis g~ | % m. 20
  g8 a16 g fis8 d g2~ | % m. 21
  g2 a4 b8 c | % m. 22
  f,16 a g f e d c b c d e f g8 g, | % m. 23
  c1~ | % m. 24
  c~ | % m. 25
  c~ | % m. 26
  c \fermata \bar "|." | % m. 27
  
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
