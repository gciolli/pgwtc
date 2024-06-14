\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue XX"
  subtitle = "in 4 voices"
  opus = "BWV 865"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key a \minor
  \time 4/4
}

soprano = \relative c' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  r8 e16 dis e8 fis g g16 fis g8 a | % m. 4
  b16 a g a b8 c dis, b r e | % m. 5
  fis16 g e fis g a fis g a8 g fis b | % m. 6
  e,8 fis16 gis a b c d e8 a, gis4 | % m. 7
  a8 a g! f e c' b a | % m. 8
  gis8 e' d c b16 c d b c b a c | % m. 9
  d16 e f d e4~ e8 a, b16 c d8~ | % m. 10
  d8 c b a g e' d! c! | % m. 11
  b4 r8 a~ a g16 fis! g a b g | % m. 12
  c4 r8 b a16 g a b c8 fis, | % m. 13
  b16 a c b a g fis g e8 e'16 f! e8 d! | % m. 14
  c8 c16 d c8 b a16 b c b a8 g | % m. 15
  fis'8 a r e d16 c e d c b d c | % m. 16
  b8 c d16 b a g e'32 fis e16 fis g fis8. g16 | % m. 17
  g2~ g8 g16 a g8 f | % m. 18
  e16 d c d e f g8~ g16 f e f g a bes g | % m. 19
  a8. bes16 g8. a16 f e f g a8 a, | % m. 20
  d16 g f e e8. d16 d4. e8 | % m. 21
  f2.~ f16 e d c | % m. 22
  b8 c4 b8 c4. bes8 | % m. 23
  a2~ a8 gis a b | % m. 24
  e,4~ e16 fis gis a b c b c d4~ | % m. 25
  d8 c4 b8~ b a16 gis a4~ | % m. 26
  a4 gis a8 a16 gis a8 b | % m. 27
  c8 c16 b c8 d e16 d c d e8 f | % m. 28
  gis,8 e r a b16 c a b c d b c | % m. 29
  d8 c b e a, a'4 g8~ | % m. 30
  g8 c16 b a g fis e dis8 b cis dis | % m. 31
  e16 d! c! b c4 b8 e16 dis e8 fis | % m. 32
  g8 g16 fis g8 a b,2~ | % m. 33
  b4. e16 d c b a b c8 b16 a | % m. 34
  g4 c~ c8 b16 a b8 cis | % m. 35
  d2~ d8 c16 b c8 d | % m. 36
  e8 a, a'4~ a16 gis a gis a8 b | % m. 37
  e,4. d16 c b8 e16 d c b a gis! | % m. 38
  a8 r gis r a4 r | % m. 39
  R1 | % m. 40
  R1 | % m. 41
  R1 | % m. 42
  r8 c16 b c8 d e e16 d e8 f | % m. 43
  g16 f e f g8 a b, g r c | % m. 44
  d16 e c d e f d e f8 e d g | % m. 45
  c,16 b! d c b a g f e8 f16 g a b c d | % m. 46
  b8 a16 g c2 b4 | % m. 47
  c8 e16 f e8 d c cis d e | % m. 48
  a,8 d16 e d8 c b!16 c d c b8 a | % m. 49
  g4 r r2 | % m. 50
  R1 | % m. 51
  R1 | % m. 52
  r2 r8 d'16 e d8 c | % m. 53
  b8 b16 c b8 a g16 a b a g8 fis | % m. 54
  e'8 g r d c16 b d c b a c b | % m. 55
  a8 b c g d'16 e d e e8. -\parenthesize \trill d32 e | % m. 56
  f2~ f8 e16 f e8 d | % m. 57
  c8 c16 d c8 b a16 b c b a8 g! | % m. 58
  f'8 a r e d16 c! e d c b d c | % m. 59
  b4. a8~ a16 b gis a b4~ | % m. 60
  b16 c a b c4~ c16 d b c d c b a | % m. 61
  gis16 b c d e f g!8~ g16 a g f g4~ | % m. 62
  g16 f e f g4~ g16 bes a g f e d cis! | % m. 63
  d16 e d e e8. d16 d2~ | % m. 64
  d4r r2 | % m. 65
  R1 | % m. 66
  r2 r8 e16 f e8 d | % m. 67
  c8 c16 d c8 bes a16 bes c bes a8 g | % m. 68
  f'8 a r e d16 cis e d cis b! d c! | % m. 69
  b!8 cis d a e'2~ | % m. 70
  e16 cis d f g4~ g16 e f a bes4~ | % m. 71
  bes16 g a c d,8 e16 g c,4~ c16 f, g bes!~ | % m. 72
  bes16 bes a g g8. f16 f4 c'4~ | % m. 73
  c1~ | % m. 74
  c8 a es'4~ es8 d c16 bes! d c | % m. 75
  bes16 c d c bes a g f g a bes a g f e d | % m. 76
  e'2 r8 e16 d e8 f | % m. 77
  g8 g16 f g8 a bes16 a g a bes g a e | % m. 78
  f16 e d e f d e cis d c! bes c d bes c a | % m. 79
  <b! e gis>4 \fermata r4 a'2~ | % m. 80
  a8 e16 d e8 fis gis gis16 fis gis8 a | % m. 81
  b16 a gis a b8 c <a, b dis>4 r8 <b e> | % m. 82
  <<
    { c8 b16 a c b a gis a4 r4 }
    \\
    { a4 a8 d, cis4 s4 }
  >> | % m. 83
  r2 r8 d'16 cis d8 e | % m. 84
  f8 f16 e f8 g a16 g f g a8 bes | % m. 85
  cis,8 a r d~ d16 c! b c d e fis gis | % m. 86
  a1 \fermata \bar "|." | % m. 87
   
}

alto = \relative c' {
  \global
  
  r8 a16 gis a8 b c c16 b c8 d | % m. 1
  e16 d c d e8 f gis, e r a | % m. 2
  b16 c a b c d b c d8 c b e | % m. 3
  a,4 g8 fis e c' b a | % m. 4
  g16 fis e fis g8 a b16 a g a b8 c~ | % m. 5
  c8 b16 a b8 e~ e d16 cis d4~ | % m. 6
  d8 c!16 b c4~ c16 \clef treble a' g! f! e d c b | % m. 7
  c16 b c4 d8 e16 d e4 f8~ | % m. 8
  f16 fis e fis gis8 a b4 r8 a | % m. 9
  g!4~ g16 f e g fis4 gis | % m. 10
  a8 a g! fis e c' b a | % m. 11
  g16 fis e fis g8 fis~ fis e16 dis e4~ | % m. 12
  e8 a16 dis, e2~ e8 dis | % m. 13
  e4. dis8 e r r4 | % m. 14
  r8 e fis g a r r4 | % m. 15
  r8 d, e fis g r r4 | % m. 16
  r2 r8 d'16 e d8 c | % m. 17
  b8 b16 c b8 a g4 a8 b | % m. 18
  c4 r8 d cis8. d16 e4~ | % m. 19
  e16 cis d8~ d16 b! cis8 d4 r8 f, | % m. 20
  g16 cis d e cis4 d8 a16 bes a8 g | % m. 21
  f8 a b! c d16 c b a g8 f~ | % m. 22
  f8 e g4 r16 a g f e8 g~ | % m. 23
  g8 f16 g f8 e d d16 e d8 c | % m. 24
  b16 c d c b8 a gis'! b r f! | % m. 25
  e16 d f e d c e d c8 d e16 c b a | % m. 26
  f'16 d e f b,8. a16 a8 r r4 | % m. 27
  R1 | % m. 28
  R1 | % m. 29
  R1 | % m. 30
  r2 r8 e'16 dis e8 fis | % m. 31
  g8 g16 fis g8 a b16 a g a b8 c | % m. 32
  dis,8 b r e fis16 g e fis g a fis g | % m. 33
  a8 g fis b e,4. dis8 | % m. 34
  e8 c'16 b a g fis e d!8 g16 f! e d cis b | % m. 35
  a8 d'16 c! b a gis fis e4 a | % m. 36
  r8 a16 gis a8 b c c16 b c8 d | % m. 37
  e16 d c d e8 f gis, e r a | % m. 38
  b16 c a b c d b c d8 c b e | % m. 39
  a,4~ a16 f' d b g4~ g16 e' c a | % m. 40
  f8 g16 a g f e f e2~ | % m. 41
  e16 d e f g a b c d, e d e f4~ | % m. 42
  f16 a g f e8 a g2~ | % m. 43
  g8 c b a g g f e | % m. 44
  d16 g a b c2 b8 bes | % m. 45
  a8 r r4 r r8 d,~ | % m. 46
  d16 e c d e f d e f8 e d g | % m. 47
  e16 f e d c b a gis a8 a'16 bes a8 g! | % m. 48
  f8 f16 g f8 e d16 e f e d8 c | % m. 49
  b'8 d r a g16 f a g f e g f | % m. 50
  e8 f g d a'16 b a b b8. \trill a32 b | % m. 51
  c16 d c d d8. \trill c32 d e16 d c b a g fis e | % m. 52
  fis8 d e fis g fis g a | % m. 53
  d,4. dis8 e e16 dis e8 r | % m. 54
  r8 g a b c g4 fis16 g | % m. 55
  a8 r r4 r2 | % m. 56
  r16 e' d c b a gis a b2~ | % m. 57
  b8 e,16 f e8 d c c16 d c8 bes | % m. 58
  a16 b! cis b a8 g fis' b! r e, | % m. 59
  d16 c e d c b d c b8 c d!16 c e d | % m. 60
  c8 d e16 d f e d8 e f4~ | % m. 61
  f8 e r r16 bes' a4~ a16 cis e d | % m. 62
  e4~ e16 a, cis d e cis d8 r a16 g | % m. 63
  f8 bes a g~ g f16 e f g a f | % m. 64
  d4 r r2 | % m. 65
  R1 | % m. 66
  R1 | % m. 67
  r8 a'16 b a8 g f f16 g f8 e | % m. 68
  d16 e f e d8 cis bes' d r a | % m. 69
  g16 f a g f e g f e8 f g4 | % m. 70
  f4 e8 a~ a16 cis! d8 g, c!~ | % m. 71
  c16 e f8 bes,4~ bes16 g a c d,4 | % m. 72
  e8 f4 e8 f c'16 d c8 bes! | % m. 73
  a8 a16 bes a8 g fis16 g a g fis8 es | % m. 74
  d '8 fis r c bes16 a c bes a g bes a | % m. 75
  g8 r r4 r2 | % m. 76
  r8 a16 g a8 b cis cis16 b cis8 d | % m. 77
  e16 d cis d e8 f g, e r a | % m. 78
  a8 d a g f d f f | % m. 79
  e4 \fermata r r8 a16 gis a8 b | % m. 80
  c8 c16 b c8 d e16 d c d e8 f! | % m. 81
  gis,8 e r a fis4 r8 e | % m. 82
  e8 f! e4 e r | % m. 83
  R1 | % m. 84
  r8 a16 g a8 b cis cis16 b cis8 d | % m. 85
  <<
    {  e16 d cis! d e8 f gis,4 r8 d'~ | d8 e16 f! <cis e>8 <b d> <a cis!>2 \fermata }
    \\
    { s1 | r8 cis16 d r8 r16 gis, a2 \bar "|."} 
  >> | % mm. 86 - 87
    
}

tenor = \relative c {
  \global
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  R1 | % m. 5
  R1 | % m. 6
  R1 | % m. 7
  R1 | % m. 8
  R1 | % m. 9
  R1 | % m. 10
  r8 e16 dis e8 fis g g16 fis g8 a | % m. 11
  b16 a g a b8 c dis, b r e | % m. 12
  fis16 g e fis g a fis g a8 g fis b | % m. 13
  e,4 b'8 a gis e fis gis | % m. 14
  a8 r r4 r8 a b cis | % m. 15
  d8 r r4 r8 b a4 | % m. 16
  g2~ g8 a4 d,8~ | % m. 17
  d8 \clef treble g'16 a g8 f e e16 f e8 d | % m. 18
  c16 d e d c8 bes a' cis r g | % m. 19
  f16 e g f e d f e d8 e f16 e d c! | % m. 20
  bes8 a16 g a4~ a8 a b! cis | % m. 21
  d8 r r4 r2 | % m. 22
  R1 | % m. 23
  r2 r8 \clef bass b16 c b8 a | % m. 24
  gis4. e8 d f!16 e d8 c | % m. 25
  b8 c d b e4. f8~ | % m. 26
  f8 d e d c r r4 | % m. 27
  r8 a'16 gis a8 b c c16 b c8 d | % m. 28
  e16 d c d e8 f gis, e r a | % m. 29
  b16 c a b c d b c d8 c b e | % m. 30
  a,2~ a8 g16 fis g8 a | % m. 31
  b8 e4 dis8 e r r4 | % m. 32
  R1 | % m. 33
  R1 | % m. 34
  R1 | % m. 35
  r2 r8 a,16 gis! a8 b | % m. 36
  c8 c16 b c8 d e16 d c d e8 f | % m. 37
  gis,8 e r a b16 c a b c d b c | % m. 38
  d8 c b e a,4. g!8~ | % m. 39
  g8 f16 e f4~ f8 e16 d e4~ | % m. 40
  e4 d~ d16 c d e f g a b | % m. 41
  c2.~ c16 e d c | % m. 42
  b8 e16 d c b a b c2~ | % m. 43
  c8 g4 d'8~ d e d c | % m. 44
  g4 r r r8 g | % m. 45
  a16 b g a b c a b c8 b a d | % m. 46
  g,4. c8 d16 f, g a b c d b | % m. 47
  g4 r r2 | % m. 48
  r8 a16 bes a8 g f f16 g f8 e | % m. 49
  d16 e f e d8 c b' d r a | % m. 50
  g16 f a g f e g f e8 f g d | % m. 51
  a'16 b a b b8. -\parenthesize \trill a32 b c2~ | % m. 52
  c8 fis, g a b d, e fis | % m. 53
  g4. a8 b2~ | % m. 54
  b8 e, fis g a d, e d16 e | % m. 55
  fis16 g e fis g a fis g a8 b16 c b c a b | % m. 56
  c8 d16 e d8 c b gis! a b | % m. 57
  c8 e, fis gis a4 r8 e | % m. 58
  f8 g a bes a gis16 fis e8 a | % m. 59
  f!8 e4 f16 dis e2~ | % m. 60
  e1~ | % m. 61
  e8 r r4 r8 e16 f e8 d | % m. 62
  cis8 cis16 d cis8 bes a4 r8 f''16 e | % m. 63
  d4. cis8 d a r4 | % m. 64
  r8 a16 gis a8 b c c16 b c8 d | % m. 65
  e16 d c d e8 f gis, e r a | % m. 66
  b16 c a b c d b cis d8 c! b e | % m. 67
  a,4 r r8 a16 bes c8 bes | % m. 68
  a8 \clef treble d16 e f g a8~ a g4 f8~ | % m. 69
  f8 e d4~ d16 cis e d cis b d cis | % m. 70
  d4~ d16 b cis e f4~ f16 d e g | % m. 71
  a4~ a16 f g bes e,8 f bes,4 | % m. 72
  c2~ c8 \clef bass c, d e | % m. 73
  f8 e f g a16 g fis g a bes c8 | % m. 74
  fis,8 a g fis g f! es d~ | % m. 75
  d8 g16 a g8 f e! e16 f e8 d | % m. 76
  cis16 d e d cis8 b a' cis r \clef treble a' | % m. 77
  bes8. a16 g f e d cis4 r8 cis | % m. 78
  d8 f d a bes f bes d | % m. 79
  b!4 \fermata r c r | % m. 80
  c4 r b r | % m. 81
  r8 b16 c d e fis gis a,4 r8 b | % m. 82
  c8 d c b a r r e' | % m. 83
  a,8 a'16 bes a8 g f f16 g f8 e | % m. 84
  d8 a16 b cis8 d e4 r8 d | % m. 85
  g4 r8 f16 e d4 r8 d16 e | % m. 86
  <<
    { f8 e16 d e8 f e2 }
    \\
    { r8 cis16 b cis8 d e2 \fermata \bar "|." } 
  >> | % m. 87
  
  
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
  r8 a16 gis a8 b c c16 b c8 d | % m. 8
  e16 d c d e8 f! gis, e r a | % m. 9
  b16 c a b cis d b cis d8 c! b e | % m. 10
  a,8 b cis dis! e e16 dis! e8 fis! | % m. 11
  g4 r8 a b b,16 a g8 e | % m. 12
  a4 b c8 b a b16 a | % m. 13
  g8 a b4 e8 d! c b | % m. 14
  a8 b' a g! fis g fis e | % m. 15
  d8 e d c b g'4 fis8 | % m. 16
  g16 e d c b8 e c a d d, | % m. 17
  g8 r r4 r2 | % m. 18
  R1 | % m. 19
  R1 | % m. 20
  r8 a'16 bes! a8 g f f16 g f8 e | % m. 21
  d16 e f e d8 c b'! d r a | % m. 22
  g16 f a g f e g f e8 f g16 e d c | % m. 23
  f16 e d c d e f d b4 c8 d~ | % m. 24
  d8 e d c b d16 c b8 a | % m. 25
  gis8 a b gis a b c f | % m. 26
  d8 b e e, a a' c, e | % m. 27
  a,8 f' e d c a' g! f | % m. 28
  e8 a16 g! f e d c b8 c'16 b a g! f e | % m. 29
  d8 r e r f e d e | % m. 30
  c8 b c a b4. a8 | % m. 31
  g8 e'16 dis e8 fis g g16 fis g8 a | % m. 32
  b16 a g a b8 c dis, b r e | % m. 33
  fis16 g e fis g a fis g a8 g fis b | % m. 34
  e,8 e16 dis e8 fis g2~ | % m. 35
  g8 fis16 e fis8 gis a r r4 | % m. 36
  r8 a g! f e4 r | % m. 37
  r8 a,16 b c8 d~ d c16 d e8 f~ | % m. 38
  f8 e16 d e8 c f e16 d e f e d | % m. 39
  cis8 a d c! b g c b | % m. 40
  a8 b16 c b8 g c4 r8 b | % m. 41
  a4 r8 g f16 g' f e d c b a | % m. 42
  g8 g' a f c' c,16 b c8 d | % m. 43
  e8 e16 d e8 fis g16 f! e f g8 a | % m. 44
  b,8 g r c d16 e c d e f d e | % m. 45
  f8 e d g c, d16 e f4~ | % m. 46
  f4 e8 a d, e16 f g8 g, | % m. 47
  c4 r8 e a,16 bes' a g! f e d cis | % m. 48
  d2~ d4. e8 | % m. 49
  f2~ f16 g f e d4~ | % m. 50
  d8 c4 b8 c d g,4~ | % m. 51
  g8 a' g f e d c e | % m. 52
  d8 d16 e d8 c b b16 c b8 a | % m. 53
  g16 a b a g8 fis e' g r d! | % m. 54
  c16 b d c b a c b a8 b c g | % m. 55
  d'4 e fis! g | % m. 56
  a4 b8 a gis e fis gis | % m. 57
  a8 c, d e f!4 r8 cis | % m. 58
  d8 e f cis d4 a~ | % m. 59
  a8 gis a4 e2~ | % m. 60
  e1~ | % m. 61
  e8 e'16 f e8 d cis cis16 d cis8 b! | % m. 62
  a16 b cis! b a8 g f4 r8 f' | % m. 63
  bes8 g a4 d,8 d16 cis d8 e | % m. 64
  f8 f16 e fis8 gis! a16 gis fis gis a8 b | % m. 65
  c,8 a r d e16 f d e f g! e fis | % m. 66
  g8 f! e a d, a'~ a16 gis a gis | % m. 67
  a4~ a16 g! f e f4~ f16 e d cis | % m. 68
  d8 r r4 r2 | % m. 69
  R1 | % m. 70
  R1 | % m. 71
  R1 | % m. 72
  r8 c16 d c8 bes a a16 bes a8 g | % m. 73
  f16 g a g f8 es d' fis r c | % m. 74
  bes16 a c bes a g bes a g4. fis8 | % m. 75
  g1~ | % m. 76
  g2~ g8 e'16 d g f e d | % m. 77
  cis8 r r4 r8 e' cis a | % m. 78
  d,8 r r4 d8 r r4 | % m. 79
  d4 \fermata r <dis a'> r | % m. 80
  <e a>4 r <e d'> r | % m. 81
  r2 fis4 r8 gis | % m. 82
  <<
    { s2 s8 a16 bes a8 g! | f8 f16 g f8 e d16 e f e d8 cis | d8 f e d g4 r8 f | e4 r8 d b'2 | a1 \fermata }
    \\
    { a8 d, e4 a,2~ | a1~ | a1~ | a1~ | a1 \fermata } 
  >> | % mm. 83 - 87
  
}

\score {
  \new StaffGroup
  <<
    \new Staff = "soprano" 
      \soprano
    
   \new Staff = "alto" 
     { \clef bass \alto }
    
    \new Staff = "tenor" 
      { \clef bass \tenor }
    
    \new Staff = "bass" 
      { \clef bass \bass }
    
  >>
  
\layout { 
  indent = 0.0
  }

}
