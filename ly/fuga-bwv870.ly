\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue I"
  subtitle = "in 3 voices"
  opus = "BWV 870"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key c \major
  \numericTimeSignature
  \time 2/4
  
}

soprano = \relative c'' {
  \global
  
  R2 | % m. 1
  R2 | % m. 2
  R2 | % m. 3
  R2 | % m. 4
  r8 c16 b c8 g | % m. 5
  e'4 \mordent d | % m. 6
  r8 c16 b c d b c | % m. 7
  a16 c b c d e c d | % m. 8
  b16 a b c b c a b | % m. 9
  c8. a16 bes8. g16 | % m. 10
  a4 b!8 c~ | % m. 11
  c8 c4 b8 | % m. 12
  c8 g'16 f g8 c, | % m. 13
  a'4 g | % m. 14
  fis8 a16 g a8 d, | % m. 15
  b'4 a | % m. 16
  gis8 b16 a b8 e, | % m. 17
  c'2~ | % m. 18
  c8 c b g | % m. 19
  e4 fis8 g~ | % m. 20
  g8 fis16 e fis8. g16 | % m. 21
  g8 e cis d~ | % m. 22
  d16 g bes a g f e d | % m. 23
  cis8 d4 cis8 | % m. 24
  r8 d16 cis d8 a | % m. 25
  f'4 e | % m. 26
  r8 d16 c d e c d | % m. 27
  b16 d c d e f! d e | % m. 28
  c8 e16 d e8 c | % m. 29
  a'8 c, a d | % m. 30
  b8 d16 c d8 b | % m. 31
  g'8 bes, g c | % m. 32
  a8 d16 c d e c d | % m. 33
  b16 a b c b c a b | % m. 34
  g8 c16 b c d bes c | % m. 35
  a16 g a bes a bes g a | % m. 36
  f2~ \trill | % m. 37
  f8 d'16 c d8 f, | % m. 38
  e16 d e f e f d e | % m. 39
  c8. c16 b8. b16 | % m. 40
  a4. b8 | % m. 41
  c4. a8 | % m. 42
  d4. b8 | % m. 43
  e4. c8 | % m. 44
  f4. d8 | % m. 45
  g4 a8 b | % m. 46
  c2~ | % m. 47
  c2~ | % m. 48
  c2~ | % m. 49
  c4 b | % m. 50
  r8 c16 b c8 g | % m. 51
  e'4 d | % m. 52
  r8 c16 b c d b c | % m. 53
  a16 c b c d e c d | % m. 54
  b8 d16 c d8 g, | % m. 55
  e'4 d | % m. 56
  c8 e16 d e8 a, | % m. 57
  f'4 e | % m. 58
  d8 f16 e f8 b, | % m. 59
  g'4 f | % m. 60
  e8 f g4~ | % m. 61
  g4 f~ | % m. 62
  f4 e~ | % m. 63
  e4 d8 c | % m. 64
  b8 g c4~ | % m. 65
  c4 b | % m. 66
  c8 d g,16 c8 b16 | % m. 67
  c16 f, e16 d e f d e | % m. 68
  f16 e f g f g e f | % m. 69
  d16 c d e d e c d | % m. 70
  e16 d e f e f d e | % m. 71
  c2~ | % m. 72
  c2~ | % m. 73
  c2~ | % m. 74
  c8 b c d | % m. 75
  e8 g16 f g8 c, | % m. 76
  a'4 g | % m. 77
  r8 f16 e f8 d | % m. 78
  d4 f | % m. 79
  <c e>8 <f as> <e g> <as b> | % m. 80
  <g c>8 <d f> e c' | % m. 81
  d,8 c' f, b | % m. 82
  <g c>2 \bar "|." | % m. 83
  
}

mezzo = \relative c'' {
  \global
  
  r8 g16 f g8 c, | % m. 1
  a'4 \mordent g | % m. 2
  r8 f16 e f g e f | % m. 3
  d16 f e f g a f g | % m. 4
  e16 d e f e f d e | % m. 5
  c16 b c d c d b c | % m. 6
  a16 b c d e fis g8~ | % m. 7
  g8 fis16 e fis4 | % m. 8
  g2~ | % m. 9
  g16 e f8~ f16 d e8~ | % m. 10
  e8 d g4 | % m. 11
  f16 a g a d,4 | % m. 12
  c8 r r4 | % m. 13
  r8 c'16 b c8 g | % m. 14
  d'4 c | % m. 15
  b8 d16 c d8 a | % m. 16
  e'4 d | % m. 17
  c8 e16 d e8 a | % m. 18
  d,2~ | % m. 19
  d8 c16 b a8 b~ | % m. 20
  b8 a16 g a8 d, | % m. 21
  b'4 a | % m. 22
  r8 g16 f g a f g | % m. 23
  e16 g f g a bes g a | % m. 24
  f16 e f g f g e f | % m. 25
  d16 c d e d e c d | % m. 26
  b16 c d e fis gis a8~ | % m. 27
  a8 gis16 fis gis4 \trill | % m. 28
  a16 gis a b a b g! a | % m. 29
  fis16 e fis g fis g e fis | % m. 30
  g16 fis g a g a f! g | % m. 31
  e16 d e f e f d e | % m. 32
  f16 e f g f g e d | % m. 33
  d8 g16 f g a f g | % m. 34
  e16 d e f e f d e | % m. 35
  c8 f16 e f g e f | % m. 36
  d16 c d e d e c d | % m. 37
  b16 a b c b c a b | % m. 38
  c4. \clef bass b8~ | % m. 39
  b16 b a8~ a16 a g8~ | % m. 40
  g8 g fis g~ | % m. 41
  g16 fis g e fis4~ | % m. 42
  fis8 b g4~ | % m. 43
  g8 c a4~ | % m. 44
  a8 d b4~ | % m. 45
  b8 \clef treble b c d | % m. 46
  e8 g16 f g8 c, | % m. 47
  a'4 g | % m. 48
  r8 f16 e f g e f | % m. 49
  d16 f e f g a f g | % m. 50
  e16 d e f e f d e | % m. 51
  c16 b c d c d b c | % m. 52
  a16 b c d e fis g8~ | % m. 53
  g8 g4 fis8 | % m. 54
  g4 r | % m. 55
  r8 g16 f g8 d | % m. 56
  a'4 g! | % m. 57
  f8 a16 g a8 e | % m. 58
  b'4 a | % m. 59
  g8 b16 a b8 g | % m. 60
  c4 bes | % m. 61
  a2 | % m. 62
  g2 | % m. 63
  f2~ | % m. 64
  f4 e | % m. 65
  d4~ d16 f e d | % m. 66
  g16 a f g e8 d | % m. 67
  c2~ | % m. 68
  c2~ | % m. 69
  c2~ | % m. 70
  c8 c4 b8 | % m. 71
  c8 \clef bass g16 f g8 c, | % m. 72
  a'4 g | % m. 73
  r8 f16 e f8 d | % m. 74
  g4 \clef treble a8 b | % m. 75
  c8 bes16 a bes8 e | % m. 76
  f4 e | % m. 77
  R2 | % m. 78
  r8 d16 c d8 g, | % m. 79
  r8 b c d | % m. 80
  e8 b c e | % m. 81
  <<
    { d8 r d r | e2 }
    \new Staff \with {
    \key c \major
    \remove "Time_signature_engraver"
    alignBelowContext = #"mezzo"
    \clef bass
    } { a,!8 r g r | c2 \bar "|." }
  >> | % m. 82-83
    
}

bass = \relative c' {
  \global
  
  R2 | % m. 1
  R2 | % m. 2
  R2 | % m. 3
  R2 | % m. 4
  R2 | % m. 5
  R2 | % m. 6
  R2 | % m. 7
  R2 | % m. 8
  r8 g16 f g8 c, | % m. 9
  a'4 \mordent g | % m. 10
  r8 f16 e f g e f | % m. 11
  d16 f e f g a f g | % m. 12
  e16 d e f e f d e | % m. 13
  f16 e f g f g e f | % m. 14
  d16 cis d e fis g e fis | % m. 15
  g16 fis g a g a f! g | % m. 16
  e16 d e fis gis a fis gis | % m. 17
  a16 gis a b a b g! a | % m. 18
  fis16 g e fis g a b g | % m. 19
  c16 b c d c d b c | % m. 20
  d2 | % m. 21
  g,8 \clef treble g'16 f! g a f g | % m. 22
  e2 | % m. 23
  r8 \clef bass d f, a | % m. 24
  d,8 r r4 | % m. 25
  R2 | % m. 26
  R2 | % m. 27
  R2 | % m. 28
  R2 | % m. 29
  R2 | % m. 30
  R2 | % m. 31
  R2 | % m. 32
  R2 | % m. 33
  R2 | % m. 34
  R2 | % m. 35
  R2 | % m. 36
  R2 | % m. 37
  R2 | % m. 38
  r8 c16 b c8 g | % m. 39
  e'4 d | % m. 40
  r8 c16 b c d b c | % m. 41
  a16 c b c d e c d | % m. 42
  b16 d c d e f! d e | % m. 43
  c16 e d e f g e f | % m. 44
  d16 f e f g a f g | % m. 45
  e16 d e f e f d e | % m. 46
  c16 d e d c bes a g | % m. 47
  f16 e f g f g e f | % m. 48
  d16 a' b c d e f d | % m. 49
  g2~ | % m. 50
  g2~ | % m. 51
  g2 | % m. 52
  r4 r8 b | % m. 53
  c8 a d d, | % m. 54
  g,16 fis g a b c a b | % m. 55
  c16 b c d c d b c | % m. 56
  a16 gis a b c d b c | % m. 57
  d16 c d e d e c d | % m. 58
  b16 a b c d e c d | % m. 59
  e16 d e f e f d e | % m. 60
  c16 b c d e f d e | % m. 61
  f16 g f e d c b a | % m. 62
  b16 g a b c d b c | % m. 63
  a16 c b c d e f d | % m. 64
  g16 a f g a b g a | % m. 65
  f16 g e f g a f g | % m. 66
  e8 f g gis | % m. 67
  a,8 g'!16 f g8 c, | % m. 68
  a'4 g | % m. 69
  r8 f16 e f8 d | % m. 70
  g4 f | % m. 71
  e16 d e f e f d e | % m. 72
  f16 e f g f g e f | % m. 73
  d16 c d e d c b a | % m. 74
  g16 f g a g f e d | % m. 75
  c16 c' e c c, c' g' c, | % m. 76
  c,16 d e f g a b c | % m. 77
  c,16 c' d c c, c' f c | % m. 78
  b16 c b a g f e d | % m. 79
  c16 c' d c e c f c | % m. 80
  g'16 c, as' c, g' c, b c | % m. 81
  f,8 r g r | % m. 82
  c,2 \bar "|." | % m. 83
    
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
