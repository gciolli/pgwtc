\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue XXI"
  subtitle = "in 3 voices"
  opus = "BWV 890"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key bes \major
  \numericTimeSignature
  \time 3/4
}

soprano = \relative c''' {
  \global
  
  R2. | % m. 1
  R2. | % m. 2
  R2. | % m. 3
  R2. | % m. 4
  r8 g f es f bes, | % m. 5
  a8 d c bes c a | % m. 6
  f8 bes bes a a g | % m. 7
  g8 c c bes bes a | % m. 8
  a4 as2~ | % m. 9
  as8 g' f es f as, | % m. 10
  g4 g'2~ | % m. 11
  g8 f es d es c' | % m. 12
  d,2.~ | % m. 13
  d8 es d c d f | % m. 14
  as8 g g f f es | % m. 15
  es8 d d c c bes | % m. 16
  bes2.~ | % m. 17
  bes4 a2~ | % m. 18
  a8 a g f g bes | % m. 19
  e,!4 f g | % m. 20
  a8 bes a g a c | % m. 21
  f2.~ | % m. 22
  f2.~ | % m. 23
  f2 es4~ | % m. 24
  es8 es d c d bes | % m. 25
  g8 d' c bes c a | % m. 26
  f4 bes2~ | % m. 27
  bes4 a r | % m. 28
  R2. | % m. 29
  r8 d c bes c a | % m. 30
  f8 a4 g8 f e! | % m. 31
  f8 r r4 r | % m. 32
  r4 c8 d es!4~ | % m. 33
  es4 d8 e! f4~ | % m. 34
  f4 e!8 f g4~ | % m. 35
  g8 f g4 a8. bes32 c | % m. 36
  f,4 es d | % m. 37
  es4 c d | % m. 38
  es4 d c~ | % m. 39
  c8 c'' bes a bes f | % m. 40
  d8 g f es f d | % m. 41
  bes8 es es d d c | % m. 42
  c8 f f es es d | % m. 43
  d2.~ | % m. 44
  d8 d c bes c a | % m. 45
  fis4 g a~ | % m. 46
  a4 d bes | % m. 47
  g4 a fis | % m. 48
  g2. | % m. 49
  a2. | % m. 50
  d,4 d'2~ | % m. 51
  d4 c8 d es4~ | % m. 52
  es8 es d c d bes | % m. 53
  g8 as g f g bes | % m. 54
  es,8 f es d es bes' | % m. 55
  es2. | % m. 56
  f2. | % m. 57
  bes,8 des des c c bes | % m. 58
  as2.~ | % m. 59
  as8 g f es f d | % m. 60
  es4 g c | % m. 61
  b!8 c d2~ | % m. 62
  d8 d c b! c g | % m. 63
  es8 as g f g es | % m. 64
  c8 f f es es d | % m. 65
  d8 g g f f es | % m. 66
  es2 es'4~ | % m. 67
  es8 f es d es c | % m. 68
  d2.~ | % m. 69
  d4 c8 bes c4~ | % m. 70
  c4 bes8 a bes4~ | % m. 71
  bes4 as8 g as4~ | % m. 72
  as4 g8 f g4~ | % m. 73
  g4 f8 es f4~ | % m. 74
  f4 es8 d es4~ | % m. 75
  es8 f es d es c | % m. 76
  d4 e!2 | % m. 77
  f8 g' f e! f bes, | % m. 78
  a8 d c bes c a | % m. 79
  f8 bes bes as as g | % m. 80
  g8 c c bes bes a! | % m. 81
  a8 bes c2~ | % m. 82
  c4 bes8 c d4~ | % m. 83
  d4 c8 d es!4~ | % m. 84
  es8 d c bes c a | % m. 85
  bes8 c bes a bes g | % m. 86
  es'2.~ | % m. 87
  es8 bes a g a f | % m. 88
  d'2 r4 | % m. 89
  R2. | % m. 90
  r8 g f es f d | % m. 91
  bes8 d4 c8 bes a | % m. 92
  bes2. \bar "|." | % m. 93
    
}

mezzo = \relative c'' {
  \global
  
  r8 c bes a bes f | % m. 1
  d8 g f es f d | % m. 2
  bes8 es es (d) d (c) | % m. 3
  c8 f f (es) es (d) | % m. 4
  d4 c bes | % m. 5
  c4 d es | % m. 6
  d4 e! f~ | % m. 7
  f4 e!8 d e4 | % m. 8
  f8 es! d c d f | % m. 9
  bes,4 d bes | % m. 10
  es8 d c bes c es | % m. 11
  a,4 c f | % m. 12
  bes,4 d f | % m. 13
  bes2.~ | % m. 14
  bes2.~ | % m. 15
  bes2 a4 | % m. 16
  bes8 es, es d d c | % m. 17
  c8 f f es es d | % m. 18
  d2. | % m. 19
  c4 d e! | % m. 20
  f4 r r | % m. 21
  r8 bes a g a f | % m. 22
  es'8 d d c c bes | % m. 23
  bes8 a a g g f | % m. 24
  f2. | % m. 25
  e!4 es2 | % m. 26
  d4 des bes | % m. 27
  e!4 f r | % m. 28
  r8 as as g g f | % m. 29
  e!2. | % m. 30
  f4 d g, | % m. 31
  a8 g' f e! f c | % m. 32
  a8 d c bes c a | % m. 33
  f8 bes bes a a g | % m. 34
  g8 c c bes bes a | % m. 35
  a4 bes c~ | % m. 36
  c8 bes bes as as g | % m. 37
  g4 a! bes~ | % m. 38
  bes8 a a g g f | % m. 39
  f4 r r | % m. 40
  r4 f'2 | % m. 41
  g2. | % m. 42
  a2. | % m. 43
  d,8 c' bes a bes g | % m. 44
  es2.~ | % m. 45
  es8 d d c c bes | % m. 46
  bes2 r4 | % m. 47
  r4 c a | % m. 48
  bes8 g e'! fis g4~ | % m. 49
  g4 fis8 g a4~ | % m. 50
  a8 g g f! f es | % m. 51
  es8 as as g g f | % m. 52
  f2.~ | % m. 53
  f8 f es d es bes | % m. 54
  g8 \clef bass c bes as bes g | % m. 55
  es8 as as g g f | % m. 56
  f8 bes bes as as g | % m. 57
  g4 \clef treble g' e! | % m. 58
  c4 d8 es! f4 | % m. 59
  b,!2. | % m. 60
  c4 es f~ | % m. 61
  f8 as g f g d | % m. 62
  es8 \clef bass f, es d es g | % m. 63
  c4 g8 as bes4~ | % m. 64
  bes4 a!8 b! c4~ | % m. 65
  c4 b!8 c d4~ | % m. 66
  d8 d c b! c g | % m. 67
  a4 c \clef treble f~ | % m. 68
  f4 f bes | % m. 69
  g4 e! a | % m. 70
  d,2 g4 | % m. 71
  es4 c f | % m. 72
  bes,2 es4 | % m. 73
  c4 a d | % m. 74
  g,2 c4 | % m. 75
  \clef bass f,4 c f~ | % m. 76
  f8 c' bes a bes g | % m. 77
  a8 r r4 r | % m. 78
  R2. | % m. 79
  r4 \clef treble d8 es f4~ | % m. 80
  f4 es8 f g4~ | % m. 81
  g8 f es d es c | % m. 82
  d8 g g f f e! | % m. 83
  e!8 a a g g fis | % m. 84
  fis8 g a g a fis | % m. 85
  g4 r r | % m. 86
  r8 bes a g g f | % m. 87
  <f a c>8 r r4 r | % m. 88
  r8 a bes c d f | % m. 89
  bes8 des, des c c bes | % m. 90
  a2. | % m. 91
  bes4 g c, | % m. 92
  d2. \bar "|." | % m. 93
    
}

bass = \relative c' {
  \global
  
  R2. | % m. 1
  R2. | % m. 2
  R2. | % m. 3
  R2. | % m. 4
  R2. | % m. 5
  R2. | % m. 6
  R2. | % m. 7
  R2. | % m. 8
  R2. | % m. 9
  R2. | % m. 10
  R2. | % m. 11
  R2. | % m. 12
  r8 c bes a bes f | % m. 13
  d8 g f es f d | % m. 14
  bes8 es es d d c | % m. 15
  c8 f f es es d | % m. 16
  d8 g g f f es | % m. 17
  es4 f f, | % m. 18
  bes4 bes'2~ | % m. 19
  bes8 a a g g f | % m. 20
  f8 g f e! f c | % m. 21
  a8 d c bes c a | % m. 22
  f8 bes bes a a g | % m. 23
  g8 c c bes bes a | % m. 24
  a4 bes2~ | % m. 25
  bes4 a2 | % m. 26
  bes8 a' g f g e! | % m. 27
  c8 g' f e! f d | % m. 28
  b!4 b b | % m. 29
  bes!4 bes bes | % m. 30
  a4 bes c | % m. 31
  f,4 r r | % m. 32
  r4 a2 | % m. 33
  bes2. | % m. 34
  c2. | % m. 35
  f,8 f' f es es d | % m. 36
  d4 c bes | % m. 37
  es,8 es' es d d c | % m. 38
  c8 f f es es d | % m. 39
  d8 es d c d f | % m. 40
  bes4 bes,8 c d4~ | % m. 41
  d4 c8 d es4~ | % m. 42
  es4 d8 es f4~ | % m. 43
  f8 d e! fis g4~ | % m. 44
  g8 bes a g a c | % m. 45
  d,4 e! fis | % m. 46
  g8 a g fis g d | % m. 47
  bes8 es d c d bes | % m. 48
  g8 c c bes bes a | % m. 49
  a8 d d c c bes | % m. 50
  bes2 b!4 | % m. 51
  c4 c' bes | % m. 52
  as4 bes bes, | % m. 53
  es2.~ | % m. 54
  es8 as g f g es | % m. 55
  c4. d8 es4~ | % m. 56
  es4 d8 es f4~ | % m. 57
  f4 e!8 f g4~ | % m. 58
  g8 f f es! es d | % m. 59
  d4 g g, | % m. 60
  c8 bes bes as as g | % m. 61
  g4 b! g | % m. 62
  c2.~ | % m. 63
  c8 d es2 | % m. 64
  f2. | % m. 65
  g2. | % m. 66
  c,4 es c | % m. 67
  f4 a f | % m. 68
  bes,8 c' bes a bes g | % m. 69
  e!8 a a g g fis | % m. 70
  fis8 a g fis g es | % m. 71
  c8 f! f es es d | % m. 72
  d8 f es d es c | % m. 73
  a8 d d c c bes | % m. 74
  bes8 c c bes bes a | % m. 75
  a2. | % m. 76
  bes4 g c | % m. 77
  f,4. g8 a bes | % m. 78
  c8 d es d es c | % m. 79
  d2. | % m. 80
  es2. | % m. 81
  f2. | % m. 82
  bes,2. | % m. 83
  c2. | % m. 84
  d2. | % m. 85
  g,8 a' g fis g es | % m. 86
  c8 d c bes c a | % m. 87
  f8 g' f es f d | % m. 88
  bes8 c bes a bes g | % m. 89
  e!4 e e | % m. 90
  es!4 es es | % m. 91
  d4 es f | % m. 92
  bes2. \bar "|." | % m. 93
    
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
