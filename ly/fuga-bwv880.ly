\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue XI"
  subtitle = "in 3 voices"
  opus = "BWV 880"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key f \major
  \numericTimeSignature
  \time 6/16
}

soprano = \relative c' {
  \global
  
  r8. f16 e f | % m. 1
  c'8-. r16 a g a | % m. 2
  d8-. r16 c d e | % m. 3
  f16 e d c d bes | % m. 4
  a16 g f e8 bes'16 | % m. 5
  a8 d16 g,8 d'16 | % m. 6
  c8 f16 b,!8 f'16 | % m. 7
  e16 d c g'8 b,!16 | % m. 8
  c8.~ c16 bes! a | % m. 9
  bes16 c d es8.~ | % m. 10
  es16 d c d e! f | % m. 11
  g8.~ g16 f e | % m. 12
  f16 g a bes8.~ | % m. 13
  bes16 a g a bes a | % m. 14
  g8.~ g16 c e, | % m. 15
  f8. e16 f g | % m. 16
  c,8 f16 f8 e16 | % m. 17
  f8 r16 r8. | % m. 18
  g16 f e d es c | % m. 19
  bes16 a g a8 d16 | % m. 20
  g,8 d'16 g,8 e'16~ | % m. 21
  e16 cis d~ d b! c!~ | % m. 22
  c16 e f b,! c d | % m. 23
  g,8 c16 c8 b!16 | % m. 24
  c8 g16 bes!8. | % m. 25
  a8.~ a8 d16 | % m. 26
  g,8.~ g8 g16 | % m. 27
  c8.~ c16 c b! | % m. 28
  c8 r16 r8. | % m. 29
  g'16 f e d e c | % m. 30
  b!16 a g cis8 r16 | % m. 31
  a'16 g f e f d | % m. 32
  cis16 b! a d8.~ | % m. 33
  d16 c! b! c8.~ | % m. 34
  c16 bes! a bes8.~ | % m. 35
  bes16 c bes a bes g | % m. 36
  fis16 e d g8.~ | % m. 37
  g16 f! e f g a | % m. 38
  bes8.~ bes16 a g | % m. 39
  a8.~ a16 bes c | % m. 40
  d8.~ d16 c bes | % m. 41
  c8.~ c16 d e | % m. 42
  f8.~ f16 e d | % m. 43
  e8.~ e16 f g | % m. 44
  a16 g f e f32 e d c | % m. 45
  f4. | % m. 46
  es16 g fis g8.~ | % m. 47
  g8. fis16 g a | % m. 48
  bes,16 d c bes c a | % m. 49
  g8 es'16 cis8 a16~ | % m. 50
  a16 a d cis d e | % m. 51
  d16 e f bes,8.~ | % m. 52
  bes16 e g c,8.~ | % m. 53
  c16 bes a bes8 r16 | % m. 54
  r16 r bes a8 g16 | % m. 55
  f8.~ f16 es d | % m. 56
  c8.~ c16 d es | % m. 57
  f8.~ f16 g a | % m. 58
  bes8.~ bes16 c d | % m. 59
  es4.~ | % m. 60
  es16 d c d es d | % m. 61
  c8.~ c16 c f | % m. 62
  bes,8.~ bes16 bes es | % m. 63
  a,8.~ a16 a d | % m. 64
  g,8.~ g16 g c | % m. 65
  f,8.~ f8 bes16~ | % m. 66
  bes16 g a~ a8 d16 | % m. 67
  bes8. a16 bes c | % m. 68
  d16 c es d es c | % m. 69
  bes4.~ | % m. 70
  bes4.~ | % m. 71
  bes8.~ bes16 g as | % m. 72
  as8.~ as16 f g | % m. 73
  g8.~ g16 e f | % m. 74
  f8.~ f16 d! e | % m. 75
  e8 r16 as8.~ | % m. 76
  as16 f g g8.~ | % m. 77
  g16 f e d c' f | % m. 78
  bes,8.~ bes16 e g | % m. 79
  c,8.~ c16 f as | % m. 80
  des,8 f16 bes8 des,16 | % m. 81
  c8 bes'16 as8 f16 | % m. 82
  <<
    { \stemUp s4. | s4. | s4. | c8 r16 a! g a }
    \\
    {  b!16 f' g as g f~ | f16 e! des~ des c bes~ | bes16 as g f e f~ | \stemDown f16 d e s8. }
  >> | % m. 83- 86
  <des' bes>8 r16 c d! e! | % m. 87
  f16 e d c d bes | % m. 88
  a16 f32 g a bes c8.~ | % m. 89
  c8. [bes~] | % m. 90
  bes16. g32 a bes c8.~ | % m. 91
  c16. a32 bes c d8.~ | % m. 92
  d32 bes c d e f g f e d c bes | % m. 93
  a16 c bes c8.~ | % m. 94
  c8 d16 es8. | % m. 95
  d8.~ d8 g16 | % m. 96
  e8.~ e8 a16 | % m. 97
  f8 d16 a bes g | % m. 98
  \set Score.measureLength = #(ly:make-moment 3 16) f8. \bar "|." | % m. 99
    
}

mezzo = \relative c' {
  \global
  
  R4. | % m. 1
  R4. | % m. 2
  R4. | % m. 3
  R4. | % m. 4
  r8. c16 bes c | % m. 5
  f8 r16 e d e | % m. 6
  a8 r16 g a b! | % m. 7
  c16 b! a g a f | % m. 8
  e16 d c d e fis | % m. 9
  g8.~ g16 f! es | % m. 10
  f16 g a bes8.~ | % m. 11
  bes16 a g a b! cis | % m. 12
  d8.~ d16 c! bes! | % m. 13
  c16 d e f8.~ | % m. 14
  f16 e d c8.~ | % m. 15
  c16 a bes~ bes8. | % m. 16
  a8 bes16 a8 g16 | % m. 17
  c16 bes a g a f | % m. 18
  e16 d c fis8. | % m. 19
  g8 r16 f!8.~ | % m. 20
  f8. [e] | % m. 21
  a8. [g] | % m. 22
  f4.~ | % m. 23
  f16 e f e8 d16 | % m. 24
  c8.~ c8 g'16~ | % m. 25
  g8 c,16 f8.~ | % m. 26
  f8 b,!16 c d e | % m. 27
  f16 g a d,8. | % m. 28
  e16 g f e f d | % m. 29
  c8 r16 r8. | % m. 30
  d16 c b! \clef bass a bes g | % m. 31
  f16 e d g8.~ | % m. 32
  g8. f16 e d | % m. 33
  e16 fis gis a8 r16 | % m. 34
  r8. g16 a bes | % m. 35
  c4.~ | % m. 36
  c8. bes16 a g | % m. 37
  a16 b! cis d8.~ | % m. 38
  d16 c! bes \clef treble c8.~ | % m. 39
  c16 d e f8.~ | % m. 40
  f16 e d e8.~ | % m. 41
  e16 f g a8.~ | % m. 42
  a16 g f g8.~ | % m. 43
  g16 a b! c8 r16 | % m. 44
  R4. | % m. 45
  r16 r d b!8 g16~ | % m. 46
  g8 c16 b! c d | % m. 47
  c16 d es a,8.~ | % m. 48
  a8 g16 g8 fis16 | % m. 49
  g4. | % m. 50
  f8 g16 a8 r16 | % m. 51
  r8. \clef bass f,16 e f | % m. 52
  c'8 r16 a g a | % m. 53
  d8 r16 c d e | % m. 54
  f16 e d c d bes | % m. 55
  a16 g f bes8.~ | % m. 56
  bes16 a g a8.~ | % m. 57
  a16 bes c \clef treble d8.~ | % m. 58
  d16 es f g8.~ | % m. 59
  g16 a bes c8 bes16 | % m. 60
  a8. [bes~] | % m. 61
  bes16 a g a8.~ | % m. 62
  a16 g f g8.~ | % m. 63
  g16 f e f8.~ | % m. 64
  f16 es d es8.~ | % m. 65
  es16 d c d8. | % m. 66
  c8. [f~] | % m. 67
  f16 d es r8. | % m. 68
  R4. | % m. 69
  r8. d16 es f | % m. 70
  g16 f as g as f | % m. 71
  e!16 f g \clef bass c,8.~ | % m. 72
  c16 as bes bes8.~ | % m. 73
  bes16 g as as8.~ | % m. 74
  as16 f g g8.~ | % m. 75
  g16 b! c~ c e f | % m. 76
  bes,8.~ bes16 e, g | % m. 77
  as4.~ | % m. 78
  as16 g f e g bes~ | % m. 79
  bes16 as g f as c~ | % m. 80
  c16 bes as g as f | % m. 81
  e8 r16 r8. | % m. 82
  r8. b! | % m. 83
  c8 d!16 e8 c16 | % m. 84
  <<
    { \stemDown f8 g16 as8. }
    \\
    { \stemUp s8. r16 c b! }
  >> | % m. 85
  c8 r16 <c es>8 r16 | % m. 86
  <des f>8 r16 r8. | % m. 87
  r16 r \clef treble bes' a8 g16 | % m. 88
  f16 r r r a es | % m. 89
  es16 d32 f g a bes16 d, f | % m. 90
  f16 e!8~ e16 e g | % m. 91
  g16 f8~ f32 c' bes a g f | % m. 92
  g32 r r16 r r8. | % m. 93
  r8. r16 r e | % m. 94
  f8 bes16 a bes c~ | % m. 95
  c8 f,16 bes8.~ | % m. 96
  bes8 g16 c8.~ | % m. 97
  c16 a bes f8 e16 | % m. 98
  f8. \bar "|." | % m. 99
    
}

bass = \relative c {
  \global
  
  R4. | % m. 1
  R4. | % m. 2
  R4. | % m. 3
  R4. | % m. 4
  R4. | % m. 5
  R4. | % m. 6
  R4. | % m. 7
  R4. | % m. 8
  R4. | % m. 9
  R4. | % m. 10
  R4. | % m. 11
  R4. | % m. 12
  R4. | % m. 13
  r8. f16 e f | % m. 14
  c'8 r16 a g a | % m. 15
  d8 r16 c d e | % m. 16
  f16 e d c d bes | % m. 17
  a16 g f bes8.~ | % m. 18
  bes8. [a] | % m. 19
  g16 f e d e c | % m. 20
  b!16 a g c b c | % m. 21
  f8 r16 e d e | % m. 22
  a8 r16 g a b! | % m. 23
  c16 b! a g a f | % m. 24
  e16 f d c d e | % m. 25
  f16 g e d e f | % m. 26
  g16 a f e f g | % m. 27
  a16 g f g8 g,16 | % m. 28
  c'16 b! a g a f | % m. 29
  e16 d c f8.~ | % m. 30
  f8. [e] | % m. 31
  d8. [bes] | % m. 32
  a4.~ | % m. 33
  a8.~ a16 bes! c | % m. 34
  d16 e fis g8 f!16 | % m. 35
  e8. [es] | % m. 36
  d4.~ | % m. 37
  d8.~ d16 e f | % m. 38
  g8. [e] | % m. 39
  f8.~ f16 g a | % m. 40
  bes8. [g] | % m. 41
  a8.~ a16 bes c | % m. 42
  d8. [b!] | % m. 43
  c8.~ c16 d e | % m. 44
  f16 e d c d bes | % m. 45
  a16 g f g a b! | % m. 46
  c16 b! a g as f! | % m. 47
  es16 d c d e! fis | % m. 48
  g16 f! es d es c | % m. 49
  bes16 a g a b! cis | % m. 50
  d16 c! bes a bes g | % m. 51
  f16 e d d'8 r16 | % m. 52
  e8 r16 es8 r16 | % m. 53
  d16 e! f e f g | % m. 54
  a,8 bes16 c8 cis16 | % m. 55
  d8.~ d16 c! bes | % m. 56
  f'8.~ f8 es16 | % m. 57
  d8 c16 bes8 a16 | % m. 58
  g8 f16 es8 d16 | % m. 59
  c8.~ c16 d es | % m. 60
  f4.~ | % m. 61
  f4.~ | % m. 62
  f4.~ | % m. 63
  f4.~ | % m. 64
  f8 r16 r8. | % m. 65
  r8. bes16 a bes | % m. 66
  f'8 r16 d c d | % m. 67
  g8 r16 f g a | % m. 68
  bes16 a g f g es | % m. 69
  d16 c bes bes' c d | % m. 70
  es16 d c bes c as | % m. 71
  g8 f16 e!8 c16 | % m. 72
  f8 r16 d8 r16 | % m. 73
  e8 r16 f8 r16 | % m. 74
  des8 r16 b!8 r16 | % m. 75
  c4.~ | % m. 76
  c4.~ | % m. 77
  c4.~ | % m. 78
  c4.~ | % m. 79
  c4.~ | % m. 80
  c4.~ | % m. 81
  c16 d e f g as | % m. 82
  d,,4. | % m. 83
  c8 r16 r8 c'16~ | % m. 84
  c8 r16 r8. | % m. 85
  <<
    { g'8 r16 f8 r16 | bes8 s16 s8. }
    \\
    { c,8 r16 c8 r16 | bes16 des f bes a g } 
  >> | % m. 86 - 87
  a8 bes16 c8 c,16 | % m. 88
  f8 r16 f, e f | % m. 89
  bes8 r16 g f g | % m. 90
  c8 r16 a g a | % m. 91
  d8 r16 bes a bes | % m. 92
  e8 r16 c d e | % m. 93
  f16 e d c d bes | % m. 94
  a16 bes g f g a | % m. 95
  bes16 c a g a bes | % m. 96
  c16 d bes a bes c | % m. 97
  d16 c bes c8 c,16 | % m. 98
  f8. \bar "|." | % m. 99
    
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
