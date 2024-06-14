\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue XI"
  subtitle = "in 3 voices"
  opus = "BWV 856"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key f \major
  \time 3/8
}

soprano = \relative c'' {
  \global
  
  \partial 8 r8
  R4. | % m. 1
  R4. | % m. 2
  R4. | % m. 3
  r8 r f, | % m. 4
  a8 g f | % m. 5
  g8 b,!16 c d e | % m. 6
  f8 g16 f e d | % m. 7
  e16 d c d e f | % m. 8
  g8 f e | % m. 9
  f16 e d e f g | % m. 10
  a16 c bes a g f | % m. 11
  e4. \prall | % m. 12
  f8 a4 | % m. 13
  g8 c4 | % m. 14
  b!8 e4 | % m. 15
  d4.~ | % m. 16
  d16 d c8 c | % m. 17
  d8 c bes | % m. 18
  c8 e,16 f g a | % m. 19
  bes8 c16 bes a g | % m. 20
  a16 g f g a bes | % m. 21
  c16 bes a bes c d | % m. 22
  e16 g f e d c | % m. 23
  b!4. \prall | % m. 24
  c16 d e f g a | % m. 25
  bes,16 c d e f g | % m. 26
  a16 c bes a g f | % m. 27
  f8 e4 \prall | % m. 28
  f8 r r | % m. 29
  g4.~ | % m. 30
  g16 g f e d c | % m. 31
  f4.~ | % m. 32
  f16 f e d cis b! | % m. 33
  e4.~ | % m. 34
  e8 a, d | % m. 35
  cis4 a'8 | % m. 36
  bes8 a g | % m. 37
  a8 cis,16 d e f | % m. 38
  g8 a16 g f e | % m. 39
  f16 a g f e d | % m. 40
  cis4.~ | % m. 41
  cis16 d32 c! bes16 a g f | % m. 42
  bes8 a g | % m. 43
  a4.~ | % m. 44
  a16 bes e,8. \prall d16 | % m. 45
  d16 e f g a bes | % m. 46
  c16 bes c d es d | % m. 47
  fis,16 \prall e! d e fis g | % m. 48
  a4.~ | % m. 49
  a16 g a bes c d | % m. 50
  es8 d c | % m. 51
  d8 fis,16 g a bes | % m. 52
  c16 bes d c bes a | % m. 53
  bes16 fis g a bes cis | % m. 54
  d16 g bes,8 \prall a16 g | % m. 55
  g8 bes4~ | % m. 56
  bes16 g c bes a g | % m. 57
  a4.~ | % m. 58
  a16 f bes a g f | % m. 59
  e8 f g | % m. 60
  a8 bes c | % m. 61
  d8 e f | % m. 62
  g4.~ | % m. 63
  g16 c, f es d c | % m. 64
  d16 c es d c bes | % m. 65
  c8 e,!16 f g a | % m. 66
  bes16 a c bes a g | % m. 67
  a16 g bes a c bes | % m. 68
  d16 c e d f e | % m. 69
  g16 a, bes g a8~ | % m. 70
  a16 bes g8. f16 | % m. 71
  \set Score.measureLength = #(ly:make-moment 2 8) f4 \fermata \bar "|." | % m. 72
  
}

mezzo = \relative c' {
  \global
  
  \partial 8 c8
  d8 c bes | % m. 1
  c8 e,16 f g a | % m. 2
  bes8 c16 bes a g | % m. 3
  a16 g f g a bes | % m. 4
  c16 bes a bes c d | % m. 5
  e16 g f e d c | % m. 6
  b!4. \prall | % m. 7
  c8 bes! a | % m. 8
  bes16 a g a bes c | % m. 9
  a4 \clef treble d8 | % m. 10
  e8 g c, | % m. 11
  c4.~ | % m. 12
  c4 f8 | % m. 13
  e4 a8 | % m. 14
  g4 c8~ | % m. 15
  c16 c b! a g f | % m. 16
  e8. bes'!16 a g | % m. 17
  f16 e d e f g | % m. 18
  a8 r r | % m. 19
  R4. | % m. 20
  r8 r f | % m. 21
  a8 g f | % m. 22
  g8 b,!16 c d e | % m. 23
  f8 g16 f e d | % m. 24
  e16 d c d e c | % m. 25
  f4 r8 | % m. 26
  r8 r c' | % m. 27
  d8 c bes | % m. 28
  c8 e,16 f g a | % m. 29
  bes8 c16 bes a g | % m. 30
  a4.~ | % m. 31
  a8 bes16 a g f | % m. 32
  g4.~ | % m. 33
  g8 a16 g f e | % m. 34
  f16 e f g a g | % m. 35
  a16 bes a g f e | % m. 36
  d16 cis d f e d | % m. 37
  cis8 r a' | % m. 38
  bes8 a g | % m. 39
  a8 cis,16 d e f | % m. 40
  g8 a16 g f e | % m. 41
  f4 r8 | % m. 42
  cis4.~ | % m. 43
  cis16 a b! cis d e | % m. 44
  f16 e cis4 | % m. 45
  d4 r8 | % m. 46
  R4. | % m. 47
  r8 r d | % m. 48
  es8 d c | % m. 49
  d8 \clef bass fis,16 g a bes | % m. 50
  c16 bes d c bes a | % m. 51
  bes16 d c bes a g | % m. 52
  fis8 r r | % m. 53
  g16 a bes c d a | % m. 54
  bes8 g fis | % m. 55
  g16 \clef treble d' g f e d | % m. 56
  e4. | % m. 57
  r16 c f es d c | % m. 58
  d4. | % m. 59
  c8 d e | % m. 60
  f8 g a | % m. 61
  bes4.~ | % m. 62
  bes16 g c bes a g | % m. 63
  a4.~ | % m. 64
  a8 \clef bass d,, g~ | % m. 65
  g16 f bes a g f | % m. 66
  g8 bes,16 c d e | % m. 67
  f4.~ | % m. 68
  f4 bes8~ | % m. 69
  bes8 \clef treble e f~ | % m. 70
  f8 e4 | % m. 71
  \set Score.measureLength = #(ly:make-moment 2 8) f4 \fermata \bar "|." | % m. 72
  
}

bass = \relative c {
  \global
  
  \partial 8 r8
  R4. | % m. 1
  R4. | % m. 2
  R4. | % m. 3
  R4. | % m. 4
  R4. | % m. 5
  R4. | % m. 6
  R4. | % m. 7
  R4. | % m. 8
  r8 r c | % m. 9
  d8 c bes | % m. 10
  c8 e,16 f g a | % m. 11
  bes8 c16 bes a g | % m. 12
  a16 g f g a bes | % m. 13
  c16 b! a b c d | % m. 14
  e16 g f e d c | % m. 15
  b!4. \prall | % m. 16
  c8 d16 e f8 | % m. 17
  bes4. | % m. 18
  a16 c bes a g f | % m. 19
  e4. \trill | % m. 20
  f4 r8 | % m. 21
  R4. | % m. 22
  R4. | % m. 23
  R4. | % m. 24
  r8 r c' | % m. 25
  d8 c bes | % m. 26
  c8 e,16 f g a | % m. 27
  bes8 c16 bes a g | % m. 28
  a16 c bes a g f | % m. 29
  e4. \trill | % m. 30
  f8 g a | % m. 31
  d,4 r8 | % m. 32
  e8 f g | % m. 33
  cis,4 cis'8 | % m. 34
  d8 c! bes | % m. 35
  a4.~ | % m. 36
  a4.~ | % m. 37
  a4.~ | % m. 38
  a4.~ | % m. 39
  a4 a8 | % m. 40
  bes8 a g | % m. 41
  a8 cis,16 d e f | % m. 42
  g8 a16 g f e | % m. 43
  f8. g16 f e | % m. 44
  d16 g a8 a, | % m. 45
  d,4 d'8 | % m. 46
  es8 d c | % m. 47
  d fis,16 g a bes | % m. 48
  c16 bes d c bes a | % m. 49
  bes8 r r | % m. 50
  fis'8 r r | % m. 51
  g8 r r | % m. 52
  es8 r r | % m. 53
  d8 r r | % m. 54
  r16 c \noBeam d8 d | % m. 55
  g,8 a bes | % m. 56
  c8 d e | % m. 57
  f8 g a | % m. 58
  bes4.~ | % m. 59
  bes16 g c bes a g | % m. 60
  f16 c f es d c | % m. 61
  bes16 c bes a g f | % m. 62
  e4 r8 | % m. 63
  f8 g a | % m. 64
  bes4 r8 | % m. 65
  a4 r8 | % m. 66
  g4 r8 | % m. 67
  f8 g a | % m. 68
  bes8 c d | % m. 69
  e8 c f | % m. 70
  bes,8 c c, | % m. 71
  \set Score.measureLength = #(ly:make-moment 2 8) f4 \fermata \bar "|." | % m. 72
   
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
      { \clef bass \mezzo }
    
    \new Staff = "bass"
      { \clef bass \bass }
      
  >>
  
\layout { 
  indent = 0.0
  }

}
