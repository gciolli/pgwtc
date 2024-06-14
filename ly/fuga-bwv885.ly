\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

#(ly:expect-warning "Previous mark event here")
#(ly:expect-warning "Two simultaneous mark events, junking this one")

\header {
  title = "Fugue XVI"
  subtitle = "in 4 voices"
  opus = "BWV 885"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key g \minor
  \numericTimeSignature
  \time 3/4
}

soprano = \relative c'' {
  \global
  
  R2. | % m. 1
  R2. | % m. 2
  R2. | % m. 3
  R2. | % m. 4
  R2. | % m. 5
  R2. | % m. 6
  R2. | % m. 7
  R2. | % m. 8
  r4 d r8 bes | % m. 9
  es4 c r8 a | % m. 10
  d4 bes r8 g | % m. 11
  c8 c c c c c | % m. 12
  c8 bes16 a bes g bes c d cis d8 | % m. 13
  d8 g, r16 g a bes c bes c8 | % m. 14
  c8 f, r16 f g a bes a bes8~ | % m. 15
  bes16 a bes a g f g f e! d e d | % m. 16
  cis8 a' d, c'! b!16 d f8 | % m. 17
  es4 r r8 r16 a | % m. 18
  bes8. a16 g fis g fis g4~ | % m. 19
  g4 fis8. a16 d,8. c16 | % m. 20
  bes2 a4~ | % m. 21
  a16 a bes a g8. fis16 g4~ | % m. 22
  g8. bes16 a8. c16 fis,8. a16 | % m. 23
  d,8. fis16 g8 r r r16 bes~ | % m. 24
  bes16 bes a g a a b! c d c d8 | % m. 25
  d8 c16 b! c4 r8 r16 c~ | % m. 26
  c16 c bes! a bes d e! f! g f g8~ | % m. 27
  g8 f16 e! f d f g a g a8 | % m. 28
  a8 d, r16 d e! f g f g8 | % m. 29
  g8 c, r16 c d e! f e f8~ | % m. 30
  f8. f16 e! d e d cis d e8~ | % m. 31
  e8 d f4 r8 d | % m. 32
  g4 es r8 c | % m. 33
  f4 d r8 bes | % m. 34
  es8 es es es es es | % m. 35
  es8 d16 es f bes, f' g a g a8 | % m. 36
  a8 d, r16 d e! f g f g8 | % m. 37
  g8 c, r16 c d e! f e f8~ | % m. 38
  f16 d e! f g a bes g f e d c | % m. 39
  f8 c f es!16 d es8 c | % m. 40
  d8 g, g' f16 es f8 d | % m. 41
  es8 c as' g16 f g8 es | % m. 42
  f2~ f16 es d c | % m. 43
  b!8 d~ d16 g, c8~ c16 c b8 | % m. 44
  c4 r16 bes! a! g a4~ | % m. 45
  a16 g a bes g2~ | % m. 46
  g16 f g a f2~ | % m. 47
  f8. a16 g8. f16 bes8. a16 | % m. 48
  g4 f8 r r4 | % m. 49
  R2. | % m. 50
  r4 es' r8 c | % m. 51
  f4 d r8 bes | % m. 52
  es4 c r8 as | % m. 53
  d8 d d d d d | % m. 54
  d16 c d c b! a! b a g4~ | % m. 55
  g4~ g16 g a b! c4~ | % m. 56
  c8 b!16 a! b4 bes~ | % m. 57
  bes8 a!16 g a4 as~ | % m. 58
  as8 g16 f g f' g as bes as bes8 | % m. 59
  bes8 es, r16 es16 f g as g as8 | % m. 60
  as8 d, r16 d es f g f g8~ | % m. 61
  g8 bes as g f es | % m. 62
  d8 d es f g4~ | % m. 63
  g16 f g f es d es d c bes c bes | % m. 64
  a!4. bes8 c4~ | % m. 65
  c4 bes~ bes16 bes a g | % m. 66
  fis8 r r4 r | % m. 67
  R2. | % m. 68
  r4 d' r8 bes | % m. 69
  es4 c r8 a | % m. 70
  d4 bes r8 g | % m. 71
  c8 c c c c c | % m. 72
  c4 r8 d bes4 | % m. 73
  r8 c bes a a g | % m. 74
  g4 r16 a bes c d c d es | % m. 75
  a,4 r16 bes c! d es d es f | % m. 76
  b,!16 g a! b c b c8 c c | % m. 77
  c4 r8 fis g4 | % m. 78
  r16 bes a g fis e! d c! bes a g fis | % m. 79
  g2.~ | % m. 80
  g2.~ | % m. 81
  g8 r16 d'8 c bes a! g16 | % m. 82
  fis8 c'16 d32 es d8 g,4 fis8 | % m. 83
  g4 r r \bar "|." \override Staff.RehearsalMark #'break-visibility = #begin-of-line-invisible \mark\markup{\musicglyph #"scripts.ufermata"} | % m. 84
   
}

alto = \relative c'' {
  \global
  
  R2. | % m. 1
  R2. | % m. 2
  R2. | % m. 3
  R2. | % m. 4
  r4 g r8 f | % m. 5
  bes4 g r8 e! | % m. 6
  a4 f r8 d | % m. 7
  g8 g g g g g | % m. 8
  g8 f16 e! f d e fis g fis g8 | % m. 9
  g8 c, r16 c d es f es f8 | % m. 10
  f8 bes, r16 bes c d es d es8~ | % m. 11
  es16 d es d c bes c bes a g a g | % m. 12
  fis8 d'4 g8 r r16 a~ | % m. 13
  a16 a g f e!4 r8 r16 g~ | % m. 14
  g16 g f e! d4 r8 r16 f | % m. 15
  e!8. f16 e8. d16 cis8. d16 | % m. 16
  a4 r r8 r16 d'~ | % m. 17
  d8 g, c, bes'! a16 c es8~ | % m. 18
  es8 d r4 r8 r16 d | % m. 19
  c16 bes c bes c8. c16 bes a bes a | % m. 20
  g16 f g f es8. \prall d16 c4 | % m. 21
  f4 d r | % m. 22
  R2. | % m. 23
  R2. | % m. 24
  R2. | % m. 25
  R2. | % m. 26
  R2. | % m. 27
  r4 a' r8 f | % m. 28
  es4 g r8 e! | % m. 29
  a4 f r8 d | % m. 30
  g8 g g g g g | % m. 31
  g8 f r16 a bes c d c d8 | % m. 32
  d8 g, r16 g a bes c bes c8 | % m. 33
  c8 f, r16 f g a bes a bes8~ | % m. 34
  bes8. bes16 a g a g f4~ | % m. 35
  f4 r r8 r16 c' | % m. 36
  bes16 c bes a g4 r8 r16 bes | % m. 37
  a16 bes a g f4 r8 r16 a | % m. 38
  g2. | % m. 39
  f4 r r16 g as g | % m. 40
  f8 d r4 r16 a'! b! a | % m. 41
  g8 es r4 r16 b'! c bes | % m. 42
  as16 g as8~ as16 g as8~ as4~ | % m. 43
  as16 as g f es8. es16 d8. f16~ | % m. 44
  f16 f es d es4 r8 c | % m. 45
  f4 d r8 bes | % m. 46
  es4 c r8 a | % m. 47
  d8 d d d d d | % m. 48
  d8 c r4 r8 r16 f | % m. 49
  es8 d r4 r8 r16 g | % m. 50
  f8 es16 f g4 r8 es | % m. 51
  as4 f r8 d | % m. 52
  g4 es r8 c | % m. 53
  f8 f f f f f | % m. 54
  f8 f f f r16 f g f~ | % m. 55
  f16 es f d es4~ es8 as | % m. 56
  f4 r16 as g f e! f e f | % m. 57
  e!4 r16 g f es d es d es | % m. 58
  d4 r16 d' es f g f g8 | % m. 59
  g8 c, r16 c d es f es f8 | % m. 60
  f8 bes, r16 bes c d es d es8~ | % m. 61
  es16 des es des c bes c bes as g as g | % m. 62
  f8 f g as bes b! | % m. 63
  c16 d es d c bes c bes as g as g | % m. 64
  fis4. g8 a4~ | % m. 65
  a16 d, es d~ d d es d c8 cis | % m. 66
  d8 r r16 a' bes c d c d8 | % m. 67
  d8 g, r16 g a bes c bes c8 | % m. 68
  c8 fis, r16 fis g a bes a bes8 | % m. 69
  bes8 es, r16 e! f g a g a8 | % m. 70
  a8 d, r16 d e! fis g fis g8~ | % m. 71
  g8 bes a g fis g | % m. 72
  a4 r8 fis g4 | % m. 73
  r8 a g4 fis | % m. 74
  d4 r r | % m. 75
  r16 e! f g a g a8~ a as | % m. 76
  g8 r r r16 bes a g a g | % m. 77
  fis4 r8 a bes4 | % m. 78
  R2. | % m. 79
  r16 b,! c d es d es8 es4 | % m. 80
  r16 c d es f es f8 f4~ | % m. 81
  f8 as g f es4 | % m. 82
  d8 r r es d4 | % m. 83
  d4 r r \bar "|." | % m. 84
   
}

tenor = \relative c' {
  \global
  
  r4 d r8 bes | % m. 1
  es4 c r8 a | % m. 2
  d4 bes r8 g | % m. 3
  c8 c c c c c | % m. 4
  c8 bes16 a bes g bes c d cis d8 | % m. 5
  d8 g, r16 g a bes c bes c8 | % m. 6
  c8 f, r16 f g a bes a bes8~ | % m. 7
  bes16 a bes a g f g f e! d e d | % m. 8
  cis8 a' d, c'! bes g | % m. 9
  c16 bes c bes a8. g16 a8 f | % m. 10
  bes16 a bes a g8 a16 bes c8. bes16 | % m. 11
  a4 r8 g fis e! | % m. 12
  d8 fis g d'16 e! f! e f8 | % m. 13
  d4~ d8 c16 bes a8 g | % m. 14
  c4~ c8 bes16 a g8 f | % m. 15
  bes4 r r | % m. 16
  R2. | % m. 17
  R2. | % m. 18
  R2. | % m. 19
  r4 d r8 bes | % m. 20
  es4 c r8 a | % m. 21
  d4 bes r8 g | % m. 22
  c8 c c c c c~ | % m. 23
  c8 bes16 a bes g a bes c bes c8 | % m. 24
  c8 f16 e! f8 r r r16 \clef treble f~ | % m. 25
  f16 f es! d es c fis g a g16 a8 | % m. 26
  a8 g16 fis g8 r r r16 \clef bass cis,! | % m. 27
  d16 cis d a d,8 d'16 e! f8 d | % m. 28
  g16 g, a bes c g c d e!8 c | % m. 29
  f16 f, g a bes f bes c d c bes a | % m. 30
  bes16 d cis d g, bes a bes e,! e'! cis a | % m. 31
  d16 d, f a d c! d c bes d g, bes | % m. 32
  es,16 g a bes c bes c bes a c f, a | % m. 33
  d,16 f g a bes a bes a g bes es, g | % m. 34
  c,16 g' a bes c d c bes a f g a | % m. 35
  bes16 a bes c d c d e! f e f8~ | % m. 36
  f4 d e!~ | % m. 37
  e4 c d~ | % m. 38
  d2 c4~ | % m. 39
  c4 r r | % m. 40
  R2. | % m. 41
  R2. | % m. 42
  R2. | % m. 43
  R2. | % m. 44
  r4 c r8 a | % m. 45
  d4 bes r8 g | % m. 46
  c4 a r8 f | % m. 47
  bes8 bes bes bes bes bes | % m. 48
  bes8 a16 g a g a b! c b c8~ | % m. 49
  c8 b!16 a g d g a b c d8~ | % m. 50
  d8 c r16 g a b! c b c8 | % m. 51
  c8 f, r16 f g as bes as bes8 | % m. 52
  bes8 es, r16 es f g as g as8~ | % m. 53
  as16 g as g f es f es d c d c | % m. 54
  b!8 b'! b b c d~ | % m. 55
  d8 g, c4~ c8 f | % m. 56
  d2 des4 | % m. 57
  c2 ces4 | % m. 58
  bes4 d r8 bes | % m. 59
  es4 c r8 as! | % m. 60
  d4 bes r8 g | % m. 61
  c8 c c c c c | % m. 62
  d16 c d c bes as! bes as g f g f | % m. 63
  es8 es' es es es es | % m. 64
  es16 d es d c bes c bes a g a g | % m. 65
  fis8 a bes fis g4 | % m. 66
  a8 r d4 r8 bes | % m. 67
  es4 c r8 a | % m. 68
  d4 bes r8 g | % m. 69
  c4 a r8 f | % m. 70
  bes4 g r8 bes | % m. 71
  a8 g es' es es es | % m. 72
  d4 r8 d d4 | % m. 73
  r8 es d4 c | % m. 74
  bes4 r r16 a bes c | % m. 75
  d16 cis d8~ d a16 b! c! b c8 | % m. 76
  d8 r r r16 d c bes! c bes | % m. 77
  a4 r8 c d4 | % m. 78
  r4 r16 a bes c! d4~ | % m. 79
  d16 g, a b! c b c8 c4 | % m. 80
  r16 a b! c d c d8 d4~ | % m. 81
  d8 f es d c bes | % m. 82
  a8 r r bes a16 bes c8~ | % m. 83
  c8 b! r4 r \bar "|." | % m. 84
    
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
  r4 g r8 f! | % m. 13
  bes4 g r8 e! | % m. 14
  a4 f r8 d | % m. 15
  g8 g g g g g | % m. 16
  g8 f16 e! f d es f g f g8 | % m. 17
  c,16 b! c d es c d es f es f8 | % m. 18
  bes,16 f' g a bes a bes8 r16 e,! fis g | % m. 19
  a16 g a8 r16 d, e! fis g fis g8 | % m. 20
  g8 c, r16 c d es f es f8 | % m. 21
  f8 bes, r16 bes c d es d es8~ | % m. 22
  es16 d es d c bes c bes a g a g | % m. 23
  fis8 d' g, f'! e! c | % m. 24
  f8 a d, c' b! g | % m. 25
  c8 g c, a' fis d | % m. 26
  g8 bes e,! d' cis a | % m. 27
  d16 r r8 r4 r | % m. 28
  R2. | % m. 29
  R2. | % m. 30
  R2. | % m. 31
  R2. | % m. 32
  R2. | % m. 33
  R2. | % m. 34
  R2. | % m. 35
  r4 bes r8 a | % m. 36
  d4 bes r8 g | % m. 37
  c4 a r8 f | % m. 38
  bes8 bes bes bes bes bes | % m. 39
  bes8 a16 g a f a b! c b c8~ | % m. 40
  c16 c b! a b g b c d c d8~ | % m. 41
  d16 g, c b! c g c d es d es8~ | % m. 42
  es8. es16 d8. c16 b!8. d16 | % m. 43
  g,8 b! c as f g | % m. 44
  c,4 r16 c d es f es f8 | % m. 45
  f8 bes, r16 bes c d es d es8 | % m. 46
  es8 a, r16 a bes c d c d8~ | % m. 47
  d16 c d c bes a bes a g f g f | % m. 48
  e!16 c d c f c f g as8 d, | % m. 49
  g16 fis g a! b! a b d g8 b, | % m. 50
  c16 c' g es c4 r8 r16 as'~ | % m. 51
  as16 d,8 as'16 bes4 r8 r16 g~ | % m. 52
  g16 c,8 g'16 as4 r8 r16 f | % m. 53
  b,!4 r8 d f, as | % m. 54
  g8 g' g g a! b! | % m. 55
  es,4~ es16 es f g as g as8 | % m. 56
  as8 d, r16 d es f g f g8 | % m. 57
  g8 c, r16 c d es f es f8 | % m. 58
  f8 bes, bes'4 r8 g | % m. 59
  c4 as r8 f | % m. 60
  bes4 g r8 es | % m. 61
  as8 as as as as as | % m. 62
  bes8 as g f es d | % m. 63
  c8 c' c c c c | % m. 64
  c8 bes a g fis e! | % m. 65
  d8 fis! g d es4 | % m. 66
  d8 d, r4 r | % m. 67
  R2. | % m. 68
  r4 r16 d' e! fis g fis g8 | % m. 69
  g8 c, r16 c d e! f e f8 | % m. 70
  f8 bes, r16 bes c d es! d es8~ | % m. 71
  es16 d es d c bes c bes a g a g | % m. 72
  fis4 r8 d g4 | % m. 73
  r8 c d4 d, | % m. 74
  g16 d' e! fis g fis g8 g4~ | % m. 75
  g8 f f2~ | % m. 76
  f16 b,! c d es d es8 es4~ | % m. 77
  es4 r8 es d4 | % m. 78
  r8 cis d4 r8 d | % m. 79
  es4 c r16 a b! c | % m. 80
  d4 b! r16 g a b | % m. 81
  c8 c c c c c | % m. 82
  c8 bes16 a bes8 c d d, | % m. 83
  g4 r r \bar "|." \override Staff.RehearsalMark #'direction = #DOWN \mark\markup{\musicglyph #"scripts.dfermata"} | % m. 84 
  
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
