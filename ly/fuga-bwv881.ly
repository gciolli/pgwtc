\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue XII"
  subtitle = "in 3 voices"
  opus = "BWV 881"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key f \minor
  \numericTimeSignature
  \time 2/4
  \partial 8
}

soprano = \relative c'' {
  \global
  
  c8
  f,8 \mordent f f des'16 bes | % m. 1
  e,!8 e e16 f g as | % m. 2
  bes16 as g as bes des c bes | % m. 3
  as16 g f g as bes c d! | % m. 4
  es16 d! c d es g f es | % m. 5
  d!16 c b! a! g a b c | % m. 6
  d!8 f as b,! | % m. 7
  c8 es g c | % m. 8
  des,8 c' bes des, | % m. 9
  c8 es as c, | % m. 10
  bes8 as' g bes, | % m. 11
  as16 c f es des4~ | % m. 12
  des16 c bes as g4~ | % m. 13
  g8 des' c4~ | % m. 14
  c8 des c4~ | % m. 15
  c8 des c4~ | % m. 16
  c8 c c as' | % m. 17
  bes,16 c des c bes c des c | % m. 18
  bes8 bes bes g' | % m. 19
  as,16 bes c bes as bes c bes | % m. 20
  as8 as as des~ | % m. 21
  des16 f, g as bes c des bes | % m. 22
  c16 des bes c des c bes as | % m. 23
  g16 as bes g es8 es' | % m. 24
  as,8 \mordent as as f'16 des | % m. 25
  g,8 g g16 as bes c | % m. 26
  des16 c bes c des f es des | % m. 27
  c16 bes as g as bes c d! | % m. 28
  es4. es8 | % m. 29
  as16 g as bes as g f es | % m. 30
  d!2~ | % m. 31
  d16 c b! c d! f es d | % m. 32
  es8 es es4~ | % m. 33
  es2 | % m. 34
  d!8 d d4~ | % m. 35
  d2 | % m. 36
  c8 c c f | % m. 37
  b,!16 c d!8~ d4 | % m. 38
  r16 g, c8~ c16 c8 b!16 | % m. 39
  c4 r8 c16 bes! | % m. 40
  as4 r16 as bes des~ | % m. 41
  des16 des c bes g'4~ | % m. 42
  g4. e!8 | % m. 43
  f8 r r4 | % m. 44
  r4 r8 a! | % m. 45
  bes es, des bes'~ | % m. 46
  bes8 c16 bes as g f e! | % m. 47
  f2~ | % m. 48
  f8 bes16 as g f e! d! | % m. 49
  e!16 f e d! c bes as g | % m. 50
  as16 f as c f c bes as | % m. 51
  bes16 g bes e! g f e d! | % m. 52
  c16 d! e! g c bes as g | % m. 53
  f16 ges f es des c bes a! | % m. 54
  bes8 f des'4~ | % m. 55
  des16 des8.~ des16 des8.~ | % m. 56
  des16 c8.~ c16 c8.~ | % m. 57
  c16 bes a! g a bes c8~ | % m. 58
  c8 bes16 a! bes8 des | % m. 59
  g,8 c16 d! e!8 f | % m. 60
  g8 f e! f | % m. 61
  g4~ g16 f e! f | % m. 62
  g4~ g16 f e! f | % m. 63
  bes16 f e! d! e f g8~ | % m. 64
  g8 f16 e! f8 g | % m. 65
  as8 as as4~ | % m. 66
  as2 | % m. 67
  g8 g g4~ | % m. 68
  g2 | % m. 69
  f4 r8 f | % m. 70
  es4 r8 f | % m. 71
  bes,8 bes bes ges'16 es | % m. 72
  a,!8 a a16 bes c des | % m. 73
  es16 des c des es ges f es | % m. 74
  des8 [des des] r | % m. 75
  des8 [des des] r | % m. 76
  e,!8 r r e'! | % m. 77
  f8 c c as' | % m. 78
  bes,16 c des c bes c des c | % m. 79
  bes8 bes bes g' | % m. 80
  as,16 bes c bes as bes c bes | % m. 81
  as8 as as f' | % m. 82
  g,16 as bes as g des' c bes | % m. 83
  as8. g16 g8. f16 | % m. 84
 \set Score.measureLength = #(ly:make-moment 3 8 )
 f4. \fermata \bar "|." | % m. 85
    
}

mezzo = \relative c' {
  \global
  
  r8 
  R2 | % m. 1
  R2 | % m. 2
  R2 | % m. 3
  r4 r8 f | % m. 4
  c8 c c as'16 f | % m. 5
  b,!8 b b16 c d! es | % m. 6
  f16 es d! es f as g f | % m. 7
  es16 d! c d es f g a! | % m. 8
  bes16 as! g f g bes as g | % m. 9
  as16 bes c bes as g f es | % m. 10
  d!16 f e! d e g f e | % m. 11
  f8 r r16 c' bes as | % m. 12
  g4 r16 f e! d! | % m. 13
  e!4. e8 | % m. 14
  f4~ f16 as g f | % m. 15
  e!16 g bes8~ bes16 g f e | % m. 16
  as8 as as4~ | % m. 17
  as2 | % m. 18
  g8 g g4~ | % m. 19
  g2 | % m. 20
  f8 f f4 | % m. 21
  es8 r r4 | % m. 22
  R2 | % m. 23
  R2 | % m. 24
  R2 | % m. 25
  R2 | % m. 26
  R2 | % m. 27
  r4 r8 as | % m. 28
  es8 es es c'16 as | % m. 29
  d,!8 d d16 es f g | % m. 30
  as16 g f g as c bes as | % m. 31
  g2~ | % m. 32
  g8 g g es' | % m. 33
  f,16 g as g f g as g | % m. 34
  f8 f f d'! | % m. 35
  es,16 f g f es f g f | % m. 36
  es8 es es as~ | % m. 37
  as8. as16 g f es d! | % m. 38
  es8. es16 d!8 f~ | % m. 39
  f16 f e! d! e g8.~ | % m. 40
  g16 g f e! f4 | % m. 41
  g4 r8 e'! | % m. 42
  des8 bes g4 | % m. 43
  r16 bes as g f g as bes | % m. 44
  c8 es ges4~ | % m. 45
  ges8 a,! bes des | % m. 46
  c8 r r4 | % m. 47
  r8 as [des c] | % m. 48
  bes8 r r4 | % m. 49
  r8 \clef bass g,16 as bes g c8 | % m. 50
  f,8 f f des'16 bes | % m. 51
  e,!8 e e16 f g as | % m. 52
  bes16 as g as bes des c bes | % m. 53
  as8 a! bes c | % m. 54
  des8 r r \clef treble bes'16 as | % m. 55
  ges8 ges16 f ges8 ges16 f | % m. 56
  ges8 es16 des es8 es16 des | % m. 57
  es2~ | % m. 58
  es16 c des f es des c bes | % m. 59
  e!8 f g as | % m. 60
  bes8 bes16 as bes4~ | % m. 61
  bes16 e! des c bes4~ | % m. 62
  bes16 e! des c bes4 | % m. 63
  r4 bes~ | % m. 64
  bes16 g as des c bes as g | % m. 65
  f8 c' c as' | % m. 66
  bes,16 c des c bes c des c | % m. 67
  bes8 bes bes g' | % m. 68
  as,16 bes c bes as bes c bes | % m. 69
  as8 as des4~ | % m. 70
  des8 c16 bes a!8 r | % m. 71
  r16 f, g a! bes8 r | % m. 72
  r16 c es des c bes a! g! | % m. 73
  f16 r r8 r c'' | % m. 74
  f,8 f f des'16 bes | % m. 75
  e,!8 e e16 f g as | % m. 76
  bes16 as g as bes des c bes | % m. 77
  as8 as as4~ | % m. 78
  as2 | % m. 79
  g8 g g4~ | % m. 80
  g2 | % m. 81
  f8 f f4~ | % m. 82
  f8 e! r e | % m. 83
  f4 e! | % m. 84
  f4. \fermata \bar "|." | % m. 85 
    
}

bass = \relative c' {
  \global
  
  r8
  R2 | % m. 1
  R2 | % m. 2
  R2 | % m. 3
  R2 | % m. 4
  R2 | % m. 5
  R2 | % m. 6
  R2 | % m. 7
  R2 | % m. 8
  R2 | % m. 9
  R2 | % m. 10
  r4 r8 c | % m. 11
  f,8 f f des'16 bes | % m. 12
  e,!8 e e16 f g as | % m. 13
  bes16 as g as bes des c bes | % m. 14
  as16 g f g as c bes as | % m. 15
  g16 f e! f g bes as g | % m. 16
  f16 as c f c as f as | % m. 17
  des,8 f des bes | % m. 18
  es16 g bes es bes g es g | % m. 19
  c,8 es c as | % m. 20
  des16 es f es des c bes as | % m. 21
  g8 bes g es | % m. 22
  as8 g f d'! | % m. 23
  es8 es, r16 des''c bes | % m. 24
  c16 es c as f es' des c | % m. 25
  bes16 des bes g es c' bes as | % m. 26
  g16 as g f es des c bes | % m. 27
  as8 as' g f | % m. 28
  c'16 bes c des c bes as g | % m. 29
  f4 bes~ | % m. 30
  bes8 as16 g f es d! c | % m. 31
  b!8 d b g | % m. 32
  c16 es g c g es c es | % m. 33
  as,8 c as f | % m. 34
  bes16 d! f bes f d bes d | % m. 35
  g,8 bes g es | % m. 36
  as16 bes c bes as g f es | % m. 37
  d!8 c' b! g | % m. 38
  c8 as f g | % m. 39
  c,4 r8 c'' | % m. 40
  f,8 f f des'16 bes | % m. 41
  e,!8 e e16 f g as | % m. 42
  bes16 as g as bes des c bes | % m. 43
  as16 g f g as bes c des | % m. 44
  \clef treble es16 des c des es ges f es | % m. 45
  des16 c bes c des es f g! | % m. 46
  as16 bes as g f es des c | % m. 47
  \clef bass des16 es des c bes as g f | % m. 48
  g16 as g f e! d! c b! | % m. 49
  c8 [c c] r | % m. 50
  c8 [c c] r | % m. 51
  c8 [c c] r | % m. 52
  R2 | % m. 53
  des2~ | % m. 54
  des16 es des c bes as ges f | % m. 55
  es8 es es es'16 c | % m. 56
  a!8 a a ges'16 es | % m. 57
  c8 c c bes'16 a! | % m. 58
  bes2~ | % m. 59
  bes16 g as c bes as g f | % m. 60
  e!16 f g as g f e d! | % m. 61
  c8 c c bes'16 g | % m. 62
  e!8 e e des'16 bes | % m. 63
  g8 g g f'16 e! | % m. 64
  f2~ | % m. 65
  f16 e! f g f es des c | % m. 66
  des8 f des bes | % m. 67
  es16 des es f es des c bes | % m. 68
  c8 es c as | % m. 69
  des16 es des c bes as g f | % m. 70
  g16 bes a! g f es des c | % m. 71
  des4 r16 des es f | % m. 72
  ges8 r r4 | % m. 73
  r16 bes a! g f es des c | % m. 74
  bes16 c des c bes as g f | % m. 75
  g16 as bes as g f e! d! | % m. 76
  c8 e! g c~ | % m. 77
  c16 f, as c f as g f | % m. 78
  des'8 bes g f | % m. 79
  c,16 e! g c e! g f e | % m. 80
  c'8 as f e! | % m. 81
  c,16 f as c f as g f | % m. 82
  bes8 g e! c | % m. 83
  des8 bes c c, | % m. 84
  f4. \fermata \bar "|." | % m. 85
    
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
