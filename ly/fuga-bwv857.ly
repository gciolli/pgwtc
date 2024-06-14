\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
 title = "Fugue XII"
 subtitle = "in 4 voices"
 opus = "BWV 857"
 copyright = "No rights reserved."
 tagline = ""
}

global = {
  \key f \minor
  \time 4/4
}

soprano = \relative c'' {
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
  R1 | % m. 11
  R1 | % m. 12
  r4 c des c | % m. 13
  b!4 e f bes, | % m. 14
  a!4 as g2~ | % m. 15
  g8 f16 g as2.~ | % m. 16
  as8 g16 as bes2.~ | % m. 17
  bes16 g as c f2.~ | % m. 18
  f8 es d! g~ g16 b,! c d es4 | % m. 19
  r16 c d! es f!4 r16 es f g as!4 | % m. 20
  r16 g a! b! c4~ c8 b16 a b4 | % m. 21
  c8 g c4~ c8 c16 bes a!8 a16 g | % m. 22
  f8 g16 a! bes4 r8 bes16 as g8 g16 f | % m. 23
  es8 f16 g as4 r8 as16 g f8 f16 es | % m. 24
  des4. es16 des c8 c16 bes as4~ | % m. 25
  as8 bes16 c des4~ des16 c des bes g'4~ | % m. 26
  g16 c, des c f4~ f8 e16 f g4~ | % m. 27
  g8 f4 es8 d! es16 f g f e! d | % m. 28
  c4 r16 bes as g f4 r16 f g as | % m. 29
  bes4. as16 g as4 r | % m. 30
  r8 ges'16 f es des c bes as8 bes16 c des8 r | % m. 31
  r8 es16 des c bes as ges f8 g!16 a bes8 r | % m. 32
  r8 c16 bes as g f es des4~ des16 f bes as | % m. 33
  g4 as~ as16 g as bes c4 | % m. 34
  r16 as16 bes c des!4 r16 c des es f4 | % m. 35
  r16 es f g as4~ as8 g16 f g4~ | % m. 36
  g16 c, d! es f4~ f8 es16 d es4~ | % m. 37
  es16 as, bes c des4~ des8 c16 bes c4~ | % m. 38
  c8 f, bes as! g4~ g16 g as bes | % m. 39
  c4~ c16 c d! es f4~ f16 bes, es8~ | % m. 40
  es16 es d! c d es c d es c d es f es d c | % m. 41
  bes2~ bes16 as g f es4~ | % m. 42
  es16 es f es d! c' bes as g8 bes es4~ | % m. 43
  es16 g f es d!4~ d8 g, c4~ | % m. 44
  c16 es d! c bes4~ bes8 es, as4~ | % m. 45
  as16 c bes as g4~ g16 f as g f es d! c | % m. 46
  b!4 g'' as g | % m. 47
  fis4 b! c f,! | % m. 48
  e!4 es d!2~ | % m. 49
  d!8 c16 d es2.~ | % m. 50
  es8 d!16 es f2.~ | % m. 51
  f16 d! e! g bes,2.~ | % m. 52
  bes8 as g4 f16 e! f g as4 | % m. 53
  r16 f g as bes4 r16 as bes c des4 | % m. 54
  r16 c d! e! f4~ f8 e16 d e4 | % m. 55
  f4~ f16 es! des! c des4~ des16 bes c des | % m. 56
  e,!8 c'~ c16 g as f~ f f e d! es4 \prall | % m. 57
  f1 \fermata \bar "|." | % m. 58
  
}

alto = \relative c' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  r4 f as g | % m. 4
  fis4 b! c f,! | % m. 5
  e!4 es d!2 | % m. 6
  c8 c16 d! e!8 d16 e f \clef bass e,! f g as4 | % m. 7
  r16 f g as bes!4 r16 as bes c des!4 | % m. 8
  r16 \clef treble c d! e f4~ f8 e16 d e4 | % m. 9
  f8 f16 g as8 as16 bes c4~ c16 bes as g | % m. 10
  f4 r r8 as,16 bes c4~ | % m. 11
  c8 bes16 c des8 des16 es f2~ | % m. 12
  f8 e!16 f g4~ g8 f4 es8 | % m. 13
  d!8 es16 f g f e! d c8 r r16 bes as g | % m. 14
  f4 r16 f g as bes4. as16 g | % m. 15
  as4 r r8 f'16 es d!8 d16 c | % m. 16
  bes8 bes16 as g4 r8 g'16 f e!8 e16 d! | % m. 17 
  c8 c16 bes as4 r8 d'!16 c b!8 b16 a! | % m. 18
  g8 g16 a! b!8 a16 b c4~ c8 bes | % m. 19
  a!8 b!16 c d! c b a g4 r16 f es d! | % m. 20
  c4 r16 c' d! es f4. es16 d | % m. 21
  es8 c16 d! es2.~ | % m. 22
  es8 bes16 c des!2.~ | % m. 23
  des8 as16 bes c2.~ | % m. 24
  c8 bes16 as g4~ g4. f16 e! | % m. 25
  f4. g16 as bes4~ bes16 as bes g | % m. 26
  as4~ as16 g as f c'8 g c bes | % m. 27
  as8 bes c16 c, d! es f4 r8 g | % m. 28
  as4 r16 bes8. c4 r8 c | % m. 29
  des8 c16 bes c2 r4 | % m. 30
  R1 | % m. 31
  R1 | % m. 32
  R1 | % m. 33
  r4 es, f es | % m. 34
  d!4 g as des, | % m. 35
  c4 ces bes2 | % m. 36
  as4 r16 b'! c d! g,4. ges8 | % m. 37
  f4~ f16 g! as bes es,4~ es16 ges f es | % m. 38
  des2~ des16 bes c des es4~ | % m. 39
  es16 es f g as4~ as16 c bes as g8. g16 | % m. 40
  f2 c8 r r4 | % m. 41
  r16 c d! es f es d c bes4~ bes8 a!16 bes | % m. 42
  c4 bes~ bes8 es16 f g8 g16 as | % m. 43
  bes4~ bes16 as g f es d! c d es8 es16 f | % m. 44
  g4~ g16 f es des c bes as bes c8 c16 d! | % m. 45
  es4~ es16 d! c bes as8 \clef bass d,!16 es f8 fis | % m. 46
  g8 g16 a! b!8 a16 b c b c d! es4 | % m. 47
  r16 \clef treble c d! es f!4~ f16 es f g as!4 | % m. 48
  r16 g a! b! c4~ c8 b16 a b c b d! | % m. 49
  g,4 r8 g c, c'16 bes a!8 bes16 c | % m. 50
  f,8 bes r as! g d'!16 c b!8 c16 d | % m. 51
  g,4 r8 des c g'16 f e!8 f16 g | % m. 52
  c,8 f4 e!8 f4 r16 g f es | % m. 53
  d!4 r8 des~ des c r ges'~ | % m. 54
  ges8 f r16 f g! as bes2~ | % m. 55
  bes16 des! c bes a!4 r16 c bes as g f es des | % m. 56
  c1 | % m. 57
  c1 \fermata \bar "|." | % m. 58
  
}

tenor = \relative c' {
  \global
  
  r4 c des c | % m. 1
  b!4 e f bes, | % m. 2
  a!4 as g2 | % m. 3
  \clef bass f8 f16 g as8 as16 bes c b,! c d! es4 | % m. 4
  r16 c d! es f!4 r16 es f g as4 | % m. 5
  r16 g a! b! c4~ c8 b16 a b4 \prall | % m. 6
  c8 g c bes! as \clef treble f'4 es!8 | % m. 7
  d!8 es16 f g f e! d c4 r16 \clef bass bes as g | % m. 8
  f4 r16 f g as bes4. as16 g | % m. 9
  as4 r r8 \clef treble c16 des es4~ | % m. 10
  es8 des16 es f8 f16 g as4~ as16 ges f es | % m. 11
  des4 r r8 \clef bass f,16 g as8 as16 bes | % m. 12
  c8 g c bes as16 r r8 r16 c, d! es! | % m. 13
  f4 r8 g as r r bes | % m. 14
  c4 r8 c des! c16 bes c4~ | % m. 15
  c4 r r2 | % m. 16
  R1 | % m. 17
  R1 | % m. 18
  r4 g as g | % m. 19
  fis4 b! c f,! | % m. 20
  e!4 es d!2 | % m. 21
  c4 r8 \clef treble c''16 bes a!8 a16 g f8 g16 a | % m. 22
  bes8 bes, r bes'16 as! g8 g16 f es8 f16 g | % m. 23
  as8 as, r as'16 g f8 f16 es des8 des16 c | % m. 24
  bes8 c16 des es8 \clef bass es, as bes16 c des4~ | % m. 25
  des8 des16 c bes8 bes16 as g8 g16 f e!8 c | % m. 26
  f8 f'16 es des4 \trill  c8 c,16 d! e!8 d16 e | % m. 27
  f16 e! f g as4 r16 f g as bes!4 | % m. 28
  r16 as bes c des4 r16 c d! e! f4~ | % m. 29
  f8 e!16 d! e4 f r8 c16 des | % m. 30
  es8 es16 f ges4~ ges8 f16 es des8 as16 bes | % m. 31
  c8 c16 des es4~ es8 des16 c bes8 f16 g! | % m. 32
  as8 as16 bes c4~ c8 bes16 c des4~ | % m. 33
  des8 es16 des c8 des16 c bes4. as16 g | % m. 34
  as8 g16 as bes as g f es8 as~ as16 g as bes | % m. 35
  c4 r16 f, g as es4 r16 g f e! | % m. 36
  f4 r r2 | % m. 37
  R1 | % m. 38
  R1 | % m. 39
  r2 r4 bes | % m. 40
  c4 bes a! d! | % m. 41
  es4 as, g ges | % m. 42
  f2 es4 r | % m. 43
  r8 bes'16 c d! c bes as g4 r | % m. 44
  r8 g16 as bes as g f es4 r | % m. 45
  r8 es16 f g f es d! c8 b! c4 | % m. 46
  d!8 r r4 r8 \clef treble c''4 bes!8 | % m. 47
  a!8 bes16 c d! c b! a! g4 r16 g, as bes | % m. 48
  c4 r16 c d! es f d es f g as! g f | % m. 49
  es16 f es d! c8 c16 bes a!8 a16 g f8 g16 a | % m. 50
  bes8 f'16 es d!8 d16 c b!8 b16 a! g8 a16 b | % m. 51
  \clef bass c8 bes!16 as! g8 g16 f e!8 e16 d! c8 d16 e | % m. 52
  f16 g as bes c4~ c8 bes r as~ | % m. 53
  as8 g16 f g bes! as g f8 g16 as bes f es des | % m. 54
  c4~ c16 d! e! f g4~ g16 a,! bes c | % m. 55
  des!16 c des es! f4~ f16 f g as! bes4~ | % m. 56
  bes16 bes as g f4 g2 \prall | % m. 57
  a!1 \fermata \bar "|." | % m. 58
  
}

bass = \relative c {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  R1 | % m. 5
  R1 | % m. 6
  r4 c des c | % m. 7
  b!4 e f bes, | % m. 8
  a!4 as g2 | % m. 9
  f4 r8 f'16 g as8 as16 bes c8 c, | % m. 10
  des4 r8 des16 es f8 f16 g as8 as, | % m. 11
  bes4 r8 bes16 c des8 des16 es f8 f, | % m. 12
  c'8 c16 d! e!8 d16 e f e,! f g as4 | % m. 13
  r16 f g as bes4 r16 as bes c des4 | % m. 14
  r16 c d! e! f4~ f8 e16 d e4 | % m. 15
  f4 r8 f16 es! d!8 d16 c bes8 c16 d | % m. 16
  es4 r8 g16 f e!8 e16 d! c8 d16 e | % m. 17
  f4 r8 d!16 c b!8 b16 a! g8 a16 b | % m. 18
  c4 r r2 | % m. 19
  R1 | % m. 20
  R1 | % m. 21
  R1 | % m. 22
  R1 | % m. 23
  R1 | % m. 24
  R1 | % m. 25
  R1 | % m. 26
  r2 r4 c | % m. 27
  des!4 c b! e! | % m. 28
  f4 bes,! a! as | % m. 29
  g2 f8 f'16 g as8 as16 bes | % m. 30
  c4~ c16 bes as ges f8 des16 es f8 f16 g! | % m. 31
  as4~ as16 ges f es des8 bes16 c des8 des16 es | % m. 32
  f4~ f16 es des c bes c des c bes as g f | % m. 33
  es8 bes' c as des bes es c | % m. 34
  f8 bes,~ bes es16 des c8 f~ f16 es f g | % m. 35
  as16 g f es d!4 es8 d e! c! | % m. 36
  f8 f16 es! d!8 d16 c b!16 g a! b c as bes c | % m. 37
  des8 des16 c bes8 bes16 as g es f g as f g! a! | % m. 38
  bes16 a! bes c des bes c des es8 es16 des c8 c16 bes | % m. 39
  as8 as'16 g f8 f16 es d!4 es8 g | % m. 40
  as8 f bes bes, c c'16 bes as!8 bes16 as | % m. 41
  g8 bes,16 c d!8 c16 d es8 es16 d c8 c16 bes | % m. 42
  a!4 bes es r8 es16 f | % m. 43
  g8 g16 as bes8 bes, c4 r8 c16 d! | % m. 44
  es8 es16 f g8 g, as4 r8 as16 bes | % m. 45
  c8 c16 d! es8 es, f g as a! | % m. 46
  g8 r r4 r2 | % m. 47
  R1 | % m. 48
  R1 | % m. 49
  R1 | % m. 50
  R1 | % m. 51
  R1 | % m. 52
  r4 c des c | % m. 53
  b!4 e! f bes, | % m. 54
  a!4 as g2 | % m. 55
  f4~ f16 f g a! bes4~ bes16 des c bes | % m. 56
  c1 | % m. 57
  f,1 \fermata \bar "|." | % m. 58
  
}

\score {
  \new StaffGroup 
  <<
    \new Staff = "soprano"
      \soprano
    
    \new Staff = "alto"
      \alto
    
    \new Staff = "tenor" 
      \tenor
    
    \new Staff = "bass"
      { \clef bass \bass }
      
  >>
  
\layout {
  indent = 0.0
  }

}
