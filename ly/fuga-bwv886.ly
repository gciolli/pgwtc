\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue XVII"
  subtitle = "in 4 voices"
  opus = "BWV 886"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key as \major
  \time 4/4
}

soprano = \relative c''' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  r8 as g c f, g16 as bes8 bes, | % m. 3
  es8 f16 g as4~ as16 bes g as f g as bes | % m. 4
  g16 f g as bes as g bes as g f es des c des es | % m. 5
  c8 es as4 g ges | % m. 6
  f4 fes es8 as4 g8 | % m. 7
  as8 r r4 r2 | % m. 8
  R1 | % m. 9
  R1 | % m. 10
  R1 | % m. 11
  R1 | % m. 12
  R1 | % m. 13
  R1 | % m. 14
  R1 | % m. 15
  R1 | % m. 16
  R1 | % m. 17
  R1 | % m. 18
  R1 | % m. 19
  R1 | % m. 20
  R1 | % m. 21
  r8 es c f bes, c16 des es8 es, | % m. 22
  as8 bes16 c des4~ des16 es c! des bes c des es | % m. 23
  c8 e! f4 e es | % m. 24
  d!4 des c8 f4 es8 | % m. 25
  es8 es, d! d'!~ d d, c c'~ | % m. 26
  c8 c, b! b'! c16 b c d! es d c es | % m. 27
  d!16 c d es f es d f e! d e f g f e g | % m. 28
  f8 c f2 e!8 es~ | % m. 29
  es8 c'16 bes a! g f es des4 r8 des | % m. 30
  es8 ges c, f bes, r r4 | % m. 31
  bes2~ bes16 bes as ges f ges as f | % m. 32
  ges16 as ges f es f ges es f8 ges as f | % m. 33
  bes8 d! es8. f16 des8 c bes es | % m. 34
  c8 f des ges c, des16 es f8 f, | % m. 35
  bes8 c16 des es4~ es16 f des es c des es f | % m. 36
  bes,4 c des2 | % m. 37
  c4 ces bes beses~ | % m. 38
  beses8 as4.~ as8 g! as \once \override Accidental #'restore-first = ##t bes! | % m. 39
  c8 d! es16 fes des es ces des es fes des es ces des | % m. 40
  bes16 es f g as4~ as16 as g f g8 ges~ | % m. 41
  ges16 ges f es f8 fes es2~ | % m. 42
  es16 es d! c d8 des~ des16 des c bes c8 ces | % m. 43
  bes8 c! des4~ des4. c8 | % m. 44
  des4 r fes r | % m. 45
  g!4 r bes \fermata r16 des, c bes | % m. 46
  c16 as bes c des es f g as es f des c f d! es | % m. 47
  as,8. f32 g g8. \trill f32 g as8. bes16 c8. d!16 | % m. 48
  es2~ es8 des16 c bes g! as8 | % m. 49
  bes8 as4 g8 as2 \bar "|." | % m. 50
    
}

alto = \relative c'' {
  \global
  
  r8 es c f bes, c16 des es8 es, | % m. 1
  as8 bes16 c des4~ des16 es c des bes c des es | % m. 2
  c16 bes c d! es4 d des | % m. 3
  c4 ces bes8 es4 d!8 | % m. 4
  es8 bes g c f,8 g16 as bes4~ | % m. 5
  bes16 bes as g as es' des c des es des c bes des c bes | % m. 6
  c16 des c bes as ces bes as g8 es' des bes | % m. 7
  es16 f es des c bes as g as bes as g f g es f | % m. 8
  g16 as g f es ges f es d!8 bes' as f | % m. 9
  g8 bes es2 d!8 des~ | % m. 10
  des8 bes'16 as g f es des c4 r8 c | % m. 11
  des8 f bes, es as, r r4 | % m. 12
  r8 f'16 es des c bes as g! as f g as g as8~ | % m. 13
  as16 g as bes g f ges8~ ges16 f ges as f es f as | % m. 14
  g!8 as4 g8~ g e! f4~ | % m. 15
  f4 e! f8 as g c | % m. 16
  f,8 g16 as bes8 bes, es f16 g as4~ | % m. 17
  as16 bes g as f g as bes g4 as | % m. 18
  g4 ges f fes | % m. 19
  es8 as4 ges8~ ges16 as f ges es f ges as | % m. 20
  f8 bes4 as8~ as16 bes g! as f g as bes | % m. 21
  g4 r8 f g4 r8 as | % m. 22
  as8 r r bes~ bes as f es~ | % m. 23
  es8 c' as des g, as16 bes c8 c, | % m. 24
  f8 g16 as bes4~ bes16 c as bes g as bes c | % m. 25
  as16 bes g as f g as bes g as f g es f g as | % m. 26
  f16 g es f d! es f g es4 r8 es | % m. 27
  f8 as d,! g c, r r4 | % m. 28
  r16 e! f g as g f as g f g as bes as g bes | % m. 29
  a!16 g a bes c bes a c bes8 f bes4~ | % m. 30
  bes4 a!8 as~ as f'16 es d! c bes as | % m. 31
  ges16 as ges f es f ges es f ges f es d! es f d | % m. 32
  es16 f es des! c d! es c d8 es4 d8 | % m. 33
  es8 as ges c f,4 r | % m. 34
  r4 bes a! as | % m. 35
  g4 ges f8 bes4 as8 | % m. 36
  ges4~ ges8 es as16 ges f as ges as ges f | % m. 37
  es16 f ges es f ges f es des es f des es fes es des | % m. 38
  c!4 fes~ fes8 es4 es8 | % m. 39
  as8 f! ges g! as16 bes ges as f! g! as bes | % m. 40
  g4 r16 es' d! c d8 bes~ bes16 des c bes | % m. 41
  c8 as~ as16 ces bes as g!8 as bes g | % m. 42
  as8 f~ f16 as g f g8 es~ es16 bes' as ges | % m. 43
  f8 beses as fes beses as beses as | % m. 44
  beses4 r des r | % m. 45
  fes4 r <des es> \fermata r | % m. 46
  r2 r4 r8 bes, | % m. 47
  c8 ces bes des c!16 f8.~ f16 es as8 | % m. 48
  bes4 beses as r | % m. 49
  es2 es \bar "|." | % m. 50
    
}

tenor = \relative c' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  R1 | % m. 5
  r8 es c f bes, c16 des es8 es, | % m. 6
  as8 bes16 c des4~ des16 es c! des bes c des es | % m. 7
  c16 bes c des es4 d! des | % m. 8
  c4 ces bes8 es4 d!8 | % m. 9
  es4 r8 g as c f, bes | % m. 10
  es,8 r r4 r8 es as4~ | % m. 11
  as4 g8 ges~ ges es'16 des c bes as ges | % m. 12
  f8 r r4 r es | % m. 13
  d!4 des c ces | % m. 14
  bes8 es des bes c16 des bes c as bes c des | % m. 15
  g,2 as8 r r4 | % m. 16
  R1 | % m. 17
  r2 r8 es' c f | % m. 18
  bes,8 c16 des es8 es, as bes16 c des4~ | % m. 19
  des16 es c des bes c des es c8 des4 c8~ | % m. 20
  c16 es des es c des es f d!8 es4 des8~ | % m. 21
  des16 des c bes as bes c d! es f es des c es des c | % m. 22
  des16 es des c bes ces as bes g8 as4 g8~ | % m. 23
  g8 r r16 f g as bes8 r r4 | % m. 24
  R1 | % m. 25
  R1 | % m. 26
  R1 | % m. 27
  R1 | % m. 28
  R1 | % m. 29
  R1 | % m. 30
  R1 | % m. 31
  r8 \clef bass bes ges ces f, ges16 as bes8 bes, | % m. 32
  es8 f16 ges as4~ as16 bes ges as f ges as bes | % m. 33
  ges16 as f ges es f g! a! bes c as bes g a! bes c | % m. 34
  a!16 \clef treble f g a bes c des es f ges f es d! bes c d | % m. 35
  es16 f es des! c a! bes c des8 bes f'4~ | % m. 36
  f16 f es des es des c bes as8 des bes r | % m. 37
  c8 r as r bes r r4 | % m. 38
  r8 es~ es as, des bes ces r | % m. 39
  R1 | % m. 40
  r8 es c f bes, c16 d! es8 es, | % m. 41
  as8 bes16 c des4~ des16 es c! des bes c des es | % m. 42
  c4 bes~ bes as16 ges' f es | % m. 43
  d!8 es fes des es fes ges es | % m. 44
  fes4 r beses r | % m. 45
  des4 r bes \fermata r | % m. 46
  r2 r4 r8 as, | % m. 47
  as4 g8 bes as des c f | % m. 48
  bes,8 c16 des es8 es, as bes16 c des4~ | % m. 49
  des16 es c des bes c des es c2 \bar "|." | % m. 50
    
}

bass = \relative c' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  R1 | % m. 5
  R1 | % m. 6
  R1 | % m. 7
  r8 as g c f,8 g16 as bes8 bes, | % m. 8
  es8 f16 g as4~ as16 bes g! as f g as bes | % m. 9
  es,16 d! es f g f es g f es f g as g f as | % m. 10
  g16 f g as bes as g bes as g as bes c bes as c | % m. 11
  bes16 as bes c des c bes des c bes c des es des c es | % m. 12
  des16 c des es f es des f es8 es, c f | % m. 13
  bes,8 c16 d! es8 es, as bes16 c des4~ | % m. 14
  des16 es c des bes c des es as,8 c des as | % m. 15
  bes16 c as bes g as bes c f, c' f8~ f16 es des c | % m. 16
  des16 c' bes as g f es des c bes' as g f es d! c | % m. 17
  bes8 es4 des!8~ des16 c des es f es des c | % m. 18
  des16 f es des c des es8~ es16 es des c bes ces bes as | % m. 19
  g8 as des es as bes ges as | % m. 20
  des,8 ges es f bes c as bes | % m. 21
  es,4 as g ges | % m. 22
  f4 fes es8 f! des es | % m. 23
  as,16 c bes as des4~ des16 des' c bes a! bes c a | % m. 24
  bes16 c bes as! g as bes g as8 des bes c | % m. 25
  f,4 r8 bes es,4 r8 as | % m. 26
  d,!8 f g g, c g' c4~ | % m. 27
  c4 b!8 bes~ bes g'16 f e! d! c bes | % m. 28
  as4 r8 as bes des g, c | % m. 29
  f,8 r r4 r16 a,! bes c des c bes des | % m. 30
  c16 bes c des es des c es d! c d es f es d f | % m. 31
  es4 r r2 | % m. 32
  R1 | % m. 33
  R1 | % m. 34
  R1 | % m. 35
  R1 | % m. 36
  r4 r8 as f bes es, f16 ges | % m. 37
  as8 as, des es16 f ges4~ ges16 as fes ges | % m. 38
  es16 fes ges as des, es ces des bes ces des es as, bes ges as | % m. 39
  f16 ges as bes es,8 r r2 | % m. 40
  R1 | % m. 41
  r2 r8 as' g c | % m. 42
  f,8 g16 as bes8 bes, es f16 g as4~ | % m. 43
  as16 bes ges as fes ges as beses ges as fes ges es fes ges as | % m. 44
  des,16 des' ces des beses des fes, beses des, beses' as beses fes beses des, fes | % m. 45
  bes,!16 fes' es fes des fes bes, des g,4 \fermata r16 es' f! g | % m. 46
  as16 c bes as g f es des c8 des es f | % m. 47
  <<
    { s2.  as4 | g4 ges f fes | es2 es }
    \\ 
    { es8 d! es e! f8. g16 as8 f | es4 r8 c des4 r16 bes ces des | g,8 as es4 as2 \bar "|." }
  >> | % mm. 48 - 50
    
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
