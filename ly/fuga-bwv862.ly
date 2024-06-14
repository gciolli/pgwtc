\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue XVII"
  subtitle = "in 4 voices"
  composer = "BWV 862"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key as \major
  \time 4/4
}

soprano = \relative c'' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  r4 as8 es' c as f' des | % m. 5
  es2. \tieDashed as4~ | % m. 6
  as4 \tieSolid g f2~ | % m. 7
  f16 bes, c d! es4~ es8 as des, bes' | % m. 8
  des,4 c bes2 | % m. 9
  as2~ as16 as bes c des4~ | % m. 10
  des16 bes c es as g as bes c bes as g f g as c | % m. 11
  bes16 as g f es f g bes as g f es d! es f as | % m. 12
  g16 f e! g f e f g c,8 f~ f g16 es | % m. 13
  f16 e! f g as2 g4~ | % m. 14
  g4 f2 e!4 | % m. 15
  f4~ f16 es! des c bes as g as bes des c bes | % m. 16
  a!16 c des es f es des c bes des ges f es des c bes | % m. 17
  a!4 bes16 c des8~ des c16 des es4~ | % m. 18
  es16 c des f bes8 f ges es as es | % m. 19
  f8 des16 f ges8 des es c f c | % m. 20
  des2. c8 f | % m. 21
  d!8 bes es2 d!4 | % m. 22
  es4 r r2 | % m. 23
  r4 as,8 es' c as ges' es | % m. 24
  f4 f8 g! es4 es8 f | % m. 25
  des4 des8 bes' c, des16 bes c8 as' | % m. 26
  bes,4 r16 es f g as4~ as16 g f as | % m. 27
  g2.~ g16 c, f8~ | % m. 28
  f16 es des! f es2 des4~ | % m. 29
  des16 c bes des c8 f des bes ges' es | % m. 30
  f4 as8 f g4 bes8 g | % m. 31
  as8 f des!4~ des8 es c f | % m. 32
  c4 bes~ bes8 as16 g as8 es' | % m. 33
  c8 as f' des es g, as4~ | % m. 34
  as4 g as2 \fermata \bar "|." | % m. 35
  
}

alto = \relative c'' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  R1 | % m. 5
  r4 es,8 as g es c' as | % m. 6
  bes4~ bes16 es, f g as bes c bes as g f as | % m. 7
  g2 f | % m. 8
  r16 es f g as2 g4 | % m. 9
  as8 g16 f es des c des es8 r r as, | % m. 10
  as'8 r r4 r2 | % m. 11
  R1 | % m. 12
  r4 f8 c' as f des'! bes | % m. 13
  c8 d!16 e! f8 c des bes es bes | % m. 14
  c8 as des as bes16 c des c bes as g bes | % m. 15
  as4 r r2 | % m. 16
  R1 | % m. 17
  r4 f8 bes ges es c' a! | % m. 18
  bes4 r16 f ges as! bes as ges f es f g bes | % m. 19
  as16 ges f es des es f as ges f es des c des es ges | % m. 20
  f4 f8 bes g! es~ es as | % m. 21
  f8 d! r g as16 bes c bes as g f as | % m. 22
  g16 as bes g es8 bes' g es des' bes | % m. 23
  c4 r r c8 es~ | % m. 24
  es8 as, bes des~ des g, a! c~ | % m. 25
  c8 f, bes2 as4~ | % m. 26
  as16 g as bes c4~ c16 des es c des4~ | % m. 27
  des16 c bes des c4~ c16 bes as g as8. as16 | % m. 28
  g8 r es as f des bes' g | % m. 29
  as4 r r r8 \tieDashed es'~ | % m. 30
  es8 des r f~ f8 \tieSolid es r g | % m. 31
  f8 r r bes, g bes c as | % m. 32
  as4. g8 f4 r8 bes | % m. 33
  as8 r r bes bes r r f | % m. 34
  bes,16 c des f es des c des c2 \fermata \bar "|." | % m. 35  
  
}

tenor = \relative c' {
  \global
  
  r4 as8 es' c as f' des | % m. 1
  es4~ es16 des c des es f g es as bes c bes | % m. 2
  as16 g f as g2 f4~ | % m. 3
  f16 es d! f es4~ es8 des16 c des f bes as | % m. 4
  g16 f es des c bes c des es4 as, | % m. 5
  g4 as16 bes c8 bes8. bes16 as8 c | % m. 6
  f,8 r r4 r2 | % m. 7
  R1 | % m. 8
  R1 | % m. 9
  r4 as8 es' c as f' des | % m. 10
  es4 es2 des4~ | % m. 11
  des4 c2 bes4~ | % m. 12
  bes4 \clef bass as8 g~ g16 bes as g f e! f g | % m. 13
  as16 bes as g f8 r r2 | % m. 14 
  R1 | % m. 15
  R1 | % m. 16
  r4 bes8 f' des bes ges' es | % m. 17
  f4 bes,2 a!16 bes c a | % m. 18
  f8 bes16 c des2 c4~ | % m. 19
  c4 bes2 a!4 | % m. 20
  bes16 es des c bes as g! f es des' c bes as  g f g | % m. 21
  as16 c bes as g f es des! c8 as' bes bes, | % m. 22
  es4~ es16 f g as bes c des c bes as g bes | % m. 23
  as16 es f g as bes c des es f ges f es des c es | % m. 24
  des16 es f es des c bes des c des es des c bes a! c | % m. 25
  bes16 c des c bes as! g bes as bes as g f es f des | % m. 26
  es16 des c bes as8 r r2 | % m. 27
  r4 g'8 c as f d'! b! | % m. 28
  c16 es, f g as bes! c bes as g f g as g f es | % m. 29
  f16 es des f es8 r r4 r8 \clef treble c'' | % m. 30
  as4 r8 d! bes4 r8 e! | % m. 31
  c8 r r4 r8 g as f | % m. 32
  es4 des c r8 es | % m. 33
  es8 r r f es r r \clef bass bes | % m. 34
  bes4 es, es2 \fermata \bar "|." | % m. 35 
  
}

bass = \relative c {
  \global
  
  R1 | % m. 1
  r4 es8 as g es c' as | % m. 2
  bes8 es, bes' g as es as f | % m. 3
  g4~ g16 c, des! es f g as f bes c des c | % m. 4
  bes16 as g bes as4~ as16 as, bes c des es f es | % m. 5
  des16 c bes des c8. d!16 es f g8~ g16 g f es | % m. 6
  d!16 bes c d es4~ es8 as d, bes' | % m. 7
  es,4~ es16 es f g as bes c bes as g f as | % m. 8
  g8 es r16 as, bes c des es f es des c bes des | % m. 9
  c4~ c16 des es c f4~ f16 es f des | % m. 10
  as'16 g as bes c8 g as f bes f | % m. 11
  g8 es as es f d! g d | % m. 12
  e!8 c d! e f4 bes, | % m. 13
  f'4 r16 f, g as bes as g f es f g bes | % m. 14
  as16 g f es des es f as g8 bes c c, | % m. 15
  f16 c' d! e! f4~ f8 bes e,! c' | % m. 16
  es,!8 a! des,4 ges r8 ges16 f | % m. 17
  es16 ges f es des c bes des es f ges es f4 | % m. 18
  bes,4 r r2 | % m. 19
  R1 | % m. 20
  R1 | % m. 21
  R1 | % m. 22
  R1 | % m. 23
  R1 | % m. 24
  R1 | % m. 25
  R1 | % m. 26
  r4 as8 es' c as f' des | % m. 27
  es4 e!16 bes' as g f2 | % m. 28
  c2 des | % m. 29
  as4~ as16 f g as bes c des es des c bes as | % m. 30
  des16 c des f es d! c bes es d es g f e! d c | % m. 31
  f16 g as f bes as g f es des c bes as bes c des | % m. 32
  es16 des c des es8 es, f4 r16 es f g | % m. 33
  as16 bes c es des c bes as g f es des c es des f | % m. 34
  es2 as2 \fermata \bar "|."
  
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
