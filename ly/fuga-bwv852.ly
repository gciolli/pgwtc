\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue VII"
  subtitle = "in 3 voices"
  opus = "BWV 852"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key es \major
  \time 4/4
}

soprano = \relative c'' {
  \global
  
  bes16 g f g es as g as c8 bes r a!16 f | % m. 1
  es'8 d c4 \trill bes16 f'd bes as! f' d as | % m. 2
  g8 as' g f es16 c d es f4~ | % m. 3
  f16 es f g as f bes, as' g8 es16 g c4~ | % m. 4
  c8 d,16 f bes4~ bes8 c,16 es as!8 g | % m. 5
  f4 g8 d! es4. f8 | % m. 6
  g4. a!8 bes2~ | % m. 7
  bes16 bes g es des bes' g des c8 es as4~ | % m. 8
  as16 as f d c as' f c bes8 d g4~ | % m. 9
  g4~ g16 c, d es f4. es8~ | % m. 10
  es16 d c d bes es d es g8 f r d16 bes | % m. 11
  as'8 g f4 es16 bes' g es des bes' g des | % m. 12
  c8 as' r as, d16 as' f d c as' f c | % m. 13
  b!8 g' r g, c16 g' e! c bes g' e bes | % m. 14
  a!16 g' c, g as f' d as g f' d b! g es' c g | % m. 15
  fis16 es' c a! f! d' b! f e! d' b g es c' g es | % m. 16
  d16 c' as f d b'! a! b c8 f es d | % m. 17
  c16 a! b! c d4~ d16 c d es f d g, f' | % m. 18
  es16 g es d c c' as f d f d c bes bes' g es | % m. 19
  c es c bes as as' f d b!4 c~ | % m. 20
  c8 b! c16 b c d es8 d e! fis | % m. 21
  g16 d c d bes e! d e  g8 f r4 | % m. 22
  f16 c bes c as d c d f8 es r4 | % m. 23
  r16 bes es c des4 r16 c f d! es4 | % m. 24
  r16 d g es f4 r16 es g bes! as f bes, g' | % m. 25
  f8 as g f es16 c d es f4~ | % m. 26
  f16 es f g as f bes, as' g bes g es des4 | % m. 27
  c16 c' as f es4 d16 f d bes as4~ | % m. 28
  as16 g f g es as g as c8 bes r a!16 f | % m. 29
  es'8 d c4 \trill bes8 f' d bes | % m. 30
  bes'2~ bes8 es, c as | % m. 31
  as'2~ as8 d, bes g | % m. 32
  g'4~ g16 c, d es f es d c bes as bes c | % m. 33
  f,4 r8 f es'd r4 | % m. 34
  f8 es4 d8 es16 g es c as4~ | % m. 35
  as16 f' d bes g8 es' g,4 f | % m. 36
  es1 \fermata \bar "|." | % m. 37
   
}

mezzo = \relative c' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  es16 d c d bes es d es g8 f r d16 bes | % m. 3
  as'8 g f4 \trill es16 bes' g f es c' a! f | % m. 4
  d16 a'! f es des bes' g es c g' es c d! bes' es, c' | % m. 5
  d8 des c bes as16 g as bes c4~ | % m. 6
  c16 bes c d es c f, es' d8 f d bes | % m. 7
  es8 r r4 r8 es c as | % m. 8
  d8 r r4 r8 d bes g | % m. 9
  c8 bes as4~ as16 c bes as g4 | % m. 10
  f4 bes~ bes8 as16 g as8 f | % m. 11
  d8 es'4 d8 es r es,4~ | % m. 12
  es16 es c as \clef bass g es' c g f8 as d4~ | % m. 13
  d16 d b! g f d' b f e!8 g c4~ | % m. 14
  c4 b!8 c d r r4 | % m. 15
  R1 | % m. 16
  r2 \clef treble c16 bes! as! bes g c b! c | % m. 17
  es8 d r b!16 g f'8 es d4 \trill | % m. 18
  c16 b! c es as4~ as8 bes,16 d g4~ | % m. 19
  g8 as,16 c f4~ f16 g as4 g8 | % m. 20
  f16 es f g a!4~ a16 g a bes c a d, c' | % m. 21
  bes4 bes~ bes16 des c bes as g f g | % m. 22
  as4 as~ as16 c bes as g f es f | % m. 23
  g4 r16 bes g es as,8 r r16 c' a! f | % m. 24
  bes,8 r r16 d' b! g c,8 r r4 | % m. 25
  r8 f' es as, bes as16 g as d bes f | % m. 26
  d8 es4 d8 es r r16 bes' g es | % m. 27
  as,8 r r16 c' a! f bes,8 r r16 f' d bes | % m. 28
  \clef bass es,8 des' c bes as16 g as bes c4~ | % m. 29
  c16 bes c d es c f, es' d8 r r4 | % m. 30
  \clef treble r16 bes' es g des bes des g c,8 r r4 | % m. 31
  r16 as d f c as c f bes,8 r r4 | % m. 32
  r8 bes as2 g8 es~ | % m. 33
  es16 d c d bes es d es ges8 f r d16 bes | % m. 34
  as'8 g f4 es r8 f | % m. 35
  <<
   { s1 | \mergeDifferentlyDottedOn r16 des8. c ces16 bes2 \fermata } 
   \\
   { bes4. f'8~ f16 d es bes~ bes c d as~ | as16 des bes g~ g as f8 g2 \fermata \bar "|." }
  >> | % mm. 36 and 37
  
}

bass = \relative c' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  R1 | % m. 5
  bes16 g f g es as g as c8 bes r a!16 f | % m. 6
  es'8 d c4 \trill bes16 f' d bes as! f' d as | % m. 7
  g8 es' r es, as16 es' c as g es' c g | % m. 8
  f8 d' r d, g16 d' bes g f des' bes g | % m. 9
  e!16 c' g e f c' as f d bes' f d es bes' g es | % m. 10
  bes8 as' g f es16 c d es f4~ | % m. 11
  f16 es f g as f bes, as' g8 bes g es | % m. 12
  as8 r r4 r8 as f d | % m. 13
  g8 r r4 r8 g e! c | % m. 14
  f8 es! d c b! g c bes | % m. 15
  a!8 a'! b! g c g as es | % m. 16
  f8 es16 f g8 g, c r r4 | % m. 17
  R1 | % m. 18
  R1 | % m. 19
  r2 g'16 es d es c f es f | % m. 20
  as8 g r fis16 d c'8 bes a!4 \trill | % m. 21
  g16 bes as! bes g as f g e! c' as f c f c as | % m. 22
  f16 as' g as f g es f d bes' g es bes es bes g | % m. 23
  es8 es' f g as f g a! | % m. 24
  bes8 g a! b! c c, d es~ | % m. 25
  es16 d c d bes es d es g8 f r d16 bes | % m. 26
  as'8 g f4 es8 es, f g | % m. 27
  as8 f g a! bes bes c d | % m. 28
  es2.~ es16 d! es f | % m. 29
  g8. f16 es8 f bes,16 f' d bes as f' d as | % m. 30
  g8 g' r es, as16 es' c as g es' c g | % m. 31
  f8 f' r d, g16 d' bes g f d' bes f | % m. 32
  e!16 c' g e f c' as f d bes' f d es bes' g es | % m. 33
  bes'8 f' d bes a'! as r16 as f d | % m. 34
  bes8 es bes' b! c4~ c16 c as f | % m. 35
  d8 bes r16 es c as bes2 | % m. 36
  es,1 \fermata \bar "|." | % m. 37
  
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
