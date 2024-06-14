\version "2.18.2"

\header {
  title = "Fugue II"
  subtitle = "in 4 voices"
  opus = "BWV 871"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key c \minor
  \time 4/4
}

soprano = \relative c'' {
  \global
  
  R1 | % m. 1
  r8 c bes c d g, c bes16 a! | % m. 2
  bes4 as! g8 c f,4~ | % m. 3
  f8 es16 d g8 f es4 d | % m. 4
  c8 g' c4~ c8 bes16 c d e! f! g | % m. 5
  as16 f c' bes as g f es d bes f' es d c bes as | % m. 6
  g8 r r4 r2 | % m. 7
  r8 r16 g' es8. f16 g8 c, f es16 d | % m. 8
  es8 as, des c16 bes c8 f, bes as16 g | % m. 9
  as8. as16 d,8 es f es4 f8~ | % m. 10
  f8 es as8. as16 g8. g16 c8. bes!16~ | % m. 11
  bes16 bes as8~ as16 g as bes e,!8 r16 as des8. c16~ | % m. 12
  c16 c bes8~ bes16 a! bes c fis,!8 g~ g16 g8 fis16 | % m. 13
  g8 g' es f g c, f es16 d | % m. 14
  es16 d c b! c es f g as c, b a! bes g' f g | % m. 15
  es8 r r4 r8 g e! fis | % m. 16
  g8 c, f es16 d es8. es16 d8 e! | % m. 17
  f8 bes, es d16 c d8 g, es'4~ | % m. 18
  es16 es d8~ d16 c8 b!16 c8 g' d es16 f | % m. 19
  es8 d r16 c8 bes!16 as8. as16 g4~ | % m. 20
  g4 c2~ c16 b! c d | % m. 21
  g,8 as g8. a!16 bes8 b! r16 g c fis, | % m. 22
  g8 es d4 c8 r r c' | % m. 23
  b!8 c d g, c bes16 as bes8 c | % m. 24
  as8 bes c f, bes as16 g as8 g~ | % m. 25
  g8 a!16 bes32 c f,8 c' d! c16 b! c4 | % m. 26
  r16 d es b! c4~ c r8 b | % m. 27
  c1 \bar "|." | % m. 28
    
}

alto = \relative c'' {
  \global
  \mergeDifferentlyDottedOn
  
  r8 g es f g c, f es16 d | % m. 1
  es4 d8 c bes4 a! \prall | % m. 2
  g8 g'4 f es d16 c | % m. 3
  b!4 c2 b4 | % m. 4
  c4 r8 es d16 fis g8 r4 | % m. 5
  r2 r8 d bes d | % m. 6
  es16 c as' g f es d c b! f' es d es as g f | % m. 7
  d'16 c d8~ d16 b! c8~ c16 b c8~ c16 b8. | % m. 8
  c16 g as8~ as16 g8.~ g16 e! f8~ f16 e8. | % m. 9
  f8 c b! c d g, c b!16 a! | % m. 10
  g16 b! c8~ c16 c d8~ d16 d es8~ es16 f ges8 | % m. 11
  e!8 r16 f des4~ des16 c f8~ f16 g as8 | % m. 12
  fis8 r16 g es4~ es16 d8 c16 bes8 a! | % m. 13
  g8 r g'4 es f | % m. 14
  g4 c, f es8 d | % m. 15
  es8 c' bes! c d g, c bes16 a! | % m. 16
  bes8 as!16 g c c, g'8~ g16 g a!8 bes16 c des8 | % m. 17
  r16 c bes as g8 a! bes es, as g16 f | % m. 18
  g8. f16 es8 d c es' as,4 | % m. 19
  r16 g8 f16 es8 e! r16 f d8 r16 es f8~ | % m. 20
  f8 es~ es16 e! f g as4 g8 f~ | % m. 21
  f16 es8 f16~ f f es8 r16 f es d es4 | % m. 22
  r16 d8 c16~ c a! b!8 c g' es f | % m. 23
  g8 c, f es16 d e!8 f g f16 e | % m. 24
  f8 g e! f g c, f e16 d! | % m. 25
  e!8 es des es f es16 d! es8 g | % m. 26
  <<
    { s4 s16 a! fis8~ fis4 s8 }
    \\
    { \tieUp b4\rest b16\rest a!8.~ \stemUp a4 b8\rest }
  >>  <fis g>8 | % m. 27
  <e! g>1 \bar "|." | % m. 28
    
}

tenor = \relative c' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  r8 g es f g c, f es16 d | % m. 4
  es16 g c, d es f g a! bes d g, a bes c d e! | % m. 5
  f8 as,! f as bes r r4 | % m. 6
  r8 c, b! c d g, c bes16 a! | % m. 7
  b!8 g c as! es8. as'16 d,8 g | % m. 8
  c,8. f16 bes,8 es as,8. des16 g,8 c | % m. 9
  f,16 c' f8~ f16 es d c b! g c bes as c d,8 | % m. 10
  es8. c16 f8 bes b! c as bes | % m. 11
  c8 f, bes as16 g as8 as'16 f bes8 c | % m. 12
  d,8 g c, bes16 a! bes8 es c d | % m. 13
  g,8 r r4 r2 | % m. 14
  r8 g' as g f d' g, a!16 b! | % m. 15
  c16 d es f g es d c bes a! bes8~ bes16 a d c | % m. 16
  bes16 g c8 a! b! c f, bes as16 g | % m. 17
  a!8 d~ d16 c f es d c des8~ des16 c bes as | % m. 18
  bes8 b! c d g,16 c b c~ c b c8~ | % m. 19
  c8 b! c4~ c8 b! c8. b16~ | % m. 20
  b16 g c8~ c bes as16 g' f es d4~ | % m. 21
  d8 c d c f, g4 a!8 | % m. 22
  g4 r8 r16 f es8 r r4 | % m. 23
  r8 es' b! c g r r4 | % m. 24
  r8 des' g, as e! c' as bes | % m. 25
  c8 f, bes as16 g as8 g g es' | % m. 26
  r4 r8 r16 d es4 r8 d | % m. 27
  c1 \bar "|." | % m. 28
    
}

bass = \relative c' {
  \global
  
  s1 | % m. 1
  s1 | % m. 2
  s1 | % m. 3
  s1 | % m. 4
  s1 | % m. 5
  s1 | % m. 6
  s1 | % m. 7
  s1 | % m. 8
  s1 | % m. 9
  s1 | % m. 10
  s1 | % m. 11
  s1 | % m. 12
  s1 | % m. 13
  s1 | % m. 14
  s1 | % m. 15
  s1 | % m. 16
  s1 | % m. 17
  R1 | % m. 18
  r4 g es f | % m. 19
  g c, f es8 d | % m. 20
  es8. g16 as8 g f d' g, a!16 b | % m. 21
  c8 c, b! c d g, c b!16 a! | % m. 22
  b!8 c f, g c, r r4 | % m. 23
  r2 c'8 r r4 | % m. 24
  r2 c8 r r4 | % m. 25
  r8 c des c b! f' es16 d! c d32 es | % m. 26
  fis,4 r r16 b'! c fis, g8 g, | % m. 27
  <c, c'>1 \bar "|." | % m. 28
    
}

\paper {
  max-systems-per-page = 4
}

\score {
  \new StaffGroup
  <<
    \new Staff = "soprano" 
      \soprano
    
     \new Staff = "alto" 
       \alto
    
     \new Staff = "tenor" \with {
      \override VerticalAxisGroup.remove-first = ##f } 
      { \clef bass \tenor }
    
    \new Staff = "bass" 
      { \clef bass \bass }
      
  >>
  
\layout { 
  \context {
    \Staff \RemoveEmptyStaves
    \override VerticalAxisGroup.remove-first = ##t
  }
  \context {
    \StaffGroup
    \remove "Keep_alive_together_engraver"
  }
  indent = 0.0
  }
  
}
