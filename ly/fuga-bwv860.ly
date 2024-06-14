\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

#(ly:expect-warning "Previous mark event here")
#(ly:expect-warning "Two simultaneous mark events, junking this one")

\header {
  title = "Fugue XV"
  subtitle = "in 3 voices"
  opus = "BWV 860"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key g \major
  \numericTimeSignature
  \time 6/8
}

soprano = \relative c'' {
  \global
  
  g8 a16 g fis g a8 b16 a g a | % m. 1
  b8 a g d c'4 | % m. 2
  b8 a g fis e'4 | % m. 3
  d8 e16 d c b a8 c16 b a g | % m. 4
  fis8 g a g a b | % m. 5
  a8 b16 cis d b cis b cis d e cis | % m. 6
  d16 cis d e fis d e d e fis g e | % m. 7
  fis8 g16 fis e d g4.~ | % m. 8
  g16 fis e g d g cis, g' b, g' a, g' | % m. 9
  fis8 d e fis g a~ | % m. 10
  a8 d, g~ g fis e | % m. 11
  d8 c b a4 r8 | % m. 12
  r8 r d~ d c a'~ | % m. 13
  a8 g4~ g8 a16 g fis e | % m. 14
  d2.~ | % m. 15
  d8 d c b a g | % m. 16
  c16 d e4~ e16 d c b a c | % m. 17
  b16 c d4~ d16 c b a g b | % m. 18
  a16 b c4~ c16 b a g fis a | % m. 19
  g16 a b4~ b8 e, a~ | % m. 20
  a16 g a8 b c16 d c b a g | % m. 21
  fis8. \prall e32 fis g8~ g16 b a g fis e | % m. 22
  d8 r r r4. | % m. 23
  a'8 g16 a b a g8 fis16 g a g | % m. 24
  fis8 g a d e,4 \trill | % m. 25
  fis8 g a b cis,4 \trill | % m. 26
  d8 d16 e fis g a8 fis16 g a b | % m. 27
  c4 b8 a4. | % m. 28
  g16 g' fis e d fis e fis e d c e | % m. 29
  d16 e d c b d c d c b a c | % m. 30
  b16 a g b fis b e, b' d, b' cis,! b' | % m. 31
  a16 g fis a e a d, a' cis,! a' b, a' | % m. 32
  g16 fis e g d g cis, g' b, g' a, g' | % m. 33
  fis16 d' c! b a g fis e d c b a | % m. 34
  g16 a b g c g d' g, e' g, f'! g, | % m. 35
  e'16 c' b d a d gis, d' fis, d' e, d' | % m. 36
  c16 a b cis dis e fis g a b c! dis, | % m. 37
  e8 fis16 e dis e fis8 g16 fis e fis | % m. 38
  g8 fis e b a'4 | % m. 39
  g8 fis e dis c'4 | % m. 40
  b8 c16 b a g fis8 a16 g fis e | % m. 41
  dis8 a' g fis e dis | % m. 42
  e8 d! c d c b | % m. 43
  c16 c' b a g b a b a g fis a | % m. 44
  g16 a g fis e g fis g fis e dis fis | % m. 45
  e8 fis16 g a8~ a g16 a b8~ | % m. 46
  b8 a g fis16 g a8 dis, | % m. 47
  e4 b8 a4.~ | % m. 48
  a4 a8 g4.~ | % m. 49
  g4 g8 fis4.~ | % m. 50
  fis16 b cis b ais b cis8 d16 cis b cis | % m. 51
  d8 cis b ais g'4 | % m. 52
  fis8 g16 fis e d cis8 e16 d cis b | % m. 53
  ais16 b cis ais d ais e' ais, fis' ais, g'! ais, | % m. 54
  fis'16 ais, b fis' e32 d cis d e16 gis, ais e' d32 cis b cis | % m. 55
  d2.~ | % m. 56
  d2.~ | % m. 57
  d16 fis,32 gis a16 e b' e, cis' e, d' fis, e' g,! | % m. 58
  fis16 g32 a b16 fis cis' fis, d' fis, e' g, fis' a, | % m. 59
  g16 fis g b e g, fis b a cis d8~ | % m. 60
  d8 cis4 d8 e16 d cis d | % m. 61
  e8 fis16 e d e fis8 g16 fis e d | % m. 62
  cis8 b'4~ b16 a b a g fis | % m. 63
  e2.~ \trill | % m. 64
  e16 a32 g fis16 a e a d, a' cis, a' b, a' | % m. 65
  g16 fis e g d g cis, g' b, g' a, g' | % m. 66
  fis16 e d fis c! fis b, fis' a, fis' g, fis' | % m. 67
  e16 d cis e b e a, e' g, e' fis, e' | % m. 68
  d16 fis, e g d' cis d4.~ \trill | % m. 69
  d2.~ | % m. 70
  d8 c4~ c8 b4~ | % m. 71
  b8 a4~ a16 g a g fis g | % m. 72
  fis16 d32 e fis16 d g d a' d, b' d, c' d, | % m. 73
  b'16 g' f! e d c b a g f! e d | % m. 74
  c16 e' d c b a gis fis! e d c b | % m. 75
  a16 b cis a d a e' a, fis' a, g' a, | % m. 76
  fis'8 b g e a fis | % m. 77
  g4.~ g16 a a8. \prall g32 a | % m. 78
  b8 c16 b a b c8 d16 c b c | % m. 79
  d8 c b a g'4 | % m. 80
  fis8 e d cis bes'4 | % m. 81
  a16 g fis e d8 r16 g8 c,16 g' fis | % m. 82
  g16 fis32 e d16 g c, g' b,8 g' a, | % m. 83
  b16 d32 c b16 d a d g,8 c4~ | % m. 84
  c32 g a b c16 g d' g, e'32 b c d e16 c fis! c | % m. 85
  <<
    {g'4~ g16 fis g4. }
    \\
    { r16 \stemDown d e c~ c8 \noBeam d4. }>> \bar "|." | % m. 86
  \override Staff.RehearsalMark #'break-visibility = #begin-of-line-invisible \mark\markup{\musicglyph #"scripts.ufermata"}
 
}

mezzo = \relative c' {
  \global
  
  R2. | % m. 1
  R2. | % m. 2
  R2. | % m. 3
  R2. | % m. 4
  d8 e16 d cis d e8 fis16 e d e | % m. 5
  fis8 e d a g'4 | % m. 6
  fis8 e d cis b'4 | % m. 7
  a8 b16 a g fis e8 g16 fis e d | % m. 8
  cis8 g' fis e d cis | % m. 9
  d16 e fis d g d a' d, b' d, c'! d, | % m. 10
  b'4. c~ | % m. 11
  c8 fis, g~ g8 fis16 g a fis | % m. 12
  g16 fis g a b g a g a b c a | % m. 13
  b8 c16 b a g c4.~ | % m. 14
  c16 b a c g c fis, c' e, c' d, c' | % m. 15
  b8 f! e d c b | % m. 16
  r8 r g' fis4.~ | % m. 17
  fis4 fis8 e4.~ | % m. 18
  e4 e8 d4. | % m. 19
  d8 c16 d e d c8 b16 c d c | % m. 20
  b8 c d g a,4 | % m. 21
  b8 c d e fis,4 | % m. 22
  g8 g16 a b cis d8 b16 cis d e | % m. 23
  fis8 e dis e d! cis | % m. 24
  d8 cis16 b a c! b c b a g b | % m. 25
  \clef bass a16 b a g fis a g a g fis e g | % m. 26
  fis16 g fis e d e fis g a b c d | % m. 27
  fis,8 \clef treble a' g~ g g fis | % m. 28
  g8 r r r4. | % m. 29
  R2. | % m. 30
  r8 r d cis r r | % m. 31
  r8 r cis b r r | % m. 32
  r8 r b a r r | % m. 33
  R2. | % m. 34
  R2. | % m. 35
  R2. | % m. 36
  R2. | % m. 37
  R2. | % m. 38
  R2. | % m. 39
  e'16 dis e fis g e fis e fis g a fis | % m. 40
  g8 a16 g fis e a4.~ | % m. 41
  a16 g fis a e a dis, a' cis, a' b, a' | % m. 42
  g8 fis16 g a g fis8 e16 fis g fis | % m. 43
  e8 fis g c dis,4 | % m. 44
  e8 fis g a b,4 | % m. 45
  c16 b a b c d e dis e fis g a | % m. 46
  b4.~ b8 c fis, | % m. 47
  e16 fis g4~ g16 fis e d cis e | % m. 48
  d16 e fis4~ fis16 e d cis b d | % m. 49
  cis16 d e4~ e16 d cis b ais cis | % m. 50
  b8 r r r4. | % m. 51
  R2. | % m. 52
  R2. | % m. 53
  R2. | % m. 54
  R2. | % m. 55
  b16 cis d b e b fis' b, gis' b, a' b, | % m. 56
  gis'8 e fis gis a b | % m. 57
  e,8. d16 cis b \clef bass a b a g! fis e | % m. 58
  d8 r r r4. | % m. 59
  r4. d8 e16 d cis d | % m. 60
  e8 fis16 e d e fis8 e d | % m. 61
  cis8 b'4 a8 b16 a g fis | % m. 62
  e8 g16 fis e d cis4 d8~ | % m. 63
  d16 g32 fis e16 g d g cis, g' b, g' a, g' | % m. 64
  fis16 g a4~ a16 g fis e d fis | % m. 65
  e16 fis g4~ g16 fis e d cis e | % m. 66
  d16 e fis4~ fis16 b, cis d e fis | % m. 67
  g16 a g fis e d cis8 r r | % m. 68
  R2. | % m. 69
  r4. r16 \clef treble g'' f! e d f | % m. 70
  e16 f! e d c e d e d c b d | % m. 71
  c16 d c b a c b4. | % m. 72
  a8 r r r4. | % m. 73
  R2. | % m. 74
  R2. | % m. 75
  R2. | % m. 76
  d8 c!16 d e d c8 b16 c d c | % m. 77
  b8 c d e fis,4 | % m. 78
  g8 a'16 g fis g a8 b16 a g a | % m. 79
  b8 a g~ g d' cis | % m. 80
  d8 g, fis e cis'4 | % m. 81
  c!4~ c16 b32 a b8 a4 | % m. 82
  b8 b a g4 fis8 | % m. 83
  g8 g f! e4 es8 | % m. 84
  d8 e! <f! b> <g c > g a | % m. 85
  b4 a8 b4. \bar "|." | % m. 86  
  
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
  g8 a16 g fis g a8 b16 a g a | % m. 11
  b8 a g d c'4 | % m. 12
  b8 a g fis e'4| % m. 13
  d8 e16 d c b a8 c16 b a g | % m. 14
  fis4 e8 d16 e d c b a | % m. 15
  g16 a b g c g d' g, e' g, f'! g, | % m. 16
  e'16 d c e b e a, e' g, e' fis, e' | % m. 17
  d16 c b d a d g, d' fis, d' e, d' | % m. 18
  c16 b a c g c fis, c' e, c' d, c' | % m. 19
  b8 a g a g fis | % m. 20
  g16 g' f! e d f e f e d c e | % m. 21
  d16 e d c b d c d c b a c | % m. 22
  b16 c b a g a b cis d e fis g | % m. 23
  fis8 b b, e a a, | % m. 24
  d8 e fis~ fis g cis, | % m. 25
  d8 r r r4. | % m. 26
  R2. | % m. 27
  d8 c16 d e d c8 b16 c d c | % m. 28
  b8 c d g a,4 | % m. 29
  b8 c d e fis,4 | % m. 30
  g16 a b4~ b16 a g fis e g | % m. 31
  fis16 g a4~ a16 g fis e d fis | % m. 32
  e16 fis g4~ g16 fis e d cis e | % m. 33
  d16 e fis d g d a' d, b' d, c' d, | % m. 34
  b'16 g' f! e d c b a g f! e d | % m. 35
  c16 d e fis! gis a b c d e fis! gis | % m. 36
  a16 g! fis a e a dis, a' cis, a' b, a' | % m. 37
  g8 c b a b b, | % m. 38
  e16 fis g e a e b' e, cis' e, dis' e, | % m. 39
  e'8 r r r4. | % m. 40
  R2. | % m. 41
  R2. | % m. 42
  R2. | % m. 43
  R2. | % m. 44
  R2. | % m. 45
  R2. | % m. 46
  r16 b,32 cis dis16 b e' b, fis' b, g' b, a' b, | % m. 47
  g'16 fis e g d g cis, g' b, g' a, g' | % m. 48
  fis16 e d fis cis fis b, fis' a, fis' g, fis' | % m. 49
  e16 d cis e b e ais, e' gis, e' fis, e' | % m. 50
  d16 fis e d cis b ais cis b ais gis fis | % m. 51
  b8 cis16 b ais b cis8 d16 cis b cis | % m. 52
  d8 cis b ais g'4 | % m. 53
  fis16 gis ais fis b fis cis' fis, d' fis, e' fis, | % m. 54
  d'16 fis, g d' cis32 b ais b cis16 e, fis cis' b32 ais gis ais | % m. 55
  b8 r r r r b, | % m. 56
  e16 fis gis e a e b' e, cis' e, d' e, | % m. 57
  cis'8 r r r r cis, | % m. 58
  d8. e16 d cis b cis b a g fis | % m. 59
  e8 d' cis d4 b8 | % m. 60
  g8 a4 d8 cis b | % m. 61
  a2.~ | % m. 62
  a2.~ | % m. 63
  a4.~ a4 r8 | % m. 64
  r8 r16 e' d cis b4 r8 | % m. 65
  r8 r16 d cis b a4 r8 | % m. 66
  r8 r16 c! b a g8 r r | % m. 67
  r4 r8 r16 a b cis d e | % m. 68
  fis16 g a8 a, d c!16 d e d | % m. 69
  c8 b16 c d c b8 c d | % m. 70
  g8 a,4 b8 c d | % m. 71
  e8 fis,4 g8 fis16 g a b | % m. 72
  c,8 \stemDown c''16 b a g fis e d c b a | % m. 73
  g16 a b g c g d' g, e' g, f'! g, | % m. 74
  e'16 e,32 fis! gis16 e a e b' e, c' e, d' e, | % m. 75
  cis'16 a' g fis e d cis b a g fis e | % m. 76
  d2.~ | % m. 77
  d16 d' c d e d c8 b16 c d c | % m. 78
  b8 a16 b c b a8 g16 a b a | % m. 79
  g16 g'32 fis e16 g d g cis, a' b, a' a, a' | % m. 80
  d,16 c! bes d a d g, e' fis, e' e, e' | % m. 81
  fis,8 d g e c d | % m. 82
  << 
   { s2. | d'8 g, b c32 b c d e16 c fis c | g'2.~ | g2. } 
   \\
   { \stemUp g,4.~ g16 g32 a b16 g c g~ | \stemDown g8 g4~ g4.~ | g2.~ | g2. } >> \bar "|." | % m. 83-86
  \override Staff.RehearsalMark #'direction = #DOWN \mark\markup{\musicglyph #"scripts.dfermata"}  
  
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
    
    \new Staff = "bass" \with {
      \consists "Mark_engraver"}
        { \clef bass \bass }
        
  >>
  
\layout {
  indent = 0.0
  }

}
