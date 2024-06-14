\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue XV"
  subtitle = "in 3 voices"
  opus = "BWV 884"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key g \major
  \numericTimeSignature
  \time 3/8
}

soprano = \relative c'' {
  \global
  
  r16 d b d g d | % m. 1
  b16 d g, d' b d | % m. 2
  e16 c a c fis, a | % m. 3
  d16 b g b e, g | % m. 4
  c16 a fis a d, fis | % m. 5
  b16 a g a b c | % m. 6
  d16 a b cis d e | % m. 7
  fis16 e d e fis g | % m. 8
  a8 r a | % m. 9
  d,8 g4~ | % m. 10
  g8 fis d | % m. 11
  b8 e4~ | % m. 12
  e16 g fis e d e | % m. 13
  fis16 a g fis e fis | % m. 14
  g8 r r | % m. 15
  r8 b4~ | % m. 16
  b16 e, fis a8.~ | % m. 17
  a16 d, e g8.~ | % m. 18
  g16 c, d fis8.~ | % m. 19
  fis8 e4~ | % m. 20
  e8 d4~ | % m. 21
  d8 cis16 b cis8 | % m. 22
  d8 r r | % m. 23
  r8 a' c,~ | % m. 24
  c16 d b d g d | % m. 25
  b16 d g, d' b d~ | % m. 26
  d8 r r | % m. 27
  r8 b' d,~ | % m. 28
  d16 e c e a e | % m. 29
  c16 e a, e' c e | % m. 30
  fis8 r r | % m. 31
  r8 a4~ | % m. 32
  a8 b16 a g fis | % m. 33
  e16 fis g4~ | % m. 34
  g16 c, d fis8.~ | % m. 35
  fis16 b, c e8.~ | % m. 36
  e16 a, b dis8.~ | % m. 37
  dis8 e16 dis e8~ | % m. 38
  e8 d!16 cis d e | % m. 39
  fis16 e d fis b fis | % m. 40
  d16 fis b, fis' d fis | % m. 41
  g16 e cis e a, cis | % m. 42
  fis16 d b d g, b | % m. 43
  e16 cis ais cis fis, ais | % m. 44
  d16 fis b, d fis d | % m. 45
  b16 d fis, b d8~ | % m. 46
  d4.~ | % m. 47
  d4.~ | % m. 48
  d8 c c | % m. 49
  c4.~ \trill | % m. 50
  c8 c c | % m. 51
  c4.~ | % m. 52
  c8 b b | % m. 53
  b4~ b16 b~ | % m. 54
  b16 b a fis! g e | % m. 55
  fis16 a d, fis a fis | % m. 56
  d16 fis a, d fis c | % m. 57
  b16 d b d g d | % m. 58
  \clef bass b16 d g, b d b | % m. 59
  g16 b e, g bes d, | % m. 60
  r16 bes' a g e' g, | % m. 61
  <<
    { fis16 \prall e d8 s | c32 a b c d e fis g a fis g a | \clef treble b32 cis d e fis d e fis g a b c! }
    \\
    { r8 r16 d,,,32 e fis g a b | s4. | s4. }
  >> | % m. 62 - 64
  d''4.~ | % m. 65
  d4.~ | % m. 66
  d8 c4~ | % m. 67
  c8 b4~ | % m. 68
  b8 a16 c fis a~ | % m. 69
  a16 g8 fis16 g fis32 e | % m. 70
  d32 c b a g b a g fis e d c | % m. 71
  \grace c8 (b4.) \fermata \bar "|." | % m. 72
    
}

mezzo = \relative c'' {
  \global
  
  R4. | % m. 1
  R4. | % m. 2
  R4. | % m. 3
  R4. | % m. 4
  R4. | % m. 5
  R4. | % m. 6
  R4. | % m. 7
  r16 g fis a d a | % m. 8
  fis16 a d, a' fis a | % m. 9
  b16 g e g cis, e | % m. 10
  a16 fis d fis b, d | % m. 11
  g16 e cis e a, cis | % m. 12
  fis16 e d e fis g | % m. 13
  a16 d, e fis g a | % m. 14
  b16 a g a b c | % m. 15
  d4 d8 | % m. 16
  c4 c8 | % m. 17
  b4 b8 | % m. 18
  a4 a8 | % m. 19
  g4 g8 | % m. 20
  fis4 fis8 | % m. 21
  e4 g8 | % m. 22
  fis8 r r | % m. 23
  r8 c' a~ | % m. 24
  a8 g r | % m. 25
  r8 d f!~ | % m. 26
  f8 e r | % m. 27
  r8 d' b~ | % m. 28
  b8 a r | % m. 29
 <<
  { r8 c, <a' c>~ | c8 [b] r }
  \\
  { s4 \hideNotes a8~ | \unHideNotes a8 r r }
 >> | % m. 30 - 31
  r8 dis fis | % m. 32
  b,8 e8. dis16 | % m. 33
  e4 b8 | % m. 34
  a4 a8 | % m. 35
  g4 g8 | % m. 36
  fis4 a8~ | % m. 37
  a8 g8. g16 | % m. 38
  fis8 r r | % m. 39
  R4. | % m. 40
  r8 r fis | % m. 41
  e4 e8 | % m. 42
  d4 d8 | % m. 43
  cis4 cis8 | % m. 44
  b4 r8 | % m. 45
  r8 r r16 a' | % m. 46
  gis16 b e, gis b gis | % m. 47
  e16 gis b, e gis8~ | % m. 48
  gis8 a a | % m. 49
  a4.~ | % m. 50
  a8 fis fis | % m. 51
  fis4.~ \trill | % m. 52
  fis8 g g | % m. 53
  g8 f!16 e d8 | % m. 54
  c4.~ | % m. 55
  c8 \clef bass fis, fis | % m. 56
  fis4~ \trill fis16 e32 fis | % m. 57
  g8 g g | % m. 58
  g8 r r | % m. 59
  r8 <g, bes> q | % m. 60
  cis4~ \trill cis16 b!32 cis | % m. 61
  d8 r r | % m. 62
  R4. | % m. 63
  R4. | % m. 64
  r16 d' b d g d | % m. 65
  b16 d g, d' b d | % m. 66
  e16 c a c fis, a | % m. 67
  d16 b g b e, g | % m. 68
  c16 a fis a d, fis | % m. 69
  b8 c d | % m. 70
  r8 b a | % m. 71
  g4. \fermata \bar "|." | % m. 72
    
}

bass = \relative c' {
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
  R4. | % m. 14
  r16 d b d g d | % m. 15
  b16 d g, d' b d | % m. 16
  e16 c a c fis, a | % m. 17
  d16 b g b e, g | % m. 18
  c16 a fis a d, fis | % m. 19
  b16 a g a b cis | % m. 20
  d16 cis b a g fis | % m. 21
  g16 e a g fis e | % m. 22
  d16 a' fis a d a | % m. 23
  fis16 a d, a' fis a | % m. 24
  b8 r r | % m. 25
  r8 b a | % m. 26
  gis16 b gis b e b | % m. 27
  gis16 b e, b' gis b | % m. 28
  c8 r r | % m. 29
  r8 fis, e | % m. 30
  dis16 fis dis fis b fis | % m. 31
  dis16 fis b, fis' dis fis | % m. 32
  g16 b g b e b | % m. 33
  g16 b e, b' g b | % m. 34
  c16 a fis a d, fis | % m. 35
  b16 g e g c, e | % m. 36
  a16 fis dis fis b, dis | % m. 37
  g16 fis e fis g a | % m. 38
  b16 fis gis ais b cis | % m. 39
  d16 cis b cis b e | % m. 40
  fis16 e d4~ | % m. 41
  d16 g, a cis8.~ | % m. 42
  cis16 fis, g b8.~ | % m. 43
  b16 e, fis ais8 fis16 | % m. 44
  b8 b, r | % m. 45
  r8 d b | % m. 46
  e8 e, r | % m. 47
  r8 gis' e16 d' | % m. 48
  c16 e a, c e c | % m. 49
  a16 c e, a c g | % m. 50
  fis16 a d, fis a fis | % m. 51
  d16 fis a, d fis c | % m. 52
  b16 d g, b d b | % m. 53
  g16 b d, g b f! | % m. 54
  e16 g c, d e c | % m. 55
  d8 d' d | % m. 56
  d4.~ | % m. 57
  d8 d d | % m. 58
  d4~ \trill d16 cis32 d | % m. 59
  e8 d, d | % m. 60
  d8 r r | % m. 61
  R4. | % m. 62
  R4. | % m. 63
  r8 d' c! | % m. 64
  b8 b' r | % m. 65
  r8 r g | % m. 66
  c,8 r a | % m. 67
  b8 r g | % m. 68
  a8 r d | % m. 69
  g8 a b | % m. 70
  r8 c, d | % m. 71
  g,4. \fermata \bar "|." | % m. 72
   
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
