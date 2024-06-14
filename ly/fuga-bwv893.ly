\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue XIV"
  subtitle = "in 3 voices"
  opus = "BWV 893"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key b \minor
  \numericTimeSignature
  \time 3/8
  \partial 8
}

soprano = \relative c'' {
  \global
  
  r8
  R4. | % m.1
  R4. | % m. 2
  R4. | % m. 3
  R4. | % m. 4
  R4. | % m. 5
  r8 r b | % m. 6
  a8 fis eis | % m. 7
  fis8 gis16 a b cis | % m. 8
  d8 d, d' | % m. 9
  cis8 cis, cis' | % m. 10
  b8 cis16 b a gis | % m. 11
  a16 cis b a gis fis | % m. 12
  g!8 a16 g fis e | % m. 13
  fis16 a g fis e d | % m. 14
  e8 fis16 e d cis | % m. 15
  d16 e fis g fis8~ | % m. 16
  fis4.~ | % m. 17
  fis8 e a~ | % m. 18
  a8 fis d~ | % m. 19
  d8 cis fis~ | % m. 20
  fis8 r gis | % m. 21
  a16 cis b a gis fis | % m. 22
  gis16 ais ais8. \trill gis32 ais | % m. 23
  b16 d cis b a! gis | % m. 24
  a16 b b8. \trill a32 b | % m. 25
  cis4.~ | % m. 26
  cis16 d cis b a gis | % m. 27
  a8 fis eis | % m. 28
  fis4 b8~ | % m. 29
  b8 eis, fis | % m. 30
  d8 cis4~ | % m. 31
  cis4 fis8 | % m. 32
  d8 e fis~ | % m. 33
  fis8 e d | % m. 34
  cis8 r a'' | % m. 35
  fis8 d cis | % m. 36
  d8 e16 fis g a | % m. 37
  b8 b, b' | % m. 38
  a8 a, a' | % m. 39
  g8 a16 g fis e | % m. 40
  fis16 a g fis e d | % m. 41
  cis8 d e~ | % m. 42
  e8 d cis | % m. 43
  b8 a b | % m. 44
  cis8 e d | % m. 45
  cis4.~ | % m. 46
  cis16 cis d, cis' d, b' | % m. 47
  cis,16 b' cis, b' cis, a' | % m. 48
  b,16 a' gis b e, gis | % m. 49
  a8 e fis | % m. 50
  g8 gis ais | % m. 51
  b8 fis gis | % m. 52
  a8 ais bis | % m. 53
  cis4.~ | % m. 54
  cis8 a gis | % m. 55
  fis8 r r | % m. 56
  r16 a' b, a' b, gis' | % m. 57
  a,16 gis' a, gis' a, fis' | % m. 58
  gis,16 fis' eis gis cis, eis | % m. 59
  fis8 cis fis~ | % m. 60
  fis16 e dis cis b8~ | % m. 61
  b8 b e~ | % m. 62
  e16 d cis b a8~ | % m. 63
  a8 a d~ | % m. 64
  d8. d16 cis b | % m. 65
  ais8 b cis | % m. 66
  d4.~ | % m. 67
  d16 cis d e fis g | % m. 68
  ais,4 r8 | % m. 69
  R4. | % m. 70
  R4. | % m. 71
  R4. | % m. 72
  r16 g' a, g' a, fis' | % m. 73
  g,16 fis' g, fis' g, e' | % m. 74
  fis,16 e' dis fis b, dis | % m. 75
  e16 dis e g b d,! | % m. 76
  cis16 b cis e a, cis | % m. 77
  d16 cis d fis a cis, | % m. 78
  b16 ais b d cis b | % m. 79
  ais16 b cis d e8~ | % m. 80
  e8 cis fis | % m. 81
  d8 b ais | % m. 82
  b8 cis16 d e fis | % m. 83
  g8 g, g' | % m. 84
  fis8 fis, fis' | % m. 85
  e8 fis16 e d cis | % m. 86
  d8 e fis~ | % m. 87
  fis8 e d | % m. 88
  cis8 d e~ | % m. 89
  e8 d cis | % m. 90
  b4 cis8 | % m. 91
  d8 a b | % m. 92
  c!8 cis dis | % m. 93
  e8 b cis | % m. 94
  d8 dis eis | % m. 95
  fis4 r8 | % m. 96
  R4. | % m. 97
  r8 r r16 fis | % m. 98
  e16 d cis b cis ais | % m. 99
  \grace ais8 (b4.) \bar "|." | % m. 100
    
}

mezzo = \relative c' {
  \global
  
  fis8
  d8 b ais | % m. 1
  b8 cis16 d e fis | % m. 2
  g8 g, g' | % m. 3
  fis8 fis, fis' | % m. 4
  e8 fis16 e d cis | % m. 5
  d16 fis e d cis b | % m. 6
  cis16 b cis d cis b | % m. 7
  \clef bass a8 fis eis | % m. 8
  fis16 gis gis8. \trill fis32 gis | % m. 9
  a16 b a gis fis e | % m. 10
  dis16 eis eis8. \trill dis32 eis | % m. 11
  fis8 gis a | % m. 12
  b16 cis cis8. \trill b32 cis | % m. 13
  d8 ais b~ | % m. 14
  b8 ais16 gis ais8 | % m. 15
  b8 r r16 e | % m. 16
  d8 b ais | % m. 17
  b16 cis cis8. \trill b32 cis | % m. 18
  d16 e d cis b a | % m. 19
  gis16 ais ais8. \trill gis32 ais | % m. 20
  b4 r8 | % m. 21
  R4. | % m. 22
  R4. | % m. 23
  R4. | % m. 24
  R4. | % m. 25
  r8 r cis | % m. 26
  a8 fis eis | % m. 27
  fis8 gis16 a b cis | % m. 28
  d8 d, d' | % m. 29
  cis8 cis, cis' | % m. 30
  b8 cis16 b a gis | % m. 31
  a8 b cis~ | % m. 32
  cis8 b a | % m. 33
  g8 a b~ | % m. 34
  b8 a r | % m. 35
  R4. | % m. 36
  R4. | % m. 37
  r16 fis' g, fis' g, e' | % m. 38
  fis,16 e' fis, e' fis, d' | % m. 39
  e,16 d' cis e a, cis | % m. 40
  \clef treble d16 cis d e fis gis | % m. 41
  a16 gis a b a gis | % m. 42
  fis16 e fis gis a fis | % m. 43
  gis16 e fis e gis e | % m. 44
  a16 e cis' e, b' e, | % m. 45
  a8. gis16 fis e | % m. 46
  d8 a' gis | % m. 47
  r8 gis fis | % m. 48
  r8 e d~ | % m. 49
  d8 cis c!~ | % m. 50
  c8 b e~ | % m. 51
  e8 dis d!~ | % m. 52
  d8 cis fis~ | % m. 53
  fis8 eis cis' | % m. 54
  a8 fis eis | % m. 55
  fis8 gis16 ais b cis | % m. 56
  d8 d, d' | % m. 57
  cis8 cis, cis' | % m. 58
  b8 cis16 b a gis | % m. 59
  a4.~ | % m. 60
  a4~ a16 a | % m. 61
  g!16 fis e8 g~ | % m. 62
  g4~ g16 g | % m. 63
  fis16 e d8 fis~ | % m. 64
  fis16 fis e4~ | % m. 65
  e16 e d cis b ais | % m. 66
  b8 fis' b~ | % m. 67
  b8 b, e~ | % m. 68
  e16 g fis e d cis | % m. 69
  d8 b ais | % m. 70
  b8 cis16 dis e fis | % m. 71
  g8 e dis | % m. 72
  e8 r e | % m. 73
  dis8 r e | % m. 74
  fis4. | % m. 75
  b,8 r e | % m. 76
  a8 r r | % m. 77
  a,8 r d | % m. 78
  g4.~ | % m. 79
  g8 r r16 g | % m. 80
  fis8 ais4 | % m. 81
  b8 r r | % m. 82
  R4. | % m. 83
  r16 d e, d' e, cis' | % m. 84
  d,16 cis' d, cis' d, b' | % m. 85
  cis,8 cis'4~ | % m. 86
  cis8 b a | % m. 87
  g8 a b~ | % m. 88
  b8 a g | % m. 89
  fis8 g a~ | % m. 90
  a8 d, g~ | % m. 91
  g8 fis f!~ | % m. 92
  f8 e a~ | % m. 93
  a8 gis g!~ | % m. 94
  g8 fis b~ | % m. 95
  b8 ais fis | % m. 96
  d8 b ais | % m. 97
  b16 b' ais b cis8 | % m. 98
  r8 fis, e | % m. 99
  \grace e8 (dis4.) \bar "|." | % m. 100
    
}

bass = \relative c {
  \global
  
  r8
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
  r8 r fis | % m. 15
  d8 b ais | % m. 16
  b8 cis16 d e fis | % m. 17
  g8 g, g' | % m. 18
  fis8 fis, fis' | % m. 19
  e8 fis16 e d cis | % m. 20
  d16 fis e d cis b | % m. 21
  cis16 dis dis8. \trill cis32 dis | % m. 22
  e16 g! fis e d! cis | % m. 23
  dis16 eis eis8. \trill dis32 eis | % m. 24
  fis16 a gis fis eis dis | % m. 25
  eis16 gis fis eis dis cis | % m. 26
  fis8 a, cis | % m. 27
  fis,8 r r | % m. 28
  r16 a' b, a' b, gis' | % m. 29
  a,16 gis' a, gis' a, fis' | % m. 30
  gis,16 fis' eis gis cis, eis | % m. 31
  fis,16 fis' gis, fis' a, fis' | % m. 32
  b,16 fis' cis fis d fis | % m. 33
  e,16 e' fis, e' g, e' | % m. 34
  a,16 e' b e cis e | % m. 35
  d16 e fis g a g | % m. 36
  fis16 a g fis e d | % m. 37
  g8 r e | % m. 38
  fis8 r d | % m. 39
  e8 a4 | % m. 40
  d,8 r b' | % m. 41
  fis'8 r cis | % m. 42
  d8 r dis | % m. 43
  e8 r e, | % m. 44
  cis8 a gis | % m. 45
  a8 b16 cis d e | % m. 46
  fis8 fis, fis' | % m. 47
  e8 e, e' | % m. 48
  d8 e16 d cis b | % m. 49
  cis16 e a e dis a' | % m. 50
  e16 g fis e d! cis | % m. 51
  d! fis b fis eis b' | % m. 52
  fis16 a gis fis eis dis | % m. 53
  cis16 d! cis b a gis | % m. 54
  fis16 fis' gis a b cis | % m. 55
  d16 e d cis b ais | % m. 56
  b8 r e, | % m. 57
  a8 r d~ | % m. 58
  d8 cis4~ | % m. 59
  cis16 b a gis fis e | % m. 60
  dis8 b'4~ | % m. 61
  b16 a g fis e d | % m. 62
  cis8 a'4~ | % m. 63
  a16 g fis e d cis | % m. 64
  b8 g'4~ | % m. 65
  g8 fis4~ | % m. 66
  fis16 e d cis b a | % m. 67
  g8 g'4 | % m. 68
  fis4 r8 | % m. 69
  r16 b, cis d e fis | % m. 70
  g8 e dis | % m. 71
  e8 fis16 g a b | % m. 72
  c!8 c,! c' | % m. 73
  b8 b, b' | % m. 74
  a8 b16 a g fis | % m. 75
  g8 g, g'~ | % m. 76
  g8 a16 g fis e | % m. 77
  fis8 fis, fis'~ | % m. 78
  fis8 e d | % m. 79
  cis16 d e fis g b, | % m. 80
  ais16 g' fis e d cis | % m. 81
  b16 cis d e fis e | % m. 82
  d16 fis e d cis b | % m. 83
  e8 r a | % m. 84
  d,8 r g | % m. 85
  cis,16 b' ais cis fis, ais | % m. 86
  b,16 b' cis, b' d, b' | % m. 87
  e,16 b' fis b g b | % m. 88
  a,16 a' b, a' cis, a' | % m. 89
  d,16 a' e a fis a | % m. 90
  g16 b a g fis e | % m. 91
  fis16 a d a gis d' | % m. 92
  a16 c! b a g! fis | % m. 93
  g!16 b e b ais e' | % m. 94
  b16 d cis b ais gis | % m. 95
  fis16 g! fis e d cis | % m. 96
  b16 cis d e fis e | % m. 97
  d8 b ais | % m. 98
  b8 d, fis | % m. 99
  b,4. \bar "|." | % m. 100
    
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
