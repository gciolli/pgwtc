\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

#(ly:expect-warning "Previous mark event here")
#(ly:expect-warning "Two simultaneous mark events, junking this one")

\header {
  title = "Fugue XIV"
  subtitle = "in 4 voices"
  opus = "BWV 869"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key b \minor
  \time 4/4
  \tempo "Largo"
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
  r8 b a fis d' (b) e! (dis) | % m. 13
  a8 (gis) f'! (e) b (ais) g'! (fis) | % m. 14
  eis8 fis d b cis2~ | % m. 15
  cis16 cis d e fis g b, ais b2~ | % m. 16
  b16 eis, fis gis a b cis fis, d' e! fis8~ fis e16 d | % m. 17
  cis16 d e8~ e d16 cis b cis d8~ d cis16 b | % m. 18
  a4 r8 fis'~ fis e16 d cis d e8~ | % m. 19
  e8 d16 cis b cis d8~ d cis16 b ais b cis8~ | % m. 20
  cis8 b16 ais b4~ b16 ais b d cis4 | % m. 21
  b4 a gis fis~ | % m. 22
  fis8 eis fis4~ fis16 gis a b! cis d fis, eis | % m. 23
  fis4 fis'~ fis8 e dis fis | % m. 24
  g2~ g8 fis eis gis | % m. 25
  cis,8 d16 e! d e fis8~ fis e16 d cis d e8~ | % m. 26
  e8 d16 cis b cis d8~ d cis16 b ais b cis8~ | % m. 27
  cis8 b r b'~ b a16 g fis g a8~ | % m. 28
  a8 g16 fis e fis g8~ g fis16 e dis e fis8~ | % m. 29
  fis8 e16 dis e fis g b a8 g fis4 | % m. 30
  e4 d cis b~ | % m. 31
  b8 b16 ais b4~ b16 cis d e fis g b, ais | % m. 32
  d2 cis4. dis8 | % m. 33
  eis4 r8 fis~ fis16 eis fis a gis4 | % m. 34
  r8 fis d b g' fis b ais | % m. 35
  e8 dis g4~ g16 fis gis ais b g! fis eis | % m. 36
  fis4 f!~ f16 e fis gis a b e, dis | % m. 37
  e16 fis, g! a b cis d fis e cis d fis g e fis ais | % m. 38
  b4 a gis fis~ | % m. 39
  fis16 eis fis eis fis4~ fis16 a, b cis d e! fis, eis | % m. 40
  fis8 fis' d b g' fis b ais | % m. 41
  e8 dis r e~ e16 dis e g fis4 | % m. 42
  e16 d! cis! b a b cis e d b cis e fis d e gis | % m. 43
  a16 g! fis e d cis b a g e fis a b g a cis | % m. 44
  d16 e d e fis g a fis d f! e! d c! b c d | % m. 45
  c!16 b8. a4~ a8 d16 e fis g a, gis | % m. 46
  e'16 fis g,! fis d' e fis, e cis' b cis d e fis g e | % m. 47
  a2 g4 f! | % m. 48
  e4 d~ d16 cis d cis d4~ | % m. 49
  d16 e fis g a b d, cis d2~ | % m. 50
  d4 cis8 fis~ fis16 b, cis d! e fis b, ais | % m. 51
  d4 cis8 b cis2 | % m. 52
  r16 fis, gis a b cis fis, eis! a4 r | % m. 53
  r4 gis' fis e | % m. 54
  dis4 cis~ cis8 bis cis4~ | % m. 55
  cis16 dis e fis gis a cis, bis e2~ | % m. 56
  e16 e fis gis a b dis, cis dis4 r | % m. 57
  r4 fis e d! | % m. 58
  cis4 b~ b16 ais b ais b4~ | % m. 59
  b16 cis d e fis g b, ais d4~ d16 cis d cis | % m. 60
  e16 d fis e g4~ g8. fis16 a!4~ | % m. 61
  a8. gis16 b!8 a gis4~ gis16 eis! fis8~ | % m. 62
  fis16 fis gis a b gis fis eis a2~ | % m. 63
  a2~ a16 fis g a b c! fis, e | % m. 64
  fis4~ fis16 fis d b g' a! b8~ b a16 g | % m. 65
  fis16 g a8~ a g16 fis e fis g8~ g fis16 e | % m. 66
  d4~ d16 e fis8~ fis e16 d! cis d e8~ | % m. 67
  e8 d16 cis b cis d8~ d cis16 b ais b cis8~ | % m. 68
  cis16 fis, gis ais b cis d fis e cis d fis g! e fis ais | % m. 69
  b16 a! g fis e d! c! b a fis g b c a b dis | % m. 70
  e4 d! cis b~ | % m. 71
  b16 ais b ais b4~ b16 cis d e fis g b, ais | % m. 72
  b16 d e fis g a c,! b c e fis g ais b d,! cis | % m. 73
  d2 cis~ | % m. 74
  cis4 c!16 ais b8~ b4 ais | % m. 75
  b1 \fermata \bar "|." 
  \override Staff.RehearsalMark #'break-visibility = #begin-of-line-invisible \mark\markup{\musicglyph #"scripts.ufermata"} | % m. 76
    
}

alto = \relative c' {
  \global
  
  r8 fis d b g' (fis) b (ais) | % m. 1
  e8 (dis) c'! (b) fis (eis) d'! (cis) | % m. 2
  bis8 cis a fis gis2 \trill | % m. 3
  fis8. eis16 fis gis a cis b gis a c! fis4 | % m. 4
  e4 d cis! b~ | % m. 5
  b8 ais b4~ b16 cis d e! fis g b, ais | % m. 6
  d2~ d8 c! b dis | % m. 7
  e2~ e8 d cis! eis | % m. 8
  fis8. g16 fis e d b cis e d b fis8 fis'~ | % m. 9
  fis8. g16 fis e dis cis b cis32 d! cis8~ cis16 b a d | % m. 10
  gis,4 a b!2~ | % m. 11
  b4 a g2 | % m. 12
  fis2 eis8 dis r dis' | % m. 13
  dis8 e r4 r8 e d!16 cis d b | % m. 14
  cis4 b8 fis g!2~ | % m. 15
  g8 cis,~ cis4 fis gis~ | % m. 16
  gis4 fis8 r r2 | % m. 17
  R1 | % m. 18
  r8 fis d b r2 | % m. 19
  R1 | % m. 20
  r8 fis' d b g' fis b ais! | % m. 21
  e dis c'! b fis eis d'! cis | % m. 22
  bis cis a fis g2 | % m. 23
  a2~ a8 c! b dis! | % m. 24
  e2~ e8 d cis eis! | % m. 25
  fis4 r8 b,16 a! g a b8~ b a16 g | % m. 26
  fis16 g a8~ a g16 fis e fis g8~ g fis16 e | % m. 27
  d16 e fis8~ fis e16 d c! d e8~ e d16 c | % m. 28
  b16 c! d8~ d c16 b a b c8~ c b16 a | % m. 29
  g4 r8 e'16 (dis) dis (e fis g) a g a b | % m. 30
  c!16 a b8~ b16 a gis fis e g! fis ais b cis d e | % m. 31
  d16 cis8. r16 a! g fis e!2 | % m. 32
  r16 fis gis ais b cis fis, eis a!2 | % m. 33
  gis8 cis a fis d' cis fis eis | % m. 34
  b8 ais r b~ b16 ais b d cis4~ | % m. 35
  cis8 b e ais, b2~ | % m. 36
  b16 ais b cis d e a,! gis a2~ | % m. 37
  a8 g! fis4 r2 | % m. 38
  r16 fis' a! g fis e dis cis b d! cis b a gis a fis | % m. 39
  gis4 r16 e'! d cis b! a gis a b4~ | % m. 40
  b8 ais r b~ b16 ais b d cis4 | % m. 41
  r8 b g e c'! b e dis | % m. 42
  a gis a4~ a8 gis cis b | % m. 43
  e8 r r fis, d4 r8 g | % m. 44
  a8 d c!4 b a~ | % m. 45
  a8 a16 gis a g! fis e d! cis! b cis d4 | % m. 46
  cis4 b a2 | % m. 47
  R1 | % m. 48
  R1 | % m. 49
  R1 | % m. 50
  r16 e' fis g a b e, dis g2~ | % m. 51
  g8 fis eis gis~ gis16 eis fis gis a b d,! cis | % m. 52
  dis4 eis8 gis!~ gis16 gis fis eis fis gis a cis | % m. 53
  b16 gis a cis b d cis b cis d e! d cis b ais gis | % m. 54
  fis16 a! gis bis cis dis! e fis e dis r a! gis b! a gis | % m. 55
  fis2 gis~ | % m. 56
  gis16 cis, dis e fis gis a b a fis gis a b cis dis! e | % m. 57
  fis16 dis e gis fis cis b a b cis d! cis b a gis fis | % m. 58
  e16 g! fis e d cis d b cis4 r16 a'! g fis | % m. 59
  e4 r8 cis'~ cis16 cis b ais! b4 | % m. 60
  cis8 d r16 e d cis b ais b8 r16 fis' e dis | % m. 61
  cis16 bis cis8 r16 gis'!8 fis16~ fis fis eis gis cis,8. d16 | % m. 62
  b2 cis~ | % m. 63
  cis16 fis, g! a b c! e, dis e2~ | % m. 64
  e16 e d! cis d8 r r e'16 d cis d e8~ | % m. 65
  e8 d16 cis b cis d8~ d cis16 b ais b cis8~ | % m. 66
  cis4 b8 a g16 a b8~ b8 a16 g | % m. 67
  fis16 g a8~ a g16 fis e fis g8~ g fis16 e | % m. 68
  d4 b'~ b8 ais! d cis | % m. 69
  fis8 r r4 r r8 b,~ | % m. 70
  b16 cis! b a gis4 r16 g! fis8~ fis16 e d e | % m. 71
  cis4 r16 fis e! d cis2 | % m. 72
  b4 r8 fis' e4 r | % m. 73
  r8 fis d b g'! fis b ais | % m. 74
  e8 dis r gis fis eis fis8. e!16 | % m. 75
  dis1 \fermata \bar "|." | % m. 76
   
}

tenor = \relative c' {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  r8 b a fis d' (b) e! (dis) | % m. 4
  a8 (gis) f'! (e) b (ais) g'! (fis) | % m. 5
  eis8 fis d b cis2 | % m. 6
  b16 fis' gis ais b cis fis, eis a!2~ | % m. 7
  a8 g! fis ais b2~ | % m. 8
  b8 ais b4 r cis | % m. 9
  b4 a gis fis~ | % m. 10
  fis8 eis fis4~ fis16 gis a b cis d fis, eis | % m. 11
  fis2~ fis4 e~ | % m. 12
  e8. \clef bass d16 cis b a fis gis b a fis c'!8 b16 a | % m. 13
  b2 e,4 b'8 a! | % m. 14
  gis8 fis fis' d e!2~ | % m. 15
  e8 cis ais fis d'2 | % m. 16
  cis2~ cis8 b16 a gis a b8~ | % m. 17
  b8 a16 gis fis gis a8~ a gis16 fis eis fis gis8 | % m. 18
  fis16 \clef treble b' cis8~ cis b16 a g a b8~ b a16 g | % m. 19
  fis16 g a8~ a g16 fis e fis g8~ g fis16 e | % m. 20
  d4 r r2 | % m. 21
  R1 | % m. 22
  R1 | % m. 23
  R1 | % m. 24
  R1 | % m. 25
  R1 | % m. 26
  R1 | % m. 27
  r8 b g e r2 | % m. 28
  R1 | % m. 29
  r8 b' g e c'! b e dis | % m. 30
  a8 gis f'! e b ais g'! fis | % m. 31
  eis8 fis d b cis2 | % m. 32
  b2 r16 cis dis eis fis gis cis, bis | % m. 33
  cis16 b! a gis fis gis a cis b gis a cis d b cis eis | % m. 34
  fis16 e! d cis b cis d fis e cis d fis g e fis ais | % m. 35
  b16 a! g fis e d! e cis d2~ | % m. 36
  d8 cis b d c!2~ | % m. 37
  c8 b r b~ b16 ais b d cis4 | % m. 38
  fis,4 r r2 | % m. 39
  R1 | % m. 40
  R1 | % m. 41
  R1 | % m. 42
  R1 | % m. 43
  \clef bass r8 a fis d b' a d cis | % m. 44
  g8 fis es' d a gis f'! e! | % m. 45
  dis8 e cis! a b2~ | % m. 46
  b8 a4 g8~ g fis e cis | % m. 47
  \clef treble r16 g''' fis e d c! b c d e d c b cis d b | % m. 48
  g16 bes a g f! e f d e4 r16 c'! b! a | % m. 49
  g16 fis e d \clef bass cis! b a g fis a b cis d e a, gis | % m. 50
  a2 b8 ais16 b cis8 e, | % m. 51
  b'8 r r4 r16 cis, dis eis fis gis a eis | % m. 52
  fis4 gis r8 cis a fis | % m. 53
  \clef treble d'8 cis fis eis b ais g'! fis | % m. 54
  cis8 bis a' gis fisis gis e cis | % m. 55
  dis2~ dis16 \clef bass gis, ais bis cis dis fis, e | % m. 56
  fis4 r r2 | % m. 57
  R1 | % m. 58
  R1 | % m. 59
  r2 r8 \clef treble fis' d b | % m. 60
  g'8 fis b ais e dis c'! b | % m. 61
  fis8 eis d'! cis bis cis a fis | % m. 62
  gis2~ gis16 cis, dis eis fis gis b, a | % m. 63
  b1 | % m. 64
  ais8 fis b d e g a! cis, | % m. 65
  d8 fis g b, cis e fis ais, | % m. 66
  b16 \clef bass fis g a! b c! e, dis e8 g a cis | % m. 67
  d8 fis, g b cis e, fis ais | % m. 68
  b8 fis d b g'! fis b ais | % m. 69
  e8 dis e2 fis4 | % m. 70
  e4 r16 b' cis d e8. e16 d cis b a! | % m. 71
  gis8 fis~ fis16 a! g! fis e!2 | % m. 72
  fis4 r8 a g4 r8 b | % m. 73
  ais16 fis gis ais b cis d b~ b b ais b cis d e cis | % m. 74
  fis4 r8 d! cis2 | % m. 75
  b1 \fermata \bar "|." | % m. 76
   
}

bass = \relative c {
  \global
  
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  R1 | % m. 5
  R1 | % m. 6
  R1 | % m. 7
  R1 | % m. 8
  r8 fis d b g' (fis) b (ais) | % m. 9
  e8 (dis) c'! (b) fis (eis) d'! (cis) | % m. 10
  bis8 cis a fis gis2 | % m. 11
  fis16 fis g! a b c! e, dis e dis g e ais b d,! cis! | % m. 12
  d8 b fis'4~ fis fis | % m. 13
  e4 d! cis b~ | % m. 14
  b8 ais b4~ b16 cis d e fis g! b, ais | % m. 15
  d2~ d16 ais b cis d e fis, eis | % m. 16
  fis4 fis'8 a b d, e gis | % m. 17
  a8 cis, d fis gis! b, cis eis | % m. 18
  fis8 a, b d e g, a cis | % m. 19
  d8 fis, g b cis e, fis ais | % m. 20
  b4~ b16 cis d fis e cis d fis g e fis ais | % m. 21
  b16 g fis e dis8. cis16 d! e d cis b a gis fis | % m. 22
  gis16 b! a gis fis a d cis b8 gis cis cis, | % m. 23
  fis'16 g! a b c! d e, dis g2~ | % m. 24
  g16 b, cis d! e fis b, ais b2~ | % m. 25
  b8 ais b d e g, a! cis | % m. 26
  d8 fis, g b cis e, fis ais | % m. 27
  b8 d e g a c! d fis, | % m. 28
  g8 b c! e, fis a b dis, | % m. 29
  e4 r r2 | % m. 30
  R1 | % m. 31
  R1 | % m. 32
  R1 | % m. 33
  R1 | % m. 34
  R1 | % m. 35
  R1 | % m. 36
  R1 | % m. 37
  r8 e d b g' fis b ais! | % m. 38
  e8 dis c'! b fis eis d'! cis | % m. 39
  bis8 cis a fis gis2 | % m. 40
  fis16 e d cis b cis d fis e cis d fis g e fis ais | % m. 41
  b16 a! g fis e fis g b a fis g b c! a b dis | % m. 42
  e8 e, cis a fis' e a gis | % m. 43
  d8 cis d2.~ | % m. 44
  d4 r r2 | % m. 45
  R1 | % m. 46
  r2 r8 d cis a | % m. 47
  fis'8 d g fis c! b a' gis | % m. 48
  d8 cis bes' a gis a fis! d | % m. 49
  e2 d4 r8 b | % m. 50
  a4 r8 fis' e4 r8 cis | % m. 51
  b16 fis gis a b cis fis, eis a2~ | % m. 52
  a4 gis fis8 r r4 | % m. 53
  R1 | % m. 54
  R1 | % m. 55
  R1 | % m. 56
  r2 r8 b gis e | % m. 57
  cis' b e dis a gis f'! e | % m. 58
  b8 ais g' fis! eis fis d b | % m. 59
  cis2 b8 r r4 | % m. 60
  R1 | % m. 61
  R1 | % m. 62
  R1 | % m. 63
  R1 | % m. 64
  R1 | % m. 65
  R1 | % m. 66
  R1 | % m. 67
  R1 | % m. 68
  R1 | % m. 69
  r8 b g e c'! b e dis! | % m. 70
  a8 gis f'! e b ais g'! fis | % m. 71
  eis8 fis d b cis2 | % m. 72
  d4 r8 dis e4 r8 eis | % m. 73
  <<
    { s1 | s2 r16 b cis d! e! cis fis8~ | fis1 }
    \\
    { fis1~ | fis16 ais, b cis dis eis fis, eis fis2 | b1 \fermata \bar "|." } >> 
  \override Staff.RehearsalMark #'direction = #DOWN \mark\markup{\musicglyph #"scripts.dfermata"} | % m. 74-76  
  
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
    
    \new Staff = "bass" \with {
      \consists "Mark_engraver"}
      { \clef bass \bass }
      
  >>
  
\layout { 
  indent = 0.0
  }

}
