\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue VI"
  subtitle = "in 3 voices"
  opus = "BWV 875"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key d \minor
  \time 4/4
}

soprano = \relative c'' {
  \global
  
   \override TupletBracket.bracket-visibility = ##f
  R1 | % m. 1
  R1 | % m. 2
  \tuplet 3/2 8 { a16[ b! c] \omit TupletNumber d[ c b] c[ d e] f[ e d] } e8 a gis g! | % m. 3
  fis8 f! e d~ d c b! e | % m. 4
  a,8 b! c gis \tuplet 3/2 8 { a16[ g! f] e[ f g] f[ e d] cis![ d e] } \noBreak | % m. 5
  d8 cis d g~ g16 e f g a b! c! a | % m. 6
  d16 cis d e f b,! e d cis a d4 c!8~ | % m. 7
  c8 bes a4 g16 g' a g f a g f | % m. 8
  e16 c f4 e8~ e16 f8 e16~ e d8 cis16 | % m. 9
  d8 bes \tuplet 3/2 8 { a16[ g f] e[ f g] f[ e d] cis![ d e] d[ e f] g[ f e] } | % m. 10
  \tuplet 3/2 8 { f16[ g a] bes[ a g] } a8 d cis c! b! bes | % m. 11
  a8 a' gis g! fis f! e4~ | % m. 12
  e8 d4 cis8 r16 a' bes a g bes a g | % m. 13
  \tuplet 3/2 8 { f16[ e d] cis[ d e] a,[ b! cis] d[ cis b!] cis[ d e] f[ e d] } e8 a | % m. 14
  gis8 g! fis f! e d4 c8 | % m. 15
  b!8 bes4 a8 gis g!4 f8 \prall | % m. 16
  e8 r r4 r \tuplet 3/2 8 { r16 d[ e] f![ e d] } | % m. 17
  \tuplet 3/2 8 { r16 bes'![ a] g[ a bes] r a [ bes] c! [ bes a] } bes8 fis \tuplet 3/2 8 { g16 [ a bes] c [ bes a] }| % m. 18
  \tuplet 3/2 8 { bes16[ c d] es[ d cis] } d4~ d8 r \tuplet 3/2 8 { g16[ f e!] d[ e f] } | % m. 19
  e8 g c, r r4 \tuplet 3/2 8 { f16[ es d] c[ d es] }| % m. 20
  d4 \tuplet 3/2 8 { cis16[ d e] f[ e d] e[ f g] a[ g f] } g8 f | % m. 21
  e4~ e16 d es d cis e! d c! b!8 bes | % m. 22
  a4. g8~ g f e4 | % m. 23
  d4 r r2 | % m. 24
  r2 \tuplet 3/2 8 { d16[ e f] g[ f e] f[ g a] bes[ a g] } | % m. 25
  a8 d cis c! b! bes a g~ | % m. 26
  g16 g f e e8. \prall d16 d2 \fermata \bar "|." | % m. 27
                  
}

mezzo = \relative c' {
  \global
  
  \override TupletBracket.bracket-visibility = ##f
  \tuplet 3/2 8 { d16[ e f]  \omit TupletNumber g[ f e] f[ g a] bes[ a g] } a8 d cis c! | % m. 1
  b!8 bes a g~ g f e a | % m. 2
  f8 d a' r r16 b,! c d e fis g e | % m. 3
  a16 gis a b! c fis, b a gis e a4 g!8~ | % m. 4
  \tuplet 3/2 8 {g16[ f e] d[ e f] e[ d c] b![ c d] } c8 cis d g, | % m. 5
  f8 bes a d cis! d e4 | % m. 6
  d4 r r16 a' bes a g bes a g | % m. 7
  fis16 d g4 f! e!8 d4 | % m. 8
  c16 c' d c bes d c bes a8 g f e | % m. 9
  f8 cis d4~ d8 \clef bass bes a4~ | % m. 10
  \tuplet 3/2 8 { a16[ g f] e[ f g] } f8. g16 r g fis8 r16 f! e8 | % m. 11
  r16 es d8 r16 d cis8 d16 \clef treble a'' bes a g bes a g | % m. 12
  f4 e~ e8 d4 cis8 | % m. 13
  \tuplet 3/2 8 {d16[ e f] g[ f e] f[ g a] bes![ a g] } a8 d cis! c! | % m. 14
  b!8 bes a4~a8 r16 f! e8. e16 | % m. 15
  d8. d16 c8. c16 b!8. bes16 a8 r | % m. 16
  \tuplet 3/2 8 { a'16[ g f] e[ f g] f[ e d] cis[ d e] } d8 a bes b! | % m. 17
  c8 cis d2~ d8 r | % m. 18
  r4 \tuplet 3/2 8 { d'16[ c! bes] a[ bes c] } bes8 d g, b! | % m. 19
  c8 r \tuplet 3/2 8 { c16[ bes a] g[ a bes] } a8 c f, a | % m. 20
  bes8 a~ a2~ a16 a bes a | % m. 21
  g16 bes a g f8 bes, a4 g8 g'~ | % m. 22
  g8 f16 e d f e d cis8 d4 cis8 | % m. 23
  \clef bass d16 c! bes a g bes a g f a g f e d cis e | % m. 24
  \tuplet 3/2 8 { d[ e f] g[ f e] f[ g a] bes[ a g] } a4~ \tuplet 3/2 8 { a16[ g f] e[ f g] } | % m. 25
  f8 \clef treble f' e a~ a8. g16~ g f8 f16 | % m. 26
  e8 d4 cis8 d2 \fermata \bar "|." | % m. 27
    
}

bass = \relative c {
  \global
  
  \override TupletBracket.bracket-visibility = ##f
  R1 | % m. 1
  R1 | % m. 2
  R1 | % m. 3
  R1 | % m. 4
  R1 | % m. 5
  \tuplet 3/2 8 { d16[ e f]  \omit TupletNumber g[ f e] f[ g a] bes[ a g] } a8 d cis c! | % m. 6
  b!8 bes a g~ g f e a | % m. 7
  d,16 d' es d c es d c b! g c4 bes8~ | % m. 8
  bes8 a g c \tuplet 3/2 8 { f,16[ g a] bes[ a g] } a8 a, | % m. 9
  \tuplet 3/2 8 { d16[ e f] g[ f e] f[ g a] bes[ a g] } a8 g \tuplet 3/2 8 { f16[ e d] cis[ d e] } | % m. 10
  d4~ d16 c! bes8 a8. d16 g,8. c16 | % m. 11
  f,8. bes16 e,!8. a16 d,8 d'4 cis8 | % m. 12
  d16 a' bes a g bes a g f4 e | % m. 13
  d8 r r4 r r16 b! c d | % m. 14
  e16 fis g e a gis a b! c fis, b a gis e a8~ | % m. 15
  a16 d, g! f e c f8~ f16 b,! e d cis a d8~ | % m. 16
  d8 cis \tuplet 3/2 8 { d16[ c! bes] a[ bes c!] bes[ a g] fis[ g a] } g8 d \noBreak | % m. 17
  es8 e! f fis \tuplet 3/2 8 { g16[ a bes] c[ bes a] bes[ c d] es[ d c] } | % m. 18
  d8 g4 fis8 \tuplet 3/2 8 { g16[ f! e!] d[ e f] } e8 g | % m. 19
  \tuplet 3/2 8 { c16[ bes a] g[ a bes] } a8 c \tuplet 3/2 8 { f,16[ es d] c[ d es] } d8 f | % m. 20
  \tuplet 3/2 8 { bes16[ a g] fis[ g a] g[ f! e] d[ e f] e[ d c!] b![ cis d] } c16 a d8~ | % m. 21
  d8 cis d g~ g fis g16 f! e d | % m. 22
  cis16 e d c! b!8 bes a16 a' bes a g bes a g | % m. 23
  f16 a g f e d cis e d c! bes a g bes a g | % m. 24
  f8 bes \tuplet 3/2 8 { a16[ g f] e[ f g] f[ e d] cis[ d e] } d8 r | % m. 25
  r16 e f g a b! c a d cis d e f b,! e d | % m. 26
  cis16 a d g a8 a, d,2 \fermata \bar "|." | % m. 27
    
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
