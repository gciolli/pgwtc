\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue IX"
  subtitle = "in 4 voices"
  opus = "BWV 878"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key e \major
  \time 2/1
  \set Score.timeSignatureFraction = 2/2
}

soprano = \relative c'' {
  \global
  
  R\breve | % m. 1
  R\breve | % m. 2
  R\breve | % m. 3
  R\breve | % m. 4
  r1 b1 | % m. 5
  cis2 e dis cis | % m. 6
  b1~ b4 a8 gis a2~ | % m. 7
  a2 gis fis2 e | % m. 8
  dis2 r r1 | % m. 9
  R\breve | % m. 10
  e1 fis2 a! | % m. 11
  gis2 fis eis4 fis gis a | % m. 12
  b4 fis b2~ b a | % m. 13
  gis1~ gis2. a8 b | % m. 14
  cis4 bis cis dis b4 cis2 b4 | % m. 15
  cis1 r | % m. 16
  b1 cis2 e | % m. 17
  dis2 cis b cis | % m. 18
  fis,4 dis gis2~ gis4 fis8 e fis2~ | % m. 19
  fis4 e8 dis e4 fis8 e dis4 b' eis,2~ | % m. 20
  eis4 fis2 eis4 cis'2 b~ | % m. 21
  b4 a2 gis4~ gis4 fis2 eis4 | % m. 22
  fis1 gis4 a b2~ | % m. 23
  b4 a gis2 fis e4 fis | % m. 24
  gis2. a4 gis fis e2 | % m. 25
  r1 r2 gis | % m. 26
  a4 cis b a gis2 ais | % m. 27
  b1~ b2. ais4 | % m. 28
  b4 gis a! b cis4 dis e2~ | % m. 29
  e2. dis4 e4 b2 ais4 | % m. 30
  b2 r4 fis' b, gis a b | % m. 31
  cis2 b a gis | % m. 32
  fis4 e' dis cis b2 ais~ | % m. 33
  ais4 gis8 fisis gis2~ gis2. fisis4 | % m. 34
  gis2 r4 b gis e fis gis | % m. 35
  a4 b cis dis e8 b e2 dis4~ | % m. 36
  dis4 cis8 b cis2 b4 r e2 | % m. 37
  fis2 a gis fis | % m. 38
  e2 dis cis2 b~ | % m. 39
  b4 cis b a gis2 r | % m. 40
  r2 e' dis cis | % m. 41
  b2 a gis8 fis gis a b4 a | % m. 42
  gis2 fis e1 \fermata \bar "|." | % m. 43
   
}

alto = \relative c' {
  \global
  
  R\breve | % m. 1
  R\breve | % m. 2
  R\breve | % m. 3
  e1 fis2 a | % m. 4
  gis2 fis e r4 b | % m. 5
  e4 fis gis ais b8 fis b2 a!4~ | % m. 6
  a4 gis8 fis gis2 cis,2. dis8 e | % m. 7
  dis2 e~ e4 dis cis2 | % m. 8
  fis,2 b cis e | % m. 9
  dis2 cis b4 e2 dis4~ | % m. 10
  dis4 cis2 b8 ais b4 fis'2 e8 dis | % m. 11
  e2. dis4 cis1 | % m. 12
  b4 cis dis e fis4 cis fis2~ | % m. 13
  fis2 e dis1 | % m. 14
  cis4 dis e fis gis2 fis | % m. 15
  e1 fis2 a! | % m. 16
  gis2 fis e4 cis gis'2~ | % m. 17
  gis4 fis8 eis fis2~ fis4 e!8 dis e4 fis8 e | % m. 18
  dis2. gis,4 a2. ais4 | % m. 19
  b2. ais4 b2 d! | % m. 20
  cis1~ cis2 gis' | % m. 21
  cis,2 b a gis | % m. 22
  a4 b cis2~ cis dis4 eis | % m. 23
  fis2. e!4 dis2 cis~ | % m. 24
  cis4 dis8 cis bis2 cis4 dis gis,2 | % m. 25
  dis'4 e8 dis cis4 a' bis, cis2 bis4 | % m. 26
  cis2 dis e4 gis fis e | % m. 27
  dis4 a'! gis fis e gis fis e | % m. 28
  dis4 e fis gis a2. gis4 | % m. 29
  fis4 e fis2 e1 | % m. 30
  fis2 a! gis fis | % m. 31
  e4 a2 gis4 fis d'! cis b | % m. 32
  a4 fisis gis ais gis \once \override Accidental #'restore-first = ##t fis! e!2 | % m. 33
  dis2 cisis dis1 | % m. 34
  dis2 b cis e | % m. 35
  dis2 cis b b' | % m. 36
  gis4 e gis ais b8 fis b2 a!4~ | % m. 37
  a4 dis, e fis gis cis a b | % m. 38
  gis4 a fis gis e fis dis e~ | % m. 39
  e4 dis8 cis dis2 r4 e8 fis gis4 ais | % m. 40
  b8 fis b2 a!4~ a8 dis, gis2 fis4~ | % m. 41
  fis8 b, e2 dis4 e r r e | % m. 42
  e2 dis b1 \fermata \bar "|." | % m. 43
    
}

tenor = \relative c' {
  \global
  
  R\breve | % m. 1
  r1 b | % m. 2
  cis2 e dis cis | % m. 3
  b2. b4 a b cis dis | % m. 4
  e8 b e2 dis4 e2 gis,4 fis | % m. 5
  gis2 e fis1~ | % m. 6
  fis4 b, e2~ e4 cis fis2~ | % m. 7
  fis4 e8 fis gis4 a b8 fis b2 ais4 | % m. 8
  b2 r e,1 | % m. 9
  fis2 a gis fis | % m. 10
  e4 a2 gis4 fis e8 dis e4 fis | % m. 11
  gis4 a b8 fis a4 gis2 fis~ | % m. 12
  fis2 e dis4 eis fis2 | % m. 13
  cis4 dis e fis gis dis gis2~ | % m. 14
  gis2 fis fis4 e dis2 | % m. 15
  cis4 gis' cis2~ cis4 b8 ais b2~ | % m. 16
  b4 a!8 gis a4 b8 a gis2. gis4 | % m. 17
  a2. ais4 b2. ais4 | % m. 18
  b4. a!8 gis fis e dis e2 r | % m. 19
  fis1 gis2 b | % m. 20
  a2 gis~ gis4 fis2 eis4 | % m. 21
  fis2 d! cis1~ | % m. 22
  cis2. dis4 eis4 cis gis'2 | % m. 23
  fis2 gis4 cis,~ cis4 bis cis dis! | % m. 24
  e4 fis8 e dis2~ dis e4 fis | % m. 25
  gis2. fis4 e2 dis | % m. 26
  e2 r r1 | % m. 27
  r2 b' cis4 e dis cis | % m. 28
  b4 d! cis b a2 b | % m. 29
  b2. a4 gis2. cis4 | % m. 30
  fis,4 dis e fis gis b cis dis | % m. 31
  gis,4 e fis gis a b cis2~ | % m. 32
  cis4 cis b ais b dis cis2 | % m. 33
  b2. ais8 gis ais4 b cis2 | % m. 34
  b2 r e,1 | % m. 35
  fis2 a gis fis | % m. 36
  e2 r r r4 e | % m. 37
  dis4 b' cis dis e8 b e2 dis4~ | % m. 38
  dis8 gis, cis2 b4~ b8 e, a2 gis4 | % m. 39
  fis1 e4 gis cis fis,~ | % m. 40
  fis4 fis gis a b dis, e fis | % m. 41
  gis2 a b4 r r cis | % m. 42
  b2. a4 gis1 \fermata \bar "|." | % m. 43
    
}

bass = \relative c {
  \global
  e1 fis2 a | % m. 1
  gis2 fis e4 dis8 cis dis4 b | % m. 2
  e4 fis gis ais b8 fis b2 a!4~ | % m. 3
  a4 gis8 fis gis2~ gis fis | % m. 4
  gis2 a4 b8 a gis4 fis e dis | % m. 5
  cis1 b2 fis | % m. 6
  gis2. e4 a2. fis4 | % m. 7
  b\breve~ | % m. 8
  b2 r r1 | % m. 9
  r1 b | % m. 10
  cis2 e dis cis | % m. 11
  b1~ b2 a | % m. 12
  gis1 fis4 gis a b | % m. 13
  cis4 gis cis2~ cis b | % m. 14
  a1 gis | % m. 15
  cis2 r4 cis d!2. dis4 | % m. 16
  e2. dis4 e2 cis | % m. 17
  fis2 e! dis cis | % m. 18
  b1 cis2 e | % m. 19
  dis2 cis b4 gis2 a!8 b | % m. 20
  cis2 b a gis | % m. 21
  fis2 b cis cis, | % m. 22
  fis4 gis a fis cis'2 b4 cis | % m. 23
  d!4 cis bis cis fis,4 gis a2 | % m. 24
  gis1 ais4 bis cis2~ | % m. 25
  cis4 b! a!2 gis1~ | % m. 26
  gis2 fis e4 cis fis2 | % m. 27
  b2 r r fis' | % m. 28
  gis4 b a gis fis2 e | % m. 29
  b'2 b, cis4 e dis cis | % m. 30
  dis4 b cis dis e2. dis4 | % m. 31
  e4 cis dis eis fis2. eis4 | % m. 32
  fis4 dis eis fisis gis2. fisis4 | % m. 33
  gis2 eis dis dis, | % m. 34
  gis2 r r1 | % m. 35
  r1 b | % m. 36
  cis2 e dis cis | % m. 37
  b4 r r2 r1 | % m. 38
  R\breve | % m. 39
  b1 cis2 e | % m. 40
  dis2 cis b a | % m. 41
  gis2 fis e4 fis gis a | % m. 42
  b1 e,1 \fermata \bar "|." | % m. 43
    
}

\score {
  \new StaffGroup
  <<
    \new Staff = "soprano"
      \soprano
    
    \new Staff = "alto" 
      \alto
    
    \new Staff = "tenor" 
      { \clef bass \tenor }
    
    \new Staff = "bass" 
      { \clef bass \bass }
      
  >>
  
\layout {
  indent = 0.0
  }

}
