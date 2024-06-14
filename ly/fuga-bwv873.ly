\version "2.18.2"

%This edition was prepared and typeset by Kyle Rother using the 1866 Breitkopf & Härtel Bach-Gesellschaft Ausgabe as primary source. 
%Reference was made to both the Henle and Bärenreiter urtext editions, as well as the critical and scholarly commentary of Alfred Dürr, however the final expression is in all cases that of the composer or present editor.
%This edition is in the public domain, and the editor does not claim any rights in the content.

\header {
  title = "Fugue IV"
  subtitle = "in 3 voices"
  opus = "BWV 873"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key cis \minor
  \numericTimeSignature
  \time 12/16
  
}

soprano = \relative c'' {
  \global
  
  R2. | % m. 1
  r4. gis16 fisis gis ais gis ais | % m. 2
  dis,16 eis fisis gis ais b cis b cis ais dis cis | % m. 3
  b16 ais b gis cis b a! gis a fis b a | % m. 4
  gis8. fis4. e8. | % m. 5
  dis8.[ dis'] gis,[ a~] | % m. 6
  a8. gis4. fis8.~ | % m. 7
  fis16 gis a gis a fis e4. | % m. 8
  dis8. r gis16 fis gis e a gis | % m. 9
  fis16 e fis dis gis fis eis dis cis fis8.~ | % m. 10
  fis8. e!4. dis16 cis b | % m. 11
  cis4. b | % m. 12
  a16 gis a fis b a gis b e~ e dis cis | % m. 13
  b16 e gis~ gis fis e dis8.[ eis] | % m. 14
  fis8. r ais[ bis] | % m. 15
  cis16 bis cis dis cis dis gis, ais bis cis dis e | % m. 16
  fis16 e fis dis gis fis e dis e cis b! cis | % m. 17
  ais'8.[ dis,] eis[ fisis] | % m. 18
  gis8. e!4. dis8. | % m. 19
  e4. dis8.[ d!] | % m. 20
  cis8. dis!16 e fis b,8. cis16 dis e | % m. 21
  a,8. b16 cis dis gis,8.~ gis16 b ais | % m. 22
  b8.~ b16 dis cis dis8.~ dis16 fis e | % m. 23
  fis16 gis fis e fis e b' ais gis fis e dis | % m. 24
  cis16 dis cis e b cis dis e dis fis cis dis | % m. 25
  e8.[ eis] a,8 r16 r r fis' | % m. 26
  eis8 e!16 dis8 d!16 cis8 fis16 eis8 e!16 | % m. 27
  dis8 eis16 fis4. e!8.~ | % m. 28
  e8.[ dis~] dis8 cis16 bis8 b!16 | % m. 29
  ais8 a!16 fis' e fis bis, cis dis gis,8. | % m. 30
  ais8.[ bis] cis[ a!~] | % m. 31
  a8. gis4. fis8. | % m. 32
  gis16 dis gis~ gis fis e dis fis a!~ a gis fis | % m. 33
  e8.[ fis] gis8 r16 r8. | % m. 34
  fis'4. eis8.[ e!] | % m. 35
  dis8.[ gis] cis,[ fis~] | % m. 36
  fis8.[ e] dis[ e~] | % m. 37
  e8.[ dis~] dis16 cis b cis dis e!~ | % m. 38
  e16 dis cis b cis b ais8. r | % m. 39
  dis16 cis dis b e dis cis b cis ais dis cis | % m. 40
  bis16 ais gis cis4. b!8.~ | % m. 41
  b8. a!16 gis fis gis4. | % m. 42
  fis4. e16 dis e cis fis e | % m. 43
  dis16 fis b~ b a gis fis eis' fis gis cis, b | % m. 44
  a8. r16 r fis' e8.[ dis~] | % m. 45
  dis16 cis dis e a, gis fis dis' e fis b, a | % m. 46
  gis16 gis' a b e, d! cis4.~ | % m. 47
  cis16 bis cis dis cis dis gis, ais bis cis dis e | % m. 48
  fis16 e fis dis gis fis e8.[ dis] | % m. 49
  cis8.~ cis16 cis e! dis8.[ cis] | % m. 50
  b8.~ b16 b d! cis b cis fis, b a | % m. 51
  gis16 fis gis e a gis fis e fis gis fis gis | % m. 52
  a4.~ a8.[ gis] | % m. 53
  a16 cis b cis b a b eis, fis eis8 r16 | % m. 54
  cis'4. bis8.[ b!] | % m. 55
  ais8.[ dis] gis,[ cis~] | % m. 56
  cis8.[ bis] cis8 r16 a gis a | % m. 57
  fis16 b a gis fis gis e a gis fis8.~ | % m. 58
  fis16 e dis e gis cis~ cis bis ais bis dis gis~ | % m. 59
  gis16 fis e dis e cis bis cis dis gis, gis' \once \override Accidental #'restore-first = ##t fis! | % m. 60
  e16 dis e fisis,8. \prall gis4.~ | % m. 61
  gis8.[ \once \override Accidental #'restore-first = ##t fis!~] fis16 e fis dis gis fis | % m. 62
  eis8.[ e!~] e16 dis e cis fis e | % m. 63
  dis16 cis b e8.~ e16 dis cis dis eis fis | % m. 64
  gis4.~ gis16 fis eis fis a cis | % m. 65
  fis4. eis8.[ e] | % m. 66
  dis8.[ gis] cis,4.~ | % m. 67
  cis16 bis ais bis ais gis gis'8.[ fisis] | % m. 68
  \once \override Accidental #'restore-first = ##t fis!8.[ eis] e! dis8 cis16 | % m. 69
  bis8 gis16 cis4. bis8. | % m. 70
  cis8. r r4. \bar "|." | % m. 71
    
}

mezzo = \relative c' {
  \global
  
  R2. | % m. 1
  R2. | % m. 2
  R2. | % m. 3
  R2. | % m. 4
  cis16 bis cis dis cis dis gis, ais bis cis dis e | % m. 5
  fis16 e fis dis gis fis e dis e cis fis e | % m. 6
  dis16 cis dis b e dis cis dis e dis e cis | % m. 7
  bis4.~ bis8.[ cis~] | % m. 8
  cis8.[ bis] cis4.~ | % m. 9
  cis8. b!4. \clef bass a16 gis fis | % m. 10
  gis4. fis | % m. 11
  e16 dis e cis fis e dis cis b e8.~ | % m. 12
  e8.[ dis] e[ fis] | % m. 13
  gis8.[ ais] b4.~ | % m. 14
  b16 a b \clef treble cis dis e fis e fis dis gis fis | % m. 15
  e8. r r4. | % m. 16
  r4. gis16 fisis gis ais gis ais | % m. 17
  dis,16 eis fisis gis ais b cis b cis ais dis cis | % m. 18
  b16 ais b gis cis b a! gis a fis b a | % m. 19
  gis16 fis gis ais gis ais b4.~ | % m. 20
  b8.[ a~] a[ gis~] | % m. 21
  gis8.[ fis~] fis[ e] | % m. 22
  dis8.[ gis] fis[ b~] | % m. 23
  b8 a!16~ a8 gis16  fis8.[ b~] | % m. 24
  b8 ais16 gis8 g!16 fis8 bis,16 cis8 r16 | % m. 25
  cis'16 dis cis b cis b fis' e! d! cis b a | % m. 26
  gis16 a gis b fis gis a b a cis gis ais | % m. 27
  b8.~ b8 a!16 gis8 bis16 cis8. | % m. 28
  fis,8.~ fis16 e fis gis8.~ gis16 fis gis | % m. 29
  cis,16 bis cis dis cis dis gis, ais bis cis dis e | % m. 30
  fis16 e fis dis gis fis e dis e cis fis e | % m. 31
  dis16 cis dis b e dis cis dis e dis e cis | % m. 32
  bis8.[ cis] dis[ bis] | % m. 33
  cis16 gis cis~ cis b a gis b d!~ d cis b | % m. 34
  a16 b cis~ cis fis, b~ b ais gis ais8. | % m. 35
  b8.[ b'] ais[ a!] | % m. 36
  gis8.[ cis~] cis16 b! ais b cis b | % m. 37
  ais16 b cis b cis ais gis4. | % m. 38
  fisis8. gis4. fisis8. | % m. 39
  gis4.~ gis8.[ fis~] | % m. 40
  fis8. e16 dis cis dis4. | % m. 41
  cis4. b16 a b gis cis b | % m. 42
  ais16 gis fis b4. ais8. | % m. 43
  b8.[ cis] dis[ eis] | % m. 44
  fis16 cis fis~ fis e! d! cis bis' cis dis gis, fis | % m. 45
  e8 gis,16 cis8.~ cis16 b cis dis e fis | % m. 46
  e16 r r r8. r16 eis fis gis cis, b | % m. 47
  a8 a'16 \grace cis,8 (bis8.) cis4.~ | % m. 48
  cis8. bis cis16 e a!~ a fisis gis~ | % m. 49
  gis16 \once \override Accidental #'restore-first = ##t fis! eis fis8.~ fis16 dis gis~ gis eis fis~ | % m. 50
  fis16 e! dis e4. dis!8. | % m. 51
  d!8.[ cis] c![ b] | % m. 52
  \clef bass cis16 d! cis b cis b e d cis b a gis | % m. 53
  fis16 a gis a e fis gis a gis b fis gis | % m. 54
  a8. \clef treble a'~ a16 gis fis eis fis gis | % m. 55
  cis,8.[ bis] gis'16 fis gis e a! gis | % m. 56
  fis16 e fis dis gis fis e dis e cis fis e | % m. 57
  dis16 cis dis b e dis cis8.~ cis16 bis ais | % m. 58
  bis8.[ cis] dis[ fis] | % m. 59
  e16 dis cis fisis8. gis8 r16 r8. | % m. 60
  \clef bass gis,16 fisis gis ais gis ais dis, eis fisis gis ais b | % m. 61
  cis16 b cis ais dis cis bis8.[ b!~] | % m. 62
  b16 ais b gis cis b ais8.[ a!~] | % m. 63
  a8. gis16 fis e fis gis ais b8.~ | % m. 64
  b16 cis d! cis d b a8 r16 r8. | % m. 65
  cis16 bis cis dis cis dis \clef treble gis, ais bis cis dis e | % m. 66
  fis16 e fis dis gis fis e dis e cis a' gis | % m. 67
  fis4. eis8.[ e!] | % m. 68
  dis8.[ gis] cis,[ fis~] | % m. 69
  fis8. e8 dis16 e a! gis dis e dis | % m. 70
  eis8. r r4. \bar "|." | % m. 71
    
}

bass = \relative c {
  \global
  
  cis16 bis cis dis cis dis gis, ais bis cis dis e | % m. 1
  fis16 e fis dis gis fis e dis e cis8.~ | % m. 2
  cis8. [b] ais [fisis'] | % m. 3
  gis,8. e'4. dis8. | % m. 4
  e8.[ bis] cis[ cis'~] | % m. 5
  cis8.[ bis] cis[ fis,] | % m. 6
  b!8.[ e,] a4. | % m. 7
  gis4.~ gis16 fis gis e a gis | % m. 8
  fis16 e fis dis gis fis e dis e cis fis e | % m. 9
  dis8.[ gis] cis,4.~ | % m. 10
  cis16 b cis gis cis b ais fis gis b8.~ | % m. 11
  b8. a!4. gis16 fis e | % m. 12
  fis8.[ b] e, r | % m. 13
  r4. b'16 a! b cis b cis | % m. 14
  fis,8.[ e] dis[ gis] | % m. 15
  cis16 dis e fis8.~ fis16 e dis e fis gis | % m. 16
  a16 gis fis gis ais bis cis4.~ | % m. 17
  cis8.[ b] ais[ dis] | % m. 18
  gis,8.[ cis] fis,[ b] | % m. 19
  e,16 dis e fis e fis b, cis dis e fis gis | % m. 20
  a16 gis a fis b a gis fis gis e a gis | % m. 21
  fis 16 e fis dis gis fis e dis e cis8 r16 | % m. 22
  gis'16 fis gis e8 r16 b' ais b gis8 r16 | % m. 23
  dis'8. e4. dis16 cis b | % m. 24
  fis'8 fis,16 gis8 ais16 b8 gis16 ais8 bis16 | % m. 25
  cis16 b! a! gis8. \prall fis r | % m. 26
  r2. | % m. 27
  gis16 a gis fis gis fis cis' b a gis fis e | % m. 28
  dis16 e dis fis cis dis e fis e gis dis eis | % m. 29
  fis4.~ fis8.[ e] | % m. 30
  dis8.[ gis] cis,[ fis] | % m. 31
  b,8.[ e] a,4. \prall | % m. 32
  gis8.[ ais] bis[ gis] | % m. 33
  cis8.[ dis] e[ eis] | % m. 34
  fis8.[ d!] cis fis16 e fis | % m. 35
  b,16 gis' fis eis dis eis dis gis e! dis e cis | % m. 36
  bis16 cis dis e dis cis gis'8 dis16 gis8. | % m. 37
  fisis8.[ fis] eis[ ais] | % m. 38
  dis,8.~ dis16 e dis cis b cis ais dis cis | % m. 39
  b16 ais b gis cis b ais8.[ dis] | % m. 40
  gis,8. r gis'16 fis gis dis gis fis | % m. 41
  eis16 dis cis fis4. e!8.~ | % m. 42
  e8. dis16 cis b cis8.[ fis] | % m. 43
  b,8. r16 r b' a8.[ gis] | % m. 44
  fis8.[ gis] ais[ bis] | % m. 45
  cis8 r16 r r cis, dis8. r16 r dis' | % m. 46
  e16 e, fis gis fis gis a gis fis eis dis eis | % m. 47
  fis4. eis8.[ e!] | % m. 48
  dis8.[ gis] cis,[ b!] | % m. 49
  a8. r16 r ais b8.[ a!] | % m. 50
  gis8. r16 r gis' a gis a b a b | % m. 51
  e,8.[ a] dis,![ e] | % m. 52
  a,16 b cis d! e d cis b a e'8.~ | % m. 53
  e8.[ dis] d![ cis~] | % m. 54
  cis16 bis cis dis cis dis gis, ais bis cis dis eis | % m. 55
  fis16 e! fis dis gis fis e dis e cis fis e | % m. 56
  dis8.[ gis] cis,[ fis] | % m. 57
  b8.[ e,] a[ dis,] | % m. 58
  gis2.~ | % m. 59
  gis4.~ gis8 fis16 e8 dis16 | % m. 60
  cis4. bis8.[ b!] | % m. 61
  ais8.[ dis] gis,8 r16 gis' fis gis | % m. 62
  cis,8 r16 cis b cis fis,8 r16 fis' e fis | % m. 63
  b,4.~ b8.~ b16 cis d! | % m. 64
  eis,8.[ eis'] fis8 gis16 a8.~ | % m. 65
  a8 gis16 fis8. gis4.~ | % m. 66
  gis4.~ gis16 fis gis a gis a | % m. 67
  dis,16 e fis gis ais bis cis bis cis ais dis cis | % m. 68
  bis16 ais bis gis cis b! a! gis a bis, cis dis | % m. 69
  e16 dis e fisis, gis ais gis4. | % m. 70
  cis,8. r r4. \bar "|." | % m. 71
  
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
