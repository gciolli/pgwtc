\version "2.18.2"

\header {
  title = "Fuga"
  subtitle = "in 4 voices"
  opus = "BWV 1178"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key d \minor
  \time 3/4
}

soprano = \relative c'' {
  \global

  s2. *7 | % m.1-7
  r4 a gis |

  a2. | % m.9
  r4 a b |
  c2. |
  r4 c d |

  e2 f4 | % m.13
  d4 e2 |
  a,2 bes4 |
  e,2 a4 |

  d,2 g4 ~ | % m.17
  g4 f e |
  f4. g8 a4 |
  f2. |

  f4 bes2 ~ | % m.21
  bes4 a g |
  a4 d2 |
  cis2 d4 |

  f,4 e2 ~| % m.25
  e4 d2 |
  d4 c2 |
  c4 f2 |

  e4. e8 ~ e d | % m.29
  c4 d e |
  f2. |
  f4 e fis |

  g4 a2 | % m.33
  a4 gis a |
  d c b |
  a2. |

  s2. | % m.37
  s2. |
  r4 d cis |
  d2. |

  r4 d e | % m.41
  f2. |
  r4 f g |
  a2 bes4 |

  g4 a2 | % m.45
  d,4. d8 e d |
  c4. f8 e d |
  c4 c b |

  cis4 d e | % m.49
  f4 g f |
  e4 f g |
  a2. |

  gis4 a2 | % m.53
  g2 f4 ~ |
  f4 e2 |
  d2 ~ d8 e |

  f2 e4 | % m.57
  e4 d cis |
  d4 c bes |
  a4 a' g |

  f e d | % m.61
  cis2 d4 ~ |
  d2 c4 ~ |
  c4 bes a |

  bes2. ~ | % m.65
  bes4 a g |
  a2. ~ |
  a4 g f |

  g2 f4 | % m.69
  e4 e4. d8 |
  s2. |
  s2. |

  s2. *5 | % m.73-77
  r4 d cis |
  d4 f, e |
  d4 a' f |

  bes2. | % m.81
  a4 c a |
  f'2. |
  e2 d4 |

  d2 c4 | % m.85
  c2 bes4 |
  bes4 a2 |
  a4. g8 f4 |

  e4 e4. d8 | % m.89
  d4\fermata s2 |  
}

alto = \relative c' {
  \global

  r4 d cis | % m.1
  d2. |
  r4 d e |
  f2. |

  r4 f g | % m.5
  a2 bes4 |
  g4 a2 |
  d,4 c b |

  a4 c e | % m.9
  a8 g f e d4 | 
  e4 a, g' |
  f4 e d |

  cis4 a' d, | % m.13
  a'4 g2 ~ |
  g4 f8 e d4 ~ |
  d4 cis c ~ |

  c4 b bes | % m.17
  a4 b cis |
  d2 e4 ~ |
  e4 d cis | 

  d4 d2 ~ |
  d4 c bes | % m.21
  c4 f2 ~ |
  f4 e d |

  d4 ~ d c ~ | % m.25
  c4 c bes8 a |
  bes2  a8 g |
  a4 d2 ~ | 

  d4 c b | % m.29
  a4 b c |
  d4 c d |
  c2 c4 |

  d4 e2 | % m.33
  e2 d4 |
  f4 e2 |
  e2. | 

  d8 e f g a4 ~ | % m.37
  a4 g2 |
  a2. |
  a2. |

  g2. | % m.41
  f4 d' c |
  bes4 d e |
  f4. ees8 d4 |

  ees4 ~ ees d8 c |  % m.45
  c4 bes2 |
  a2 b4 |
  r4 a gis |

  a2. | % m.49
  r4 a b |
  c2. |
  r4 c d |

  e2 f4 |  % m.53
  d4 e2 |
  d2 c4 ~ |
  c4 bes8 a bes4 ~ |

  bes4 a4 ~ a8 g | % m.57
  f2 e4 |
  f4 e d |
  c4 c' bes |

  a4 g f |  % m.61
  e4 a g8 a |
  f8 g e4. a8 |
  fis4 g fis |

  g2. ~ | % m.65
  g4 f e |
  f2. ~ |
  f4 e d |

  e2 d4 |  % m.69
  c4 c b |
  c4 e c |
  f4. a8 g f |

  e4 g e | % m.73
  a4. c8 b a |
  gis4 e' a, ~ |
  a ~ a8 bes g4 ~ | 

  g8 a f4 e | % m.77
  f4 f e |
  f4 d e |
  d2 s4 |

  r4 f g | % m.81
  f4 a f |
  d'2. | 
  cis2 a4 |

  bes4 a2 | % m.85
  a2 g4 |
  g4 ~ g8 e f4 |
  e2 d4 |

  <d b>4 <cis a>2 | % m.89
  a4 s2 |  
}

tenor = \relative c {
  \global

  s2. *18 | % m.1-18
  r4 d cis |
  d2. |

  r4 d e | % m.21
  f2. |
  r4 f g |
  a2 bes4 |

  g4 a2 | % m.25
  g4 fis2 |
  g4 e2 |
  f2 e8 d |

  a'2 e4 | % m.29
  a2. |
  r4 a2 ~ |
  a4 g a |

  bes4 c2 | % m.33
  c4 b a ~ |
  a8 b ~ b a ~ a g ~ |
  g4. g8 f e |

  f4 d' e | % m.37
  d2 e4 |
  f2 e4 |
  r4 a, b |

  c4 b cis | % m.41
  d4 s2 |
  s4 a' g |
  f2 g4 |

  c4 fis,2 |  % m.45
  g4 gis2 |
  r4 a, gis |
  a4 d, e |

  a b cis | % m.49
  d2. |
  c4 d e |
  f2. |

  e4 cis d | % m.53
  b4 c a |
  bes4 g a |
  fis2 g4 |

  d'2. | % m.57
  s2. |
  s2. |
  s2. |

  s2. *9 | % m.61-69
  r4 a gis |
  a2. |
  r4 a b |

  c2. | % m.73
  r4 c d |
  e2 f4 |
  d4 e2 |

  a,4 d cis | % m.77
  d4 g, a |
  d4 a ~ a8 g |
  f2. |

  r4 d2 | % m.81
  c2. |
  s2. |
  r4 a f |

  f2 e4 | % m.85
  d2 d4 |
  c4 c4. d8 |
  r4 a ~ a |

  g8 f e4 g | % m.89
  fis4 s2 |
}

bass = \relative c {
  \global

  s2. *28 | % m.1-28

  r4 a gis | % m.29
  a2. |
  r4 a b |
  c2. |

  r4 c d | % m.33
  e2 f4 |
  d4 e2 |
  cis2. |

  d4 d c' | % m.37
  bes2. |
  a2 ~ a8 g |
  f2. |

  e2. | % m.41
  d4 r r |
  s2. |
  s2. |

  s2. *12 | % m.45-56

  r4 d cis | % m.57
  d2. |
  r4 d e |
  f2. |

  r4 f g | % m.61
  a2 bes4 |
  g4 a2 |
  d,2. |

  r4 g, g' | % m.65
  c,2. |
  r4 f, f' |
  bes,2. |

  r4 g2 | % m.69
  c4. d8 e4 |
  a,2. |
  s2. |

  s2. *6 | % m.73-78
  r4 d cis |
  d2. |

  r4 d e | % m.81
  f2. |
  r4 f g |
  a2 d4 |

  g,4 a ~ a8 g  | % m.85
  fis4 d g |
  e4 f2 |
  cis2 d4 |

  g,4 a2 | % m.89
  d,4 s2 |    
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
    
     \new Staff = "tenor"
      { \clef bass \tenor }
    
    \new Staff = "bass" 
      { \clef bass \bass }
      
  >>
  
\layout { 
  \context {
    \Staff \RemoveEmptyStaves
  }
  indent = 0.0
  }
  
}
