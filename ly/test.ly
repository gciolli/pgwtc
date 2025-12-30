\version "2.24.1"

\header {
  title = "Test notes"
  composer = "Test composer"
  copyright = "No rights reserved."
  tagline = ""
}

global = {
  \key c \major
  \time 4/4
}

soprano = \relative c' {
  \global
  c4 r d-. e-!
}

\score {
  \new StaffGroup
  <<
    \new Staff = "soprano" 
      \soprano
  >>
  
\layout {
  indent = 0.0
}

}
