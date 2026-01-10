# Invalid Syntax: Staccato

Four of the 48 fugues have articulations such as staccato (-.) and
staccatissimo (-!) which Lilypond exports using a syntax that Racket
cannot parse.

See the test.ly file, which includes a single bar:

    c4 r d-. e-!

which is exported as follows:

    (make-music
      'NoteEvent
      'duration
      (ly:make-duration 2)
      'pitch
      (ly:make-pitch 0 0))

    (make-music
      'RestEvent
      'duration
      (ly:make-duration 2))

    (make-music
      'NoteEvent
      'articulations
      (list (make-music
              'ArticulationEvent
              'midi-extra-velocity
              4
              'midi-length
              #<procedure 7fdf8a6e5100 at ice-9/eval.scm:336:13 (a b)>
              'articulation-type
              'staccato))
      'duration
      (ly:make-duration 2)
      'pitch
      (ly:make-pitch 0 1))

    (make-music
      'NoteEvent
      'articulations
      (list (make-music
              'ArticulationEvent
              'midi-extra-velocity
              6
              'midi-length
              #<procedure 7fdf8a6e5cc0 at ice-9/eval.scm:336:13 (a b)>
              'articulation-type
              'staccatissimo))
      'duration
      (ly:make-duration 2)
      'pitch
      (ly:make-pitch 0 2)))))

Rather than embarking on a complicated modification of Lilypond export
functions, we note that it is sufficient to simply replace all the lines like

    #<procedure ...>

with an expression that parses.

Given that such articulations fall out of the scope of our analysis,
we simply replace them with `#f`.

# Other Invalid Syntax

As of 2026-01-04, the following elements are not yet properly
processed:

  - ContextSpeccedMusic
  - SimultaneousMusic
  - TimeScaledMusic
  - AdHocMarkEvent
  - ChordEvent
  - GraceMusic

For now, those elements end up in the CSV as a single tuple with the
"notes" attribute set to "TODO".
