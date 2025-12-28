#!/bin/bash

x=$1
if=../ly/$x.ly
pf="cache/tmp-${x}-"
of=cache/extract-$x.ly

csplit -f $pf $if '/score/'

cat ${pf}00 >$of

rm -f ${pf}??

cat >>$of <<EOF
#(display "#hash(")
EOF

for y in $(sed -nf make-extract.sed $if); do
    sf=cache/${x}-${y}.scm
    cat >>$of <<EOF
  #(newline)
  #(newline)
  #(display "(${y} . ")
  \\displayMusic \\${y}
  #(display ")")
EOF
done

cat >>$of <<EOF
#(display ")")
EOF
