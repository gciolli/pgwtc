#!/bin/bash

x=$1
if=../ly/$x.ly
pf="cache/tmp-${x}-"
of=cache/extract-$x.ly

csplit -f $pf $if '/score/'

cat ${pf}00 >$of

rm -f ${pf}??

cat >>$of <<EOF

#(display "(define voces '(")
#(newline)

{
EOF

for y in $(sed -nf make-extract.sed $if); do
    sf=cache/${x}-${y}.scm
    cat >>$of <<EOF

  #(display "('${y} . ")
  #(newline)
  \\displayMusic \\${y}
  #(display ")")
  #(newline)

EOF
done

cat >>$of <<EOF
}

#(display "))")
#(newline)

EOF
