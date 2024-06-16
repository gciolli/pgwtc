#!/bin/bash

#
# This script creates a MIDI file from a "voci" file.
#

set -e

f () {
    local if=$1
    #
    cat <<EOF
$(cat $if)

\\score {
  <<
EOF
    sed -nf lib/build-midis.sed $if
    cat <<EOF
  >>
  \\midi {}
}
EOF
}

f $1
