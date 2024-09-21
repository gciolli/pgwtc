#!/bin/bash

#
# This script extracts the definition of the voices from an original
# .ly file from the WTC collection.
#

set -e

f () {
    local if=$1
    local x=${if%.ly}
    x=${x#ly/}
    local opt=tmp/${x}-tmp
    local ov=tmp/${x}.ly
    echo
    echo "# BWV $x"
    echo
    csplit -f $opt $if '%^global = %' '/\\\(score\|paper\) /'
    mv ${opt}00 $ov
    rm -f ${opt}*
}

f $1
