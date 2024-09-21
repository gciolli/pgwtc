#!/bin/bash

f () {
    local if=$1
    local x=${if%.midi}
    x=${x#tmp/midi-fuga-bwv}
    mftext $f | sed -n -f mftext2pg.sed | sed -e "s/@BWV@/${x}/g"
}

for f in $@; do
    f $f
done
