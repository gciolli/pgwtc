#!/bin/bash

# This script reads all the *.ly files in the given directory, does
# some sanitization, and then outputs them in a format suitable for
# ingestion by the COPY command in PostgreSQL.

f () {
    local if=$1
    local bwv=${if%.ly}
    bwv=${bwv#ly/fuga-bwv}
    echo -n "${bwv}@"
    sed -zf lib/ly2pg.sed $if
    echo
}

for x in $1/*.ly; do
    f $x
done
