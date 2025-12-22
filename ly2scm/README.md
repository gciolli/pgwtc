The code in this directory parses .ly source files by reading them
with Lilypond and exporting the voices as Scheme objects.

 1. The `make-extract.sh` shell script reads

        ../ly/X.ly

    and creates a file

        cache/extract-X.ly

 2. Lilypond reads that file and creates a file

        cache/X.scm

    which contains a Scheme list of pairs `(A . B)` where A is a
    symbol indicating the voice and B is the Scheme object containing
    the music for that voice

 3. The `ly-scm2csv.scm` Scheme script reads that file and creates a
    file

        cache/X.csv

    which is ready for ingestion by `pgwtc`.
