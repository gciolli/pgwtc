SRC1=$(wildcard ly/fuga-*.ly)
SRC2=$(patsubst ly/%.ly, tmp/%.ly, $(SRC1))
SRC3=$(patsubst ly/%.ly, tmp/midi-%.ly, $(SRC1))
MIDs=$(patsubst ly/%.ly, tmp/midi-%.mid, $(SRC1))

target: $(MIDs)

tmp/%.mid : tmp/%.ly
	lilypond -o tmp/ $<

tmp/midi-%.ly : tmp/%.ly
	bash lib/build-midis.sh $< >$@

tmp/%.ly : ly/%.ly
	bash lib/extract-voices.sh $<
