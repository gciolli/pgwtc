LYs = $(patsubst ../ly/%.ly, %, $(wildcard ../ly/fuga-*891.ly))
EXTRACTs = $(patsubst %, cache/extract-%.ly, $(LYs))
CSVs = $(patsubst %, cache/%.csv, $(LYs))

cache/extract-%.ly: ../ly/%.ly
	bash make-extract.sh $*

cache/%.scm: cache/extract-%.ly
	lilypond -dno-print-pages $< >$@

cache/%.csv: cache/%.scm
	racket -f ly-scm2csv.scm $*

target: $(CSVs)

debug:
	@echo LYs: $(LYs)

clean:
	@rm -f cache/*
