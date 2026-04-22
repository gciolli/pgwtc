BWVs = 846 847 848 849 850 851 852 853 854 855 856 857 \
       858 859 860 861 862 863 864 865 866 867 868 869 \
       870 871 872 873 874 875 876 877 878 879 880 881 \
       882 883 884 885 886 887 888 889 890 891 892 893 \
       1178

LYs = $(patsubst %, fuga-bwv%, $(BWVs))

EXTRACTs = $(patsubst %, cache/extract-%.ly, $(LYs))
CSVs = $(patsubst %, cache/%.csv, $(LYs))

cache/extract-%.ly: ../ly/%.ly
	bash make-extract.sh $*

cache/%.scm: cache/guile-%.scm
	sed -f deguile.sed $< >$@

cache/guile-%.scm: cache/extract-%.ly
	lilypond -dno-print-pages $< >$@

cache/%.csv cache/%.log: cache/%.scm
	racket -f ly-scm2csv.scm $* 2>cache/$*.log

target: $(CSVs)

debug:
	@echo LYs: $(LYs)

clean:
	@rm -f cache/*

.DELETE_ON_ERROR:
