TARGET=pgwtc-subjects.lb.pdf

target: $(TARGET)

pgwtc-subjects.lb.tex:
	psql -Aqt -o cache/pgwtc-subjects.lb.tex -c "SELECT * FROM pgwtc.lilypond_book_subjects()"
	cp cache/pgwtc-subjects.lb.tex .

%.lb.pdf: %.lb.tex
	lilypond-book $< --pdf --output cache
	pdflatex -output-directory cache $*.lb.tex
	cp -a cache/$@ $@
