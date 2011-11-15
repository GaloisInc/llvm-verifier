LATEX = xelatex

memory.pdf : memory.tex cfg.odd_sum.pdf odd_sum.dominator.pdf
	$(LATEX) memory.tex
	$(LATEX) memory.tex

%.pdf : %.dot
	dot -Tpdf $^ > $@

clean :
	rm -f *.aux *.log memory.pdf cfg.odd_sum.pdf odd_sum.dominator.pdf

.PHONY : clean
