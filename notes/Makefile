LATEX = xelatex

memory.pdf : memory.tex
	$(LATEX) memory.tex
	$(LATEX) memory.tex

clean :
	rm -f *.aux *.log memory.pdf

.PHONY : clean
