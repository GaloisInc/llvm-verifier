TARGET    = lss-tutorial
TMP       = out
MAIN      = main
MAINTARGET= ${MAIN}/${TARGET}
TMPTARGET = ${TMP}/${TARGET}
SRCS      = ${wildcard *.tex */*.tex *.sty */*.sty} ${wildcard *.bib */*.bib} \
	    ${wildcard *.sty} Makefile ${wildcard *.cry */*.cry} \
	    ${MAINTARGET}.md
SPELLSRC  = ${filter-out ${wildcard utils/*.tex}, ${wildcard *.tex */*.tex}}
TMPDIR    = out
NEWSPELL  = ${TMPDIR}/${TARGET}.SPELLNEW
OLDSPELL  = ${TMPDIR}/${TARGET}.SPELLOLD
SPELL     = aspell -t -l
PDFARGS   = -H ${MAINTARGET}-head.tex \
            -B ${MAINTARGET}-before.tex \
            -A ${MAINTARGET}-after.tex \
            --latex-engine=xelatex
HTMLARGS  = --css ${MAIN}/doc.css \
            --standalone \
            --self-contained
AUX       = ${wildcard ${TMP}/*.blg} ${wildcard ${TMP}/*.bbl} ${wildcard ${TMP}/*.aux} \
            ${wildcard ${TMP}/*.eps} ${wildcard ${TMP}/*.log} ${wildcard ${TMP}/*.toc} \
	    ${wildcard ${TMP}/*.out} ${wildcard ${TMP}/*.idx} ${wildcard ${TMP}/*.ilg} \
	    ${wildcard ${TMP}/*.ind} ${wildcard ${TMP}/*.brf} ${wildcard ${TMP}/*.glg} \
	    ${wildcard ${TMP}/*.glo} ${wildcard ${TMP}/*.gls} ${wildcard ${TMP}/*.ist} \

LATEX     = xelatex -output-driver=xdvipdfmx -output-directory=${TMP} -halt-on-error -file-line-error
BIBTEX    = bibtex
MAKEINDEX = makeindex

all: pdf

pdf: ${TARGET}.pdf

${TARGET}.pdf: ${SRCS}
	pandoc ${PDFARGS} -o $@ ${MAINTARGET}.md

${TARGET}.html: ${SRCS}
	pandoc ${HTMLARGS} -o $@ ${MAINTARGET}.md

# load into Cryptol
cry:
	cryptol -n ${MAINTARGET}

.PHONY: spellClean superClean clean quickSpell

clean:
	rm -f ${AUX}

superClean: clean
	rm -f ${TARGET}.pdf 

spellClean:
	rm -f ${NEWSPELL} ${OLDSPELL}

quickSpell:
	@touch ${NEWSPELL}
	@mv -f ${NEWSPELL} ${OLDSPELL}
	@cat ${SPELLSRC} | ${SPELL} | tr "A-Z" "a-z" | sort | uniq | less > ${NEWSPELL}
	@echo '(The ones marked with < are new.)'
	@diff ${NEWSPELL} ${OLDSPELL}
