## twice seems enough for now -- could also use texi2dvi, not portably
../inst/doc/PBStools-UG.pdf: PBStools-UG.Rnw
	pdflatex PBStools-UG.Rnw
	pdflatex PBStools-UG.Rnw
	mv PBStools-UG.pdf ../inst/doc
	@rm -f PBStools-UG.aux PBStools-UG.log PBStools-UG.out
