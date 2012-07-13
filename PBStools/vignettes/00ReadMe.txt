Vignette Notes
==============

1. We've taken the 'makefile' concept from RODBC, presumably to help build 
   vignettes in UNIX-based operating systems. The makefile contents are:

   ## twice seems enough for now -- could also use texi2dvi, not portably
   ../inst/doc/PBStools-UG.pdf: PBStools-UG.Rnw
   pdflatex PBStools-UG.Rnw
   pdflatex PBStools-UG.Rnw
   mv PBStools-UG.pdf ../inst/doc
   @rm -f PBStools-UG.aux PBStools-UG.log PBStools-UG.out

   According to Jim Uhl (Oracle DBA and VMS system manager, Vancouver Island 
   University, Nanaimo, BC) the cryptic first line:

   ../inst/doc/PBStools-UG.pdf: PBStools-UG.Rnw
   can be interpreted as:

   ../inst/doc/PBStools-UG.pdf depends on PBStools-UG.Rnw - if the latter
   changes, do the following actions to rebuild
   ../inst/doc/PBStools-UG.pdf

   When 'make' runs it will check the timestamps of the two files to
   decide whether or not to run the actions.

2. We include the Journal of Statistical Software control files 
   (jss.bst, jss.cls) in the package 'vignettes' folder to enforce the proper 
   implementation of citations and the bibliography.

   A straight-forward build from the command line:

   R CMD INSTALL --build --compact-docs %1

   seems to find these files in the R installation, presumably at:
   C:\Apps\R\R2151\share\texmf\bibtex\bst  and
   C:\Apps\R\R2151\share\texmf\tex\latex
   but our build routines use a an R script file:

   Rscript.exe build.r

   that calls the install command (above) using the R function 'system'.
   This method does not implement the bibliography build unless we 
   issue the build commands twice. Perhaps the bibliography needs this?

   system("R CMD build --compact-vignettes PBStools",wait=TRUE,intern=TRUE)

   system("R CMD INSTALL --build --compact-docs PBStools",wait=TRUE,intern=TRUE)

   For now, however, we include the jss files in the vignettes folder.

