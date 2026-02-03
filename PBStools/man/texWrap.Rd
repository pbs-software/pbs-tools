\name{texWrap}
\alias{texWrap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Wrap Tex Blob
}
\description{
  Wrap a tex blob with a tex preamble and postamble
  so that the blob can be compiled in \LaTeX.
}
\usage{
texWrap(texfile, outnam="someTex", start.page=1,
   add.table=FALSE, add.defs=NULL, compile=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texfile}{\code{character} -- name of a tex file containing the tex blob}
  \item{outnam}{\code{character} -- output name (sans suffix `.tex')}
  \item{start.page}{\code{numeric} -- page number of the first page}
  \item{add.table}{\code{logical} -- if \code{TRUE}, extra commands will be
    added to the preamble to deal with tables}
  \item{add.defs}{\code{character} -- an optional vector of \LaTeX commands
    to add to the preamble (e.g., colour definitions, macros, etc.)}
  \item{compile}{\code{logical} -- if \code{TRUE}, latex commands will be 
    sent to the system's command interface:
    \preformatted{
paste0("pdflatex.exe -quiet ", outnam, ".tex")
paste0("latexmk.exe  -c ", outnam, ".tex")
  } }
}
\details{
	The blob is amended thusly:\cr
	\code{c(texhead, "\\begin{document}", texblob, "\\end{document}")}
}
\value{
  A \LaTeX file in the user's workspace to compile into a PDF for perusal.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: remote office, Vancouver BC\cr
  Last modified \code{Rd: 2025-07-25}
}
\note{
  The function will likely evolve over time as various blobs are cleansed, folded, and manipulated.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{texArray}},
  \code{\link[PBStools]{convUTF}},
  \code{\link[PBStools]{readClog}},
  \code{\link[PBStools]{subsetFile}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M08_LaTeX}
% \concept{ ~cpt2 }
% Use only one concept per line.
