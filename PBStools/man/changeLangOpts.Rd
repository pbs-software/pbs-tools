\name{changeLangOpts}
\alias{changeLangOpts}
\alias{eop}
\alias{fop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Change Language Options for Number Displays
}
\description{
  Change the options that control the display of numbers in plots based on language.
}
\usage{
changeLangOpts(L="e", stringsAsFactors=FALSE)
eop()
fop()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{L}{\code{character} -- language designator (only one allowed), where \code{"e"} 
    specifies English and \code{"f"} specifies French.}
  \item{stringsAsFactors}{\code{logical} -- if \code{FALSE}, turn this option off;
    if \code{TRUE}, lord help you.}
}
\details{
  The function switches a few \code{options()} depending on language:\cr
  \code{"e"} -- \code{list(OutDec=".", big.mark=",", stringsAsFactors=stringsAsFactors)}\cr
  \code{"f"} -- \code{list(OutDec=",", big.mark=" ", stringsAsFactors=stringsAsFactors)}.

  The quick-utility functions \code{'eop'} and \code{'fop'} call the function 
  \code{'changeLangOpts'} with either \code{L="e"} or \code{L="f"}, respectively.
}
\value{
  No value returned, just changes three items in \code{options()}.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2019-09-30}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{createFdir}},
  \code{\link[PBStools]{linguaFranca}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{environment}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M01_Utility}
\concept{figures}
\concept{language}
% Use only one concept per line.
