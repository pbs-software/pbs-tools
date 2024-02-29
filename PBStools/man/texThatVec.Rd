\name{texThatVec}
\alias{texThatVec}
\title{
  Tex That Vector
}
\description{
  Convert a vector to a phrase \sQuote{x, y, and z} for use in Sweave.
}
\usage{
texThatVec(vec, simplify=TRUE)
}
\arguments{
  \item{vec}{\code{"numeric"|"character"} -- an R vector to summarise in Sweave.}
  \item{simplify}{\code{logical} -- if \code{TRUE}, remove duplicated values and sort the unique values.}
}
\details{
  Primarily a utility function for use in Sweave where one wishes to convert a 
  vector of items into a phrase for a beautiful sentence.
}
\value{
	Returns a single-element character string that summarises the vector.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-27}
}
\note{
  The function will likely undergo evolution as additional beautiful documents are typeset.
}
\seealso{
  \code{\link[PBStools]{collectFigs}},
  \code{\link[PBStools]{formatCatch}},
  \code{\link[PBStools]{makeLTH}},
  \code{\link[PBStools]{readClog}},
  \code{\link[PBStools]{revStr}},
  \code{\link[PBStools]{scaleVec}},
  \code{\link[PBStools]{subsetFile}},
  \code{\link[PBStools]{texArray}},
  \code{\link[PBStools]{toUpper}},
  \code{\link[PBStools]{wrapText}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}% use one of  RShowDoc("KEYWORDS")
\concept{M08_LaTeX}
