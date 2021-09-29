\name{findRC}
\alias{findRC}
\title{
  Find Rows and Columns
}
\description{
  Find the number of rows and columns for plotting multi-panel figures 
  given the number of figures to fit on one page.
}
\usage{
findRC(nf, orient="portrait")
}
\arguments{
  \item{nf}{\code{numeric} -- number of figures to fit on one page.}
  \item{orient}{\code{character} -- orientation:
    if \code{"portrait"}, return more rows than columns;
    if \code{"landscape"}, return more columns than rows.}
}
\details{
  Find the best square fit and favour rows or columns depending on the orientation specified.
}
\value{
  Two-element numeric vector specifying number of rows and columns for a multipanel page.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2018-11-13}
}
\note{
 This function is basically a spin-off from \code{'.findSquare'} in \code{PBSmodelling}.
}
\seealso{
  \code{\link[PBStools]{countLines}},
  \code{\link[PBStools]{crossTab}},
  \code{\link[PBStools]{findPV}},
  \code{\link[PBStools]{gatherVals}},
  \code{\link[PBStools]{inWord}},
  \code{\link[PBStools]{listTables}},
  \code{\link[PBStools]{makeLTH}},
  \code{\link[PBStools]{scaleVec}},
  \code{\link[PBStools]{splitTab}},
  \code{\link[PBStools]{texArray}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}% use one of  RShowDoc("KEYWORDS")
\keyword{arith}% __ONLY ONE__ keyword per line
\concept{M01_Utility}
