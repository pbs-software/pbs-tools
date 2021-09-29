\name{findPV}
\alias{findPV}
\title{
  Find Point in Vector
}
\description{
  Find nearest position in vector choice using a target point.

}
\usage{
findPV(p, v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{\code{numeric} -- target point (e.g. 15.3 degrees C).}
  \item{v}{\code{numeric} -- vector choice (e.g. 0:25 degrees C).}
}
\details{
  Using a vector of choices, find the index number in that choice that 
  most closely matches the target point. See answer supplied by Daniel Malter
  in reference below.
}
\value{
  Index number in vector \code{'v'} that most closely matches target point \code{'p'}.
}
\references{
  \href{https://stat.ethz.ch/pipermail/r-help/2008-July/167216.html}{[R] Find the closest value in a list or matrix}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2018-11-13}
}
\seealso{
  \code{\link[PBStools]{addStrip}},
  \code{\link[PBStools]{calcMA}},
  \code{\link[PBStools]{convYM}},
  \code{\link[PBStools]{findHoles}},
  \code{\link[PBStools]{inWord}},
  \code{\link[PBStools]{plotTernary}},
  \code{\link[PBStools]{runCCA}},
  \code{\link[PBStools]{scaleVec}},
  \code{\link[PBStools]{subsetFile}},
  \code{\link[PBStools]{zapHoles}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
\concept{M01_Utility}
