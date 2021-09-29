\name{calcHM}
\alias{calcHM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate the Harmonic Mean}
\description{
  Calculate the harmonic mean of a numeric vector,
  possibly excluding zeroes and/or adding an offset 
  to compensate for zero values.
}
\usage{
calcHM(x, offset=0, exzero=TRUE)
}
\arguments{
  \item{x}{\code{numeric} -- vector of numbers}
  \item{offset}{\code{numeric} -- value to add to all components, including zeroes}
  \item{exzero}{\code{logical} -- if \code{TRUE}, exclude zeroes (but still add the offset)}
}
\details{
  Calculation: N / sum(1/x)
}
\value{
  Harmonic mean of the modified vector \code{x + offset}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2020-07-13}
}
\note{
  \code{NA} values are automatically removed from \code{x}.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{calcGM}}\cr
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{calcMA}},
  \code{\link[PBStools]{countLines}},
  \code{\link[PBStools]{countVec}},
  \code{\link[PBStools]{findPV}},
  \code{\link[PBStools]{prime}},
  \code{\link[PBStools]{scaleVec}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
\keyword{arith}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M01_Utility}
% \concept{ ~cpt2 }
% Use only one concept per line.
