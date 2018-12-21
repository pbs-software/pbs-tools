\name{countVec}
\alias{countVec}
\title{
  Count Real Non-Zero Items in Vector
}
\description{
  Count the number of items that exclude zeroes and are available (not NA)
  in a vector of values.
}
\usage{
countVec(x, exzero=TRUE)
}
\arguments{
  \item{x}{\code{numeric} -- vector of numbers (e.g., specimen ages).}
  \item{exzero}{\code{logical} -- if \code{TRUE} exclude zero values from the count.}
}
\details{
  Bascially made for use in cross-tabulations where a user wants to know how many
  positive ages or lengths occur for a combination of factors (e.g., year vs. SSID).
}
\value{
  Total count of observations.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2018-12-13}
}
\seealso{
  \code{\link[PBStools]{biteData}},
  \code{\link[PBStools]{calcMA}},
  \code{\link[PBStools]{compAF}},
  \code{\link[PBStools]{compLen}},
  \code{\link[PBStools]{countLines}},
  \code{\link[PBStools]{findPV}},
  \code{\link[PBStools]{findRC}},
  \code{\link[PBStools]{plotMW}},
  \code{\link[PBStools]{revStr}},
  \code{\link[PBStools]{scaleVec}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
