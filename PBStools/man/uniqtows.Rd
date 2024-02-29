\name{uniqtows}
\alias{uniqtows}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  PJS -- Ensure Unique Tows
}
\description{
  Ensure unique tows occur for each survey, collapse records as needed.
}
\usage{
uniqtows(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{\code{data.frame} -- survey data object.}
}
\details{
  Function checks for more than one observation per tow by looking at unique combinations of \code{"year"} and \code{"set"}.
}
\value{
  Revised (if needed) survey data object.
}
\author{
  \href{mailto:paul@starrfish.net}{Paul J. Starr}, Chief Groundfish Scientist\cr
  Canadian Groundfish Research and Conservation Society (CGRCS), Nanaimo BC

  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2019-12-13}
}
\note{
  PJS maintains code in statistical software called \href{https://www.stata.com/}{STATA}.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{calcBiom}},
  \code{\link[PBStools]{doSynoptic}},
  \code{\link[PBStools]{getLabels}},
  \code{\link[PBStools]{keepAtts}},
  \code{\link[PBStools]{plotIndex}},
  \code{\link[PBStools]{prepGFsurv}},
  \code{\link[PBStools]{restratify}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{indices}
\concept{PJS}
% Use only one concept per line.
