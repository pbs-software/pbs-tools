\name{restratify}
\alias{restratify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  PJS -- Restratify Survey by Depth
}
\description{
  Restratify survey by using depth intervals not originally used in survey.
}
\usage{
restratify(dat, strategy, dbegin, dend, renamevar)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{\code{data.frame} -- survey data object.}
  \item{strategy}{\code{numeric} -- code that chooses depths to use for restratification.}
  \item{dbegin}{\code{character} -- field name for beginning depth of tow.}
  \item{dend}{\code{character} -- field name for ending depth of tow.}
  \item{renamevar}{\code{character} -- name for new stratum field}
}
\details{
  Restratification based on depths using the \code{strategy}:
  \itemize{
    \item \code{strategy=0} -- use the minimum of beginning and ending depths;
    \item \code{strategy=1} -- use the maximum of beginning and ending depths;
    \item \code{strategy=2} -- use beginning depth;
    \item \code{strategy=3} -- use ending depth;
    \item \code{strategy=4} -- use the mean of beginning and ending depths.
  }
}
\value{
  Survey data object with additional field with new depth strata.
}
\author{
  \href{mailto:paul@starrfish.net}{Paul J. Starr}, Chief Groundfish Scientist\cr
  Canadian Groundfish Research and Conservation Society (CGRCS), Nanaimo BC

  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
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
  \code{\link[PBStools]{uniqtows}}
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
