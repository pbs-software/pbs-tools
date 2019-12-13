\name{doSynoptic}
\alias{doSynoptic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  PJS -- Perform Common Machinations for Synoptic Surveys
}
\description{
  Get the synoptic file for specified survey and go through PJS machinations.
}
\usage{
doSynoptic(file, survey, logappendflag=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{\code{data.frame|character} -- input can be either a file name or a data frame that has already been imported.}
  \item{survey}{\code{numeric} -- PJS survey number.}
  \item{logappendflag}{\code{logical} -- if \code{TRUE}, sets variable \code{append=TRUE}, which has no effect at present.}
}
\details{
  Although this function was designed by PJS for use with synoptic surveys (four in BC),
  it can be used on other surveys snd serves as a standardising routine.
}
\value{
  Modified survey data frame object (e.g., adds best depth, calculates missing distance, etc.).
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
  \code{\link[PBStools]{getLabels}},
  \code{\link[PBStools]{keepAtts}},
  \code{\link[PBStools]{plotIndex}},
  \code{\link[PBStools]{prepGFsurv}},
  \code{\link[PBStools]{restratify}},
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
