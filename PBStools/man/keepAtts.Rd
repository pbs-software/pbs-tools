\name{keepAtts}
\alias{keepAtts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  PJS -- Keep Attributes
}
\description{
  Keep metadata attributes of data objects when they are altered.
}
\usage{
keepAtts(dat, expr, batts=c("names","row.names","class"), extras=list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{\code{data.frame} -- survey data file.}
  \item{expr}{\code{expression} -- usualy an R expression that subsets \code{dat}.}
  \item{batts}{\code{character} -- string vector of attribute names that are likely to 
    change on subsetting (i.e. determined by subset action) and so are excluded from 
    re-attachment to the altered attribute list.}
  \item{extras}{\code{list} -- extra data that user might employ in the \code{expr}.}
}
\details{
  For some reason, R drops attributes when subsetting (not sure if this is universal).
  This function serves to preserve attributes (chiefly metadata) whenever the survey 
  data object is altered (e.g, subsetting, appending, etc.).
}
\value{
  Data object that has been altered in some way but has retained metadata attributes.
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
