\name{coalesce}
\alias{coalesce}
\title{
  R Equivalent of SQL Coalesce
}
\description{
  Eric Vallabh Minikel:\cr
  \emph{In SQL there is a function called \code{'coalesce'}, which accepts
  any number of columns and returns the first non-null value in each row,
  reading across the columns in the order they're passed in.
  This is super useful when you're trying to relationalize a not-so-relational
  database, where a desired value might be found in one column or in another.}
}
\usage{
coalesce(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{\code{numeric|character} -- any number of R vectors of the same length.}
}
\details{
  As long as input vectors have the same length, they can be a variety of 
  classes (e.g., \code{numeric}, \code{character}, \code{logical}, etc.).

  Attributes \code{'author'} and \code{'source'} are automatically attached to
  reflect the open-source contribution by Eric Vallabh Minikel. A user can 
  send the output from \code{'coalesce'} to the \code{base} package function 
  \code{'as.vector'} to remove the attributes.
}
\value{
   Returns the first non-NA value in each position of any number of R vectors with same length.
}
\references{
\href{https://www.cureffi.org/2013/05/02/r-equivalent-of-sql-coalesce/}{R equivalent of SQL coalesce}
}
\author{
  \href{http://www.cureffi.org/about/}{Eric Vallabh Minikel} is on a lifelong quest to prevent prion disease.
  He is a scientist based at the Broad Institute of MIT and Harvard.

  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2019-10-02}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{biteData}},
  \code{\link[PBStools]{countVec}},
  \code{\link[PBStools]{inWord}},
  \code{\link[PBStools]{prime}},
  \code{\link[PBStools]{revStr}},
  \code{\link[PBStools]{scaleVec}},
  \code{\link[PBStools]{stdConc}},
  \code{\link[PBStools]{texThatVec}},
  \code{\link[PBStools]{ttget}}
}
\examples{
local(envir=.PBStoolEnv,expr={
  lettuce = letters
  lettuce[prime(2:26)] = NA
  print(coalesce(lettuce,LETTERS))
})
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{logic}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M01_Utility}
\concept{SQL}
% Use only one concept per line.
