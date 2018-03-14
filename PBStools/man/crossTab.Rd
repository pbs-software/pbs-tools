\name{crossTab}
\alias{crossTab}
\title{
  Run a Cross Tabulation
}
\description{
  Run a cross tabulation on a data frame, summarizing z-values
  by specified 'factors'.
}
\usage{
crossTab(x=PBSdat, y=c("year","major"), z="landed", 
   func=function(x){sum(x)/1000.}, na.val=99, hadley=FALSE, ...)
}
\arguments{
  \item{x}{\code{data.frame} -- object with data for cross tabulation.}
  \item{y}{\code{character} -- field names in \code{x} that contain factor-like levels to build a contigency table.}
  \item{z}{\code{character} -- field name in \code{x} that contains the values to summarize in the contigency table.}
  \item{func}{\code{function} -- to summarize \code{z} at each factor combination found in \code{y}.}
  \item{na.val}{\code{numeric} -- convert \code{NA} values in factors \code{y} to a dummy value.}
  \item{hadley}{\code{logical} -- if \code{TRUE}, use Hadley Wickham's \code{reshape} 
    package functions for the cross-tabulation (deprecated).}
  \item{...}{additional arguments to pass to \code{reshape}'s function \code{cast}.}
}
\details{
  This function requires the R package \code{reshape}, and uses the function
  \code{melt.data.frame} and \code{cast} therein (only if \code{hadley=TRUE}).
}
\value{
  A cross tabulation of \code{y} with values \code{z} summarized by \code{func}.
  The function \code{crossTab} is useful for summarizing data query results (\emph{e.g.}, total catch 
  by year and PMFC major area) when exploring / debugging functions like \code{buildCatch}.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus: Institute of Ocean Sciences (IOS), Sidney BC}\cr
  Last modified \code{Rd: 2018-03-14}
}
\seealso{
  \code{\link{buildCatch}}, \code{\link{getCatch}}, \code{\link{getData}} \cr
  \pkg{reshape}: \code{\link[reshape]{melt.data.frame}}, \code{\link[reshape]{cast}}
}
\keyword{manip}

