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
crossTab(x=PBSdat, y=c("year","major"), 
     z="landed", func=function(x){sum(x)/1000.}, ...)
}
\arguments{
  \item{x}{data frame for cross tabulation.}
  \item{y}{field names in \code{x} that contain factor-like levels to build a contigency table.}
  \item{z}{field name in \code{x} that contains the values to summarize in the contigency table.}
  \item{func}{function to summarize \code{z} at each factor combination found in \code{y}.}
  \item{...}{additional arguments to pass to \pkg{reshape}'s function \code{cast}.}
}
\details{
  This function requires the R package \pkg{reshape}, and uses the function
  \code{melt.data.frame} and \code{cast} therein.
}
\value{
  A cross tabulation of \code{y} with values \code{z} summarized by \code{func}.
  The function \code{crossTab} is useful for summarizing data query results (\emph{e.g.}, total catch 
  by year and PMFC major area) when exploring / debugging functions like \code{buildCatch}.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\seealso{
  \code{\link{buildCatch}}, \code{\link{getCatch}}, \code{\link{getData}} \cr
  \pkg{reshape}: \code{\link[reshape]{melt.data.frame}}, \code{\link[reshape]{cast}}
}
\keyword{manip}

