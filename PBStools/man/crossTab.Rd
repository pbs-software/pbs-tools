\name{crossTab}
\alias{crossTab}
\title{
  Run a Cross Tabulation
}
\description{
  Run a cross tabulation on a data frame, summarizing z-values
  by specified \sQuote{factors}.
}
\usage{
crossTab(x=PBSdat, y=c("year","major"), z="landed", 
   func=function(x){sum(x)/1000.}, 
   na.val=999, na.str="NA", hadley=FALSE, ...)
}
\arguments{
  \item{x}{\code{data.frame} -- object with data for cross tabulation.}
  \item{y}{\code{character} -- field names in \code{'x'} that contain factor-like levels to build a contigency table.}
  \item{z}{\code{character} -- field name in \code{'x'} that contains the values to summarize in the contigency table.}
  \item{func}{\code{function} -- to summarize \code{'z'} at each factor combination found in \code{'y'}.}
  \item{na.val}{\code{numeric} -- convert \code{'NA'} values in numeric factors \code{'y'} to a dummy value.}
  \item{na.str}{\code{character} -- convert empty strings \code{""} or \code{'NA'} values in character factors \code{'y'} to a dummy value.}
  \item{hadley}{\code{logical} -- if \code{TRUE}, use Hadley Wickham's \code{'reshape'} 
    package functions for the cross-tabulation (deprecated but still available due to its robust nature).}
  \item{...}{additional arguments to pass to \code{reshape}'s function \code{'cast'}.}
}
\details{
  Performs a cross-tabulation using summary factors which occur as numeric and/or 
  character fields in the data frame \code{'x'} using expression manipulation
  to populate a summary array.

  If \code{'hadley=TRUE'}, this function requires the R package \code{reshape},
  and uses the function \code{'melt.data.frame'} and \code{'cast'}.
}
\value{
  A cross tabulation of \code{'y'} with values \code{'z'} summarized by \code{'func'}.
  The function \code{'crossTab'} is useful for summarizing data query results (e.g. total catch 
  by year and PMFC major area) when exploring / debugging functions like \code{'buildCatch'}.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2019-03-22}
}
\seealso{
In \pkg{PBStools}:\cr
  \code{\link[PBStools]{buildCatch}},
  \code{\link[PBStools]{countVec}},
  \code{\link[PBStools]{getCatch}},
  \code{\link[PBStools]{getData}} \cr
In \pkg{reshape}:\cr
  \code{melt.data.frame},
  \code{cast}
}
\keyword{manip}
\concept{M01_Utility}

