\name{convYM}
\alias{convYM}
\title{
  Convert Date Limits into a Vector of Year-Months
}
\description{
  Convert a two-element vector of date limits into a vector of year-months
  where every month within the limits is represented.
}
\usage{
convYM(x)
}
\arguments{
  \item{x}{vector of date limits with class \code{POSIXt} or a character vector of dates
    specified in the format \code{"yyyy-mm-dd"} that can be converted to dates.}
}
\value{
  Returns an expanded character vector of ordered year-months \code{"yyyy-mm"}
  between and including the specified date limits. 
  This character vector can be used for labelling and other factor-like routines.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2021-06-15}\cr
}
\seealso{
  \code{\link{convYP}}, \code{\link{convFY}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function() {
  cat("Year-month labels between the specified date limits\n")
  print(convYM(c("2009-07-01","2011-11-11")))
  invisible() }
pbsfun()
})
}
\keyword{manip}
\concept{M01_Utility}

