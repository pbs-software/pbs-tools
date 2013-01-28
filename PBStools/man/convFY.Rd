\name{convFY}
\alias{convFY}
\title{
  Convert Dates into Fishing/Fiscal Years
}
\description{
  Convert a vector of dates into a vector of fishing or fiscal years
  based on a specified month to start the fishing/fiscal year.
}
\usage{
convFY(x, startM=4)
}
\arguments{
  \item{x}{vector of date limits with class \code{POSIXt} or a character vector of dates
    specified in the format \code{"yyyy-mm-dd"} that can be converted to dates.}
  \item{startM}{scalar integer from \code{1} to \code{12} specifying 
    which month starts the fishing/fiscal year.}
}
\value{
  Returns a numeric vector of fishing/fiscal years of equal length to the input vector
  of dates. The output vector has names representing year-month \code{"yyyy-mm"}.
  If \code{startM=1}, the output dates also represent calendar years.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\seealso{
  \code{\link{convYM}}, \code{\link{convYP}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function() {
  cat("Fishing years starting in April\n")
  print(convFY(paste("2009-",pad0(1:12,2),"-15",sep="")))
  invisible() }
pbsfun()
})
}
\keyword{manip}

