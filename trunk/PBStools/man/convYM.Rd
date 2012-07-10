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
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\seealso{
  \code{\link{convYP}}, \code{\link{convFY}}
}
\examples{
pbsfun=function() {
  cat("Year-month labels between the specified date limits\n")
  print(convYM(c("2009-07-01","2011-11-11")))
  invisible() }
pbsfun()
}
\keyword{manip}

