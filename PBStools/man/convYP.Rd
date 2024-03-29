\name{convYP}
\alias{convYP}
\title{
  Convert Dates into Binned Year Periods
}
\description{
  Convert a vector of dates into one of year-periods through binning
  the dates into intervals specified in days.
}
\usage{
convYP(x, ndays=90)
}
\arguments{
  \item{x}{date vector with class \code{POSIXt} or a character vector of dates
    specified in the format \code{"yyyy-mm-dd"} that can be converted to dates.}
  \item{ndays}{integer value specifying a time interval in days to partition a year.}
}
\details{
  The routine actually fudges the integer day interval to a real number that 
  will create even breaks.
}
\value{
  Returns a vector (with same length as input date vector) that specifies dates
  as real numbers where the integer part is the year and the fraction part is the 
  proportion of the year corresponding to the bin's right-most value.
  Each element of the vector also has a name that specifies the year and bin number.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2021-06-15}\cr
}
\seealso{
  \code{\link[PBStools]{convFY}},
  \code{\link[PBStools]{convYM}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function() {
  x=paste("2009-",c("03-31","06-30","09-30","12-30"),sep="")
  cat("Year periods based on numeric value of input dates\n")
  print(convYP(x,30))
  invisible() }
pbsfun()
})
}
\keyword{manip}
\concept{M01_Utility}

