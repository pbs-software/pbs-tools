\name{convCT}
\alias{convCT}
\title{
  Convert crossTab Object to Matrix or Data Frame
}
\description{
  Convert an object (class \code{cast_df}) generated by \code{crossTab}, which
  uses \pkg{reshape} functions, into a regular matrix or data frame.
}
\usage{
convCT(CT, fn=as.matrix, colAsRowName=TRUE)
}
\arguments{
  \item{CT}{two-dimensional output from a call to the function \code{crossTab}.}
  \item{fn}{conversion function (either \code{as.matrix} or \code{as.data.frame}).}
  \item{colAsRowName}{logical: if \code{TRUE}, use the first column to name the rows, else retain the original row names.}
}
\details{
  The function \code{crossTab} returns a \pkg{reshape} \code{cast_df} object
  where the first field is the unique set from the first factor used to build
  the contingency (cross tabulation) table. Unique values of the second
  factor form additional fields in the crosstab table.

  This function essentially removes the first field and uses it to name the
  rows of the crosstab table.
}
\value{
  Returns a matrix or data frame if \code{fn=as.matrix} or \code{fn=as.data.frame}, respectively.
  Otherwise, returns the original object.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2021-06-15}\cr
}
\seealso{
  \code{\link{buildCatch}}, \code{\link{crossTab}}
}
\keyword{manip}
\concept{M01_Utility}

