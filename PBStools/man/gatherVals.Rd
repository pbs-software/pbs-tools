\name{gatherVals}
\alias{gatherVals}
\title{
  Gather Values from Multi-Column Objects
}
\description{
  Gathers data from multiple columns and organises them into key-value pairs.
  This function essentially acts as a replacement for \code{tidyr::gather}.
}
\usage{
gatherVals(x, columns)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Data object (only data frames tested thus far).}
  \item{columns}{Column numbers within the data object to gather.
    If not specified, the function gathers all columns.}
}
\details{
  The routine simply loops through each column specified, extracts the column 
  name to use as the descriptor in the \code{key} field, and extracts the column
  values to populate the \code{value} field.
}
\value{
  A two-field (two-column) data frame with fields named \code{key} and
  \code{value}. The number of rows in the output data frame will be the number
  of columns (\code{length(columns)}) multiplied by the number of records
  (\code{nrow(x)}).
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2021-06-15}\cr
}
\seealso{
  \code{\link[PBStools]{biteData}},
  \code{\link[PBStools]{crossTab}},
  \code{\link[PBStools]{spooler}},
  \code{\link[PBStools]{texArray}}
}
\examples{
local(envir=.PBStoolEnv,expr={
  cocoa = gatherVals(swiss[1:3,])
  print(cocoa)
})
}
\keyword{manip}
\concept{M01_Utility}

