\name{calcOccur}
\alias{calcOccur}
\title{Calculate Percent Occurrence of EventData in PolySet}
\description{
  Calculate the percent occurrence of \code{EventData} in a \code{PolySet}.
}
\usage{
calcOccur(polyset="qcb", events, wt=1, mess=FALSE, ioenv=.GlobalEnv)
}
\arguments{
  \item{polyset}{name of a \code{PolySet} object (e.g., \code{qcb}).}
  \item{events}{name of an \code{EventData} object.}
  \item{wt}{weight to give each fishing event (default = \code{1}); 
    weight \code{wt} can also be a string specifying a field in \code{events}
    (e.g., \code{"gcat"} might be the catch of the species).}
  \item{mess}{logical: if \code{TRUE}, display the percent occurrence
    on the current device.}
  \item{ioenv}{input/output environment for function input data and output results.}
}
\details{
  The function calculates the percent occurrence of events in a 
  set of polygons. The \code{PolySet} must have a \code{PolyData} 
  attribute with a category field called \code{label}.
}
\value{
  A vector of percent occurrences by the category \code{label} contained in 
  the \code{PolyData} attribute of \code{PolySet}. \cr
  Additionally, the global object \code{PBStool} is created and contains:
  \item{pd}{the \code{PolyData} set modified.}
  \item{loc}{the \code{LocationSet} created by \code{findPolys()}.}
  \item{eidpid}{a vector showing number of events in each PID.}
  \item{occur}{a vector of total occurrences by the category \code{label}.}
  \item{poccur}{a vector of percent occurrences by the category \code{label}.}
}
\author{ 
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  data(testdatC,envir=.PBStoolEnv)
  dat=testdatC[!is.na(testdatC$X) & !is.na(testdatC$Y),]
  edat=as.EventData(dat,projection="LL",zone=9)
  calcOccur(polyset="qcb", events="edat", mess=TRUE)
  invisible() }
pbsfun()
})
}
\seealso{
  \code{\link{calcHabitat}}, \code{\link{calcSurficial}}, \code{\link{qcb}} \cr
  \pkg{PBSmapping}: \code{\link[PBSmapping]{findPolys}}
}
\keyword{hplot}
