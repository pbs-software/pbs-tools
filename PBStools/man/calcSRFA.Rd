\name{calcSRFA}
\alias{calcSRFA}
\title{Get Slope Rockfish Assessment Areas}
\description{
  Determine slope rockfish assessment areas from combinations 
  of PMFC major and minor areas as well as locality codes for 
  fishing grounds within PMFC minor areas.
}
\usage{
calcSRFA(major, minor=NULL, loc=NULL, subarea=FALSE) 
}
\arguments{
  \item{major}{numeric: Pacific Marine Fisheries Commission major area codes
    or matrix/data frame with three columns containing codes \code{major}, 
    \code{minor}, and \code{loc}}
  \item{minor}{numeric: Pacific Marine Fisheries Commission minor area codes}
  \item{loc}{numeric: locality codes denoting fishing grounds within PMFC minor areas}
  \item{subarea}{logical: if \code{TRUE} then determine SRF subareas 
    (Queen Charlotte Sound gullies) instead of SRF areas.}
}
\details{
  The function either accepts three vectors (\code{major}, \code{minor}, 
  \code{loc}) of equal length or one matrix/data frame \code{major} with 
  three columns corresponding to the first three arguments.
}
\value{
  A character vector specifying slope rockfish assessment areas/subreas.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-27}\cr
}
\seealso{ 
  \code{\link[PBSdata]{srfa}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  dat=data.frame(major=c(3:7,9,9),
    minor=c(23,26,11,8,8,35,31),loc=c(1,1,1,3,6,1,1))
  sdat=cbind(dat,srfa=calcSRFA(dat),srfs=calcSRFA(dat,subarea=TRUE))
  print(sdat); invisible() }
pbsfun()
})
}
\keyword{methods}
\keyword{utilities}
\concept{M05_Spatial}
