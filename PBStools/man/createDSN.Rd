\name{createDSN}
\alias{createDSN}
\title{Create User DSNs for PBS Groundfish Databases}
\description{
  Create a suite of User DSNs for PBS groundfish databases, currently 
  hosted on the server \code{DFBCV9TWVASP001}.
}
\usage{
createDSN(trusted=TRUE) 
}
\arguments{
  \item{trusted}{logical: if \code{TRUE}, allow SQL Server to use a trusted DFO login ID.}
}
\details{
  \code{createDSN()} automatically creates (overwrites) six User DSNs:\cr
  \code{GFBioSQL}\cr
  \code{GFCatch}\cr
  \code{GFCruise}\cr
  \code{PacHarvest}\cr
  \code{PacHarvHL}\cr
  \code{PacHarvSable}\cr\cr
  In XP, the new User DSN can be seen by navigating to:\cr
  <Control Panel><Administrative Tools><Data Sources (ODBC)>.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2021-06-15}\cr
}
\seealso{ 
  \code{\link{confODBC}}, \code{\link{runModules}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(os=.Platform$OS.type,dfo=FALSE) {
  if (os=="windows" && dfo) createDSN() 
  else showMessage("Only functional for DFO personnel using Microsoft Windows OS")
  invisible() }
pbsfun()
})
}
\keyword{data}
\keyword{utilities}
\concept{M01_Utility}
