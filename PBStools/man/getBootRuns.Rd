\name{getBootRuns}
\alias{getBootRuns}
\title{
  Get Survey Bootstrap Results
}
\description{
  Get the survey bootstrap results from \code{GFBioSQL}.
}
\usage{
getBootRuns(strSpp)
}
\arguments{
  \item{strSpp}{character string code for a species}
}
\details{
  This function calls the \code{SQL} query \code{gfb_boot.sql} to
  download the bootstrap results for the selected species from
  \code{GFBioSQL} tables \code{BOOT_HEADER} and \code{BOOT_DETAIL}.
  The bootstrap tables are maintained by Norm Olsen, PBS.
}
\value{
  A data frame containing the bootstrapped biomass estimates and 
  confidence limits for \code{strSpp} from all surveys. The data
  frame is ordered by \code{date} and \code{bootID}.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-27}
}
\seealso{
  \code{\link[PBStools]{getPMR}}, \code{\link[PBStools]{showIndices}} \cr
  Available SQL queries: \code{\link[PBStools]{SQLcode}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(strSpp="057",dfo=FALSE) {
  if (dfo) {
    bootTab=getBootRuns(strSpp)
    print(bootTab) }
  else showMessage("If connected to DFO network, set argument 'dfo=TRUE'")
  invisible() }
pbsfun()
})
}
\keyword{ data }
\concept{M04_Survey}
