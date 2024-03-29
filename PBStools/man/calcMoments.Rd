\name{calcMoments}
\alias{calcMoments}
\title{Calculate Survey Moments from Raw Data}
\description{
  Calculate survey moments, including relative biomass,
  for a species from raw survey data.
}
\usage{
calcMoments(strSpp="396", survID=c(1,2,3,121,167))
}
\arguments{
  \item{strSpp}{string specifying species code (\code{species$code}).}
  \item{survID}{numeric survey ID assigned to each survey by the GFBio team.}
}
\details{
  The function uses \code{getData} to calculate survey specs using the 
  SQL queries: \cr
  \code{gfb_survey_stratum.sql} \cr
  \code{gfb_survey_catch.sql}. \cr \cr
  These queries are modified SQL procedures originally designed by Norm Olsen to build SQL tables: \cr
  \code{BOOT_HEADER} and \code{BOOT_DETAIL}.
}
\value{
  Invisibly returns a list of survey moments.
  Additionally, a global list object \code{PBStool} provides the following:
  \item{doors}{Vector of default doorspread values for each survey from \code{BOOT_DEFAULTS}.}
  \item{speed}{Vector of default vessel trawling speeds for each survey from \code{BOOT_DEFAULTS}.}
  \item{moments}{List of moment calculations by strata, listed by \code{survID}.}
}
\author{ 
  \href{mailto:norm.olsen@dfo-mpo.gc.ca}{Norm Olsen}, Program Head -- Surveys\cr
  Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-27}\cr

}
\seealso{
  \code{\link[PBStools]{calcPMR}}, \code{\link[PBStools]{showIndices}} \cr
  Available SQL queries: \code{\link[PBStools]{SQLcode}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(strSpp="455", survID=c(1,121),dfo=FALSE){
  if (dfo) {
    momList=calcMoments(strSpp,survID)
    print(momList) }
  else showMessage("If connected to DFO network, set argument 'dfo=TRUE'")
  invisible() }
pbsfun()
})
}
\keyword{hplot}
\concept{M04_Survey}
