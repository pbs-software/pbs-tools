\name{makePMRtables}
\alias{makePMRtables}
\title{Make pmr Tables for Surveys}
\description{
  Create \code{.csv} files containing (p, mu, rho) values for strata in 
  the specified surveys.
}
\usage{
makePMRtables(strSpp, surveys=list(qcss=c(1,2,3,121,167),
     wcvis=c(16,70,126), wqcis=c(79,122,129)), 
     qName="gfb_pmr.sql", path=.getSpath()) 
}
\arguments{
  \item{strSpp}{string specifying species code used in \code{GFBioSQL}.}
  \item{surveys}{named list object containing vectors of survey ID numbers, 
    where the names describe the surveys (e.g., \code{qcss} = Queen Charlotte Sound 
    Synoptic; \code{wcvis} = West Coast Vancouver Island Synoptic; \code{wqcis} = 
    West Queen Charlotte Islands Synoptic).}
  \item{qName}{string specifying name of the SQL query.}
  \item{path}{path leading to the SQL code \code{qName}.}
}
\details{
  The code scrolls through the survey IDs, issuing sequential SQL queries to the DFO
  database \code{GFBioSQL}, and collects the debris in \code{.csv} files, one for each
  name in the list \code{surveys}.
}
\references{
  Schnute, J.T. and R. Haigh. (2003) A simulation model for designing 
  groundfish trawl surveys. \emph{Canadian Journal of Fisheries and 
  Aquatic Sciences} \bold{60}, 640--656.
}
\author{ 
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{ 
  \code{\link[PBStools]{calcPMR}}, \code{\link[PBStools]{makeCATtables}} \cr
  Available SQL queries: \code{\link[PBStools]{SQLcode}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(strSpp="451",dfo=FALSE) {
  if (dfo) makePMRtables(strSpp)
  else showMessage("If connected to DFO network, set argument 'dfo=TRUE'")
  invisible() }
pbsfun()
})
}
\keyword{data}
\keyword{utilities}
