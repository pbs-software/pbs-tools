\name{getPMR}
\alias{getPMR}
\title{Get (p, mu, rho) Values for Survey Strata}
\description{
  Get \code{p}, \code{mu}, and \code{rho} values for populations by strata from 
  survey data housed in \code{GFBioSQL} using the query \code{gfb_pmr.sql} 
  stored in \code{library/PBStools/sql}.
}
\usage{
getPMR(strSpp="396", survID=c(1,2,3,121,167))
}
\arguments{
  \item{strSpp}{string specifying species code (see \code{species$code}).}
  \item{survID}{numeric survey ID assigned to each survey by the GFBio team.}
}
\details{
  The function executes the SQL query \code{gfb_pmr.sql} for each survey ID \code{i}: \cr
  \code{getData("gfb_pmr.sql","GFBioSQL",strSpp,path=.getSpath(),surveyid=i)} \cr
  and collects the results in a data frame.
}
\value{
  A data frame where each row describes the population parameters (\code{p,mu,rho}) 
  for each unique survey ID and survey stratum. Attributes include \code{fqt} (query name), 
  \code{strSpp} (string species code), \code{h} (strata), \code{surveys.all}
  (data frame from a call to \code{getBootRuns}), and \code{surveys.selected}
  (records from \code{surveys.all} with the specified \code{survID}).

  Additionally, a global list object \code{PBSfish} contains: \cr
  \item{pmrTab }{the data frame output object described above.}
  \item{sTime }{system and elapsed time to get the \code{pmr} data.}
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
  \code{\link[PBStools]{getBootRuns}}, \code{\link[PBStools]{calcPMR}},  
  \code{\link[PBStools]{sampBG}},  \code{\link[PBStools]{showIndices}} \cr
  Available SQL queries: \code{\link[PBStools]{SQLcode}}
}
\examples{
pbsfun=function(dfo=FALSE){
  if (dfo) {
    pmrTab=getPMR(strSpp="396", survID=c(1,121))
    print(pmrTab) }
  else showMessage("If connected to DFO network, set argument 'dfo=TRUE'")
  invisible() }
pbsfun()
}
\keyword{ data }
\keyword{ utilities }
