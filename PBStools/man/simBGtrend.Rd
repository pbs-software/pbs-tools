\name{simBGtrend}
\alias{simBGtrend}
\title{Simulate Population Projection Based on Prior Binomial-Gamma Parameters.}
\description{
  Simulate a population projection based on prior binomial-gamma parameters
  (\eqn{p}, \eqn{\mu}, \eqn{\rho}), and display the results.
}
\usage{
simBGtrend(pmr=pop.pmr.qcss, Npred=15, genT=NULL, Nsim=1,
   yrs=NULL, alpha=1, ioenv=.GlobalEnv)
}
\arguments{
  \item{pmr}{data frame with fields: \cr
    \code{SID} = survey ID number assigned by the GFBio team; \cr
    \code{h} = strata ID (alpha and/or numeric); \cr
    \code{p} = proportion of zero measurements; \cr
    \code{mu} = mean of nonzero measurements; \cr
    \code{rho} = CV of nonzero measurements; \cr
    \code{A} = bottom area (sq.km) of each stratum; \cr
    \code{n} = actual number of tows performed to derive (p,mu,rho); \cr
    \code{k} = preference/weighting of new tows. }
  \item{Npred}{number of predicted years past the observed year.}
  \item{genT}{generation time; if specified, \code{Npred} is automatically set to 3 times \code{genT}.}
  \item{Nsim}{number of population simulations.}
  \item{yrs}{years of the observed parameters (normally, \code{getPMR()} attaches 
    this information to the \code{pmr} data frame as an attribute called \code{survey}).}
  \item{alpha}{coefficient to weight past observations of population parameters:
    \code{alpha=0} applies equal weighting, increasingly positive \code{alpha} values 
    apply exponentially heavier weights to latter observations, and negative \code{alpha}
    values weight earlier observations more heavily.}
  \item{ioenv}{input/output environment for function input data and output results.}
}
\details{
  This function attempts to simulate a population past the observed year by 
  randomly sampling the binomial-gamma distribution \eqn{BG(p, \mu, \rho)} using weighted prior values
  of the population parameters \eqn{(p, \mu, \rho)}.
}
\value{
  No value is explicitly returned by the function. Objects generated are appended to 
  the global list object \code{PBStool} (which contains the last call to \code{bootBG}):
  \item{pmrhist}{array: the \code{p}, \code{mu}, \code{rho} values used for each year of each simulation.}
  \item{Btrend}{array: estimated annual biomass and CI values for each simulation.}
  \item{Bmean}{data frame: mean annual biomass and CI limits of the multiple simulations.}
  \item{Bseen}{data frame: mean annual biomass and CI limits of the observed surveys.}
  \item{brR}{data frame: for each simulation (one record): \cr
    slope \code{b} of the linear trend through annual indices; \cr
    annualized rate of change \code{r} implied by the slope \code{b}; \cr
    accumulated rate of change \code{R} implied by the slope \code{b}.}
}
\references{
  Schnute, J.T. and R. Haigh. (2003) A simulation model for designing 
  groundfish trawl surveys. \emph{Canadian Journal of Fisheries and 
  Aquatic Sciences} \bold{60}, 640--656.

  Schnute, J., Haigh, R., Krishka, B., Sinclair, A. and Starr, P. (2004)
  The British Columbia longspine thornyhead fishery: analysis of survey and 
  commercial data (1996--2003). \emph{Canadian Science Advisory Secretariat, 
  Research Document} \bold{2004/059}, 75 pp.
}
\author{ 
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-27}
}
\seealso{ 
  \code{\link{getPMR}}, \code{\link{calcPMR}}, \code{\link{sampBG}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(Nsim=1,alpha=1) {
  simBGtrend(Nsim=Nsim,alpha=alpha)
  invisible() }
pbsfun()
})
}
\keyword{ data }
\keyword{ utilities }
\concept{M04_Survey}
