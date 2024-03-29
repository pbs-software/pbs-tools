\name{bootBG}
\alias{bootBG}
\title{Bootstrap Biomass Using Binomial-Gamma Population Parameters}
\description{
  Bootstrap stratified biomass estimates using random draws from the 
  binomial-gamma distribution using strata population parameters supplied.
}
\usage{
bootBG(dat=pop.pmr.qcss, K=100, S=100, R=500, 
   SID=NULL, ci=seq(0.1,0.9,0.1), 
   lims=c("emp","norm","basic","perc","bca"), allo=0, ioenv=.GlobalEnv)
}
\arguments{
  \item{dat}{data frame with fields: \cr
    \code{SID} = survey ID number assigned by the GFBio team; \cr
    \code{h} = strata ID (alpha and/or numeric); \cr
    \code{p} = proportion of zero measurements; \cr
    \code{mu} = mean of nonzero measurements; \cr
    \code{rho} = CV of nonzero measurements; \cr
    \code{A} = bottom area (sq.km) of each stratum; \cr
    \code{n} = actual number of tows performed to derive (p,mu,rho); \cr
    \code{k} = preference/weighting of new tows. }
  \item{K}{number of proposed survey tows.}
  \item{S}{number of simulations.}
  \item{R}{number of bootstrap replicates.}
  \item{SID}{survey ID to simulate and bootstrap.}
  \item{ci}{confidence intervals for simulation; e.g., 
    \code{ci=c(0.90,0.95)} gives confidence limits \code{(0.025,0.05,0.95,0.975)}.}
  \item{lims}{bootstrap confidence limit types to compute (see \code{boot::boot.ci}).}
  \item{allo}{tow allocation scheme: \code{0} = existing, \code{1} = optimal.}
  \item{ioenv}{input/output environment for function input data and output results.}
}
\details{
  This function depends on the R package \pkg{boot}. The user supplies
  a data frame \code{dat} summarizing a survey's strata population 
  parameters, as outlined in Schnute and Haigh (2003). These parameters are 
  used to estimate the survey population using random draws from the 
  binomial-gamma distribution.
}
\value{
  No value is explicitly returned by the function. A global list object \code{PBStool}
  provides results of the analysis:
  \item{dat}{data frame: a subset from the user input that contains 
    population parameters for each unique stratum.}
  \item{bgtab}{matrix: parameter values and moment estimates for each stratum.}
  \item{dStab}{matrix: sampled density estimates for each stratum.}
  \item{BStab}{matrix: sampled biomass estimates for each stratum.}
  \item{bgsamp}{list: final simulated set of densities sampled from the binomial-gamma
    distribution and listed by stratum.}
  \item{Bboot}{list: results of \code{boot::boot()} listed for each simulation.}
  \item{Bqnt}{array: bootstrap quantiles for each simulation and type of confidence
    interval calculation.}
  \item{Best}{vector: biomass estimate for each simulation.}
  \item{Bobs}{scalar: observed biomass estimate (moment calculation).}
  \item{group}{vector: stratum grouping showing allocation of \code{K} tows.}
}
\references{
  Schnute, J.T. and R. Haigh. (2003) A simulation model for designing 
  groundfish trawl surveys. \emph{Canadian Journal of Fisheries and 
  Aquatic Sciences} \bold{60}, 640--656.
}
\author{ 
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-27}\cr
}
\seealso{
  In package \pkg{PBStools}:\cr
    \code{\link[PBStools]{getPMR}},
    \code{\link[PBStools]{calcPMR}}, 
    \code{\link[PBStools]{sampBG}},
    \code{\link[PBStools]{showAlpha}}\cr
  In package \pkg{boot}:\cr
    \code{\link[boot]{boot.ci}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(SID=1,S=50){
  bootBG(dat=pop.pmr.qcss,SID=SID,S=S)
  showAlpha()
  invisible() }
pbsfun()
})
}
\keyword{data}
\keyword{utilities}
\concept{M04_Survey}
