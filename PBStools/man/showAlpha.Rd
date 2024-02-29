\name{showAlpha}
\alias{showAlpha}
\title{Show Quantile Confidence Levels (alpha) for Bootstraps.}
\description{
  Show quantile confidence levels \eqn{\alpha} for bootstrapped biomass.
}
\usage{
showAlpha(lims=c("emp","bca"))
}
\arguments{
  \item{lims}{bootstrap confidence limit types from choices in \code{bootBG}.}
}
\details{
  This function assumes that \code{bootBG()} has been run and that the data
  list object \code{PBStool} exists in the global position. The figure generated
  follows the format of Figure 3 in Schnute and Haigh (2003). The panels show: \cr
  (a) histogram of bootstrapped biomass; \cr
  (b) cumulative probability distribution of bootstrapped biomass; \cr
  (c) normal quantiles of the estimates \eqn{\hat{B}}{B_hat}; \cr
  (d) observed probabilities that the true value \eqn{B} lies in the \eqn{j^{th}}{jth} confidence interval; \cr
  (e) \eqn{\hat{\alpha}_j}{\alpha_hat[j]} \emph{vs.} \eqn{\alpha_j}{\alpha[j]} for chosen confidence limit types.
}
\value{
  No value is explicitly returned by the function. Objects generated are appended to 
  the global list object \code{PBStool}:
  \item{plev}{vector: probability levels used in the \code{bootBG} analysis.}
  \item{xy}{histogram: output from the last \eqn{\alpha} histogram.}
  \item{xcount}{vector: count of \eqn{\alpha} values in Confidence Interval (CI) for the last graph.}
  \item{pobs}{vector: probability of \eqn{\alpha} values in CI bins for the last graph.}
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
  Last modified \code{Rd: 2024-02-27}
}
\seealso{ 
  \code{\link{bootBG}}, \code{\link{getPMR}}, 
  \code{\link{calcPMR}}, \code{\link{sampBG}}
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
\keyword{ data }
\keyword{ utilities }
\concept{M04_Survey}
