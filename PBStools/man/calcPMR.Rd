\name{calcPMR}
\alias{calcPMR}
\title{Calculate (p, mu, rho) from a Sample Population}
\description{
  Calculate \code{p}, \code{mu}, and \code{rho} values for a sample 
  population.
}
\usage{
calcPMR(x, na.value=NULL)
}
\arguments{
  \item{x}{vector: sample population.}
  \item{na.value}{specify a value to assign to \code{NA} values in the sample; 
    the default \code{NULL} means that \code{NA} values will be removed before the calculation.}
}
\details{
  This function calculates the binomial-gamma parameters of a sample,
  as described by Schnute and Haigh (2003): \cr
  \code{p} = proportion of zero values, \cr
  \code{mu} = the mean of the non-zero values, \cr 
  \code{rho} = the CV of the non-zero values.
}
\value{
  A named vector with four elements: \code{n} (sample size), \code{p}, 
  \code{mu}, and \code{rho}.
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
  \code{\link{getPMR}}, \code{\link{sampBG}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(P=list(n=1000,p=0.25,mu=1,rho=0.5)) {
  unpackList(P)
  x = sampBG(n,p,mu,rho) # create a sample population
  y = calcPMR(x)
  cat("True parameters for sampling the binomial-gamma distribution:\n")
  print(unlist(P))
  cat("Estimated parameters of the sampled binomial-gamma distribution:\n")
  print(y)
  invisible() }
pbsfun()
})
}
\keyword{ data }
\keyword{ utilities }
\concept{M04_Survey}
