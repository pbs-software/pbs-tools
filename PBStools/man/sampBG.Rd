\name{sampBG}
\alias{sampBG}
\title{Sample from the Binomial-Gamma Distribution}
\description{
  Take \eqn{n} random samples from the binomial-gamma distribution \eqn{BG(p, \mu, \rho)}.
}
\usage{
sampBG(n, p=0.25, mu=1, rho=0.5)
}
\arguments{
  \item{n}{number of random samples to draw.}
  \item{p}{proportion of zero values.}
  \item{mu}{mean of the non-zero values.}
  \item{rho}{coefficient of variation (CV) of the non-zero values.}
}
\details{
  This function generates \eqn{n} random samples from the binomial \eqn{B(p)} and 
  the gamma \eqn{G(\mu, \rho)} distributions, and returns the product of the two.
}
\value{
  A vector of length \code{n} of values drawn from the binomial-gamma
  distribution.
}
\references{
  Schnute, J.T. and R. Haigh. (2003) A simulation model for designing 
  groundfish trawl surveys. \emph{Canadian Journal of Fisheries and 
  Aquatic Sciences} \bold{60}, 640--656.
}
\author{ 
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\seealso{ 
  \code{\link{calcPMR}}, \code{\link{getPMR}}
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
