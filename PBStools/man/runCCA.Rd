\name{runCCA}
\alias{runCCA}
\title{
  Run Catch-Curve Analysis on Age Frequency Data
}
\description{
  Perform a catch-curve analysis using a model formulated by 
  Schnute and Haigh (2006). The code allows a user to perform 
  both frequentist (NLM) and Bayesian (BRugs) analyses.
}
\usage{
runCCA(fnam="nage394", hnam=NULL, ioenv=.GlobalEnv, ...)
}
\arguments{
  \item{fnam}{string specifying file name}
  \item{hnam}{string name of a history file}
  \item{ioenv}{input/output environment for function input data and output results.}
  \item{...}{additional arguments (not currently used).}
}
\details{
  The function opens up a notebook GUI with two tabs named
  \dQuote{NLM} and \dQuote{BRugs}. The user needs to find a
  decent modal fit using \code{NLM} before attempting to run 
  the Bayesian analysis, which employs Markov chain Monte Carlo (MCMC) 
  techniques using the Gibbs sample algorithm.

  Currently, the NLM fit can use one of three sampling distributions:
  multinomial, Dirichlet, and logistic-normal. The BRugs model is only 
  coded for four cases using the Dirichlet distribution:

  \code{M.....}case 1: survival only, which depends only on mortality, \cr
  \code{MS....}case 2: survival and selectivity only, \cr
  \code{MA....}case 3: survival and recruitment anomalies only, \cr
  \code{MSA...}case 4: survival, selectivity, and recruitment anomalies.

  The Bayesian model is coded in the OpenBUGS language and 
  can be found in the examples subdirectory \cr
  \code{library/PBStools/examples}.
}
\value{
  Output is primarily visual as directed by the GUI. 
  A number of objects are dumped to the R environment
}
\references{
  Schnute, J.T., and Haigh, R. (2007) 
  Compositional analysis of catch curve data, with an application to \emph{Sebastes maliger}. 
  \emph{ICES Journal of Marine Science} \bold{64}: 218--233.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\seealso{
  \code{\link[PBStools]{plotProp}}, \code{\link[PBSdata]{nage394}}
}
\keyword{hplot}
\keyword{optimize}
\keyword{iteration}

