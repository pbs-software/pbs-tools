\name{runCCA}
\alias{runCCA}
\alias{.runCCA.checkMon}
\alias{.runCCA.compileMod}
\alias{.runCCA.diag.panel}
\alias{.runCCA.evalMod}
\alias{.runCCA.getChains}
\alias{.runCCA.getData"}
\alias{.runCCA.getLayout}
\alias{.runCCA.model}
\alias{.runCCA.objFun}
\alias{.runCCA.plotACF}
\alias{.runCCA.plotData}
\alias{.runCCA.plotDens}
\alias{.runCCA.plotHist}
\alias{.runCCA.plotNLM}
\alias{.runCCA.plotPairs}
\alias{.runCCA.plotTrace}
\alias{.runCCA.setVals}
\alias{.runCCA.updateMod}
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
.runCCA.checkMon()
.runCCA.compileMod()
.runCCA.diag.panel(x, ...)
.runCCA.evalMod()
.runCCA.getChains(chains=BRugs::samplesGetFirstChain():BRugs::samplesGetLastChain())
.runCCA.getData()
.runCCA.getLayout(nc)
.runCCA.model(P)
.runCCA.objFun(P)
.runCCA.plotACF(file=ttcall(PBStool)$CCAhist,
   clrs=c("blue","red","green","magenta","navy"))
.runCCA.plotData()
.runCCA.plotDens(file=ttcall(PBStool)$CCAhist,
   clrs=c("blue","red","green","magenta","navy"))
.runCCA.plotHist()
.runCCA.plotNLM()
.runCCA.plotPairs()
.runCCA.plotTrace(file=ttcall(PBStool)$CCAhist,
   clrs=c("blue","red","green","magenta","navy"))
.runCCA.setVals()
.runCCA.updateMod()
}
\arguments{
  \item{fnam}{\code{character} -- string specifying file name}
  \item{hnam}{\code{character} -- string name of a history file}
  \item{ioenv}{\code{environment} -- input/output environment for function input data and output results.}
  \item{...}{\code{dots} -- additional arguments (not currently used).}
  \item{x}{\code{numeric} -- matrix of parameter estimates}
  \item{chains}{\code{numeric} -- index of chain samples}
  \item{nc}{\code{numeric} -- number of chains}
  \item{P}{\code{numeric} -- vector of parameter estimates (Z, alpha, betak, tau, rho)}
  \item{file}{\code{numeric} -- matrix of sample history}
  \item{clrs}{\code{character} -- colour names for the chains}
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
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-29}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{plotProp}}\cr
In package \pkg{PBSdata}:\cr
  \code{\link[PBSdata]{nage394}}
}
\keyword{hplot}
\keyword{optimize}
\keyword{iteration}
\concept{M03_Fishery}

