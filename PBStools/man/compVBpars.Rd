\name{compVBpars}
\alias{compVBpars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Compare VB Parameters
}
\description{
  Compare MCMC samples of derived parameters from von Bertalanffy models.
}
\usage{
compVBpars(bfiles, prefix="vbstan.barf.", 
   pnams=c("linf","k","t0"), redo.QVB=TRUE, quants=c(0.05,0.25,0.5,0.75,0.95),
   bcol=c("orange", "green4"), bgrp=c("female","male"), 
   outnam, png=FALSE, pngres=400, PIN=c(8,8), lang=c("e","f"))
}
\arguments{
  \item{bfiles}{\code{character} -- string names of binary files from a collection of MCMC VB fits using Stan.}
  \item{prefix}{\code{character} -- string prefix for input and possibly output file names.}
  \item{pnams}{\code{character} -- parameter names in MCMC output from Stan fits.}
  \item{redo.QVB}{\code{logical} -- if \code{TRUE}, recalculate quantile values for each parameter from the MCMC samples.}
  \item{quants}{\code{numeric} -- vector of five quantiles to use for the quantile plots.}
  \item{bcol}{\code{character} -- box colours.}
  \item{bgrp}{\code{numeric} -- box groups.}
  \item{outnam}{\code{character} -- optional output name to override an internally generated one.}
  \item{png}{\code{logical} -- if \code{TRUE}, send the figure ouput to a \code{'.png'} file.}
  \item{pngres}{\code{numeric} -- resolution of output figure (pixels per inch).}
  \item{PIN}{\code{numeric} -- width and height of output figure (inches).}
  \item{lang}{\code{character} -- vector of letters specifying language in figures (\code{"e"}= english, \code{"f"}= french.}
}
\value{
  Figure where each panel depicts a VB parameter, and horizontal boxplots show
  the quantile distribution of MCMC samples from various Stan fits.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2021-06-15}
}
\note{
  Stan model code currently supplied by Sean Anderson (PBS, DFO Science).
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{calcVB}},
  \code{\link[PBStools]{compVB}},
  \code{\link[PBStools]{quantBox}},
  \code{\link[PBStools]{residVB}}.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{hplot}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% Use only one concept per line.
\concept{Growth models}
\concept{M06_Assessment}
