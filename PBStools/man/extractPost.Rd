\name{extractPost}
\alias{extractPost}
\title{
  Extract Model Posteriors
}
\description{
  Extract model posteriors from three model platforms (Awatea, SS3, iSCAM) for clients.
}
\usage{
extractPost(spc="BOR", stock="CST", assYr=2021, path=getwd(),
   values="R", runs=1:3, rwts=rep(1,3), vers="", burnin=200, nmcmc=1200,
   model="Awatea", fleets="trawl", extra="forSomeone",
   proj=FALSE, projY1=2022, projY2=2023, catpol=1500)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spc}{\code{character} -- species 3-letter code}
  \item{stock}{\code{character} -- stock name (e.g., \code{'CST'}, \code{'North'}, \code{'5DE'})}
  \item{assYr}{\code{numeric} -- year of stock assessment}
  \item{path}{\code{character} -- path/directory to model runs}
  \item{values}{\code{character} -- acronyms for particular posteriors:\cr
    \code{'B'} = spawning biomass (usually female)\cr
    \code{'R'} = recruitment of age-1 or age-0 fish (depends on model)\cr
    \code{'Rdev'} = recruitment deviations\cr
    \code{'U'} = exploitation/harvest rate = \code{1 - exp(-F)}\cr
    \code{'P'} = model parameters\cr
    \code{'HRP'} = historical reference points, usually \code{'Bavg'} and \code{'Bmin'} }
  \item{runs}{\code{numeric} -- vector of run numbers used to make a composite base case.}
  \item{rwts}{\code{numeric} -- vector of reweight numbers used in the respective run numbers.}
  \item{vers}{\code{character} -- version number for a particular \code{run} (first introduced for POP 2023)}
  \item{burnin}{\code{numeric} -- number of MCMC samples used for burn-in (these are discarded).}
  \item{nmcmc}{\code{numeric} -- total number of MCMC samples, including burn-in.}
  \item{model}{\code{character} -- model platform, one of \code{'Awatea'}, \code{'SS3'}, \code{'iSCAM'}.}
  \item{fleets}{\code{character} -- fleet names uses to label catches.}
  \item{extra}{\code{character} -- additional text to append to file's output name.}
  \item{proj}{\code{logical} -- if \code{TRUE}, extract projections (currently only used for GMU's concerns about Bocaccio).}
  \item{projY1}{\code{numeric} -- year to start the exrapolation of projection exploitation.}
  \item{projY2}{\code{numeric} -- year to exrapolate to using the exploitation rates estimated in \code{'projY1'}.}
  \item{catpol}{\code{numeric} -- catch policy to use from the exploitation rates in \code{'projY1'}.}
}
\details{
  For models \code{'Awatea'} and \code{'iSCAM'}, code loops throught the designated \code{'runs'} and rebuilds the posterior.
  For \code{'SS3'}, this process was previously done by the function \code{'PBSsynth::gatherMCMC'} and stored in a binary object called \code{'compo.YYMMDD'}.
}
\value{
  A series of \code{'csv'} (text) files that contain the posteriors for \code{'values'} specified, one file per value.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-29}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{calcStockArea}},
  \code{\link[PBStools]{getFile}}\cr
In package \pkg{PBSsynth}:\cr
  \code{gatherMCMC}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
\keyword{IO}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M01_Utility}
% \concept{ ~cpt2 }
% Use only one concept per line.
