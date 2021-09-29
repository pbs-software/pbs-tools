\name{plotGREFS}
\alias{plotGREFS}
\title{
Plot Gamma by Reference Year
}
\description{
Plot the gamma value in \code{'buildCatch'} (e.g. landed RRF/landed ORF) by year, major area, and fishery ID to visualise its change over time.
}
\usage{
plotGREFS(dat, years=1996:2019, majors=3:9, fid=1, strSpp="394", addRGM=FALSE, 
   onefig=FALSE, vlines, png=FALSE, pngres=400, PIN=c(12, 9))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{\code{array} -- three-dimensional array (year, major, fid) produced by the function \code{'buildCatch'}, which saves the object to a binary \code{'.rda'} file that can be loaded into the R console at a later date.}
  \item{years}{\code{numeric} -- reference years to display along the x-axis.}
  \item{majors}{\code{numeric} -- major PMFC areas to display as lines.}
  \item{fid}{\code{numeric} -- fishery IDs to display in separate panels.}
  \item{strSpp}{\code{character} -- string species code, usually Hart code (page number as a string).}
  \item{addRGM}{\code{logical} -- if \code{TRUE}, add running geometric mean.}
  \item{onefig}{\code{logical} -- if \code{TRUE}, create a separate figure for each FID.}
  \item{vlines}{\code{numeric} -- values (years) along the x-axis to add vertical dashed lines for visial delineation.}
  \item{png}{\code{logical} -- if \code{TRUE}, send the figure(s) to a \code{'.png'} file.}
  \item{pngres}{\code{numeric} -- resolution of output figure (pixels per inch).}
  \item{PIN}{\code{numeric} -- width and height of output figure (inches).}
}
\details{
  Provides graphical check on how gamma changes in potential reference years; helps user to choose reference years for each fishery.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2021-09-28}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{buildCatch}},
  \code{\link[PBStools]{plotDiag}},
  \code{\link[PBStools]{plotRecon}}.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{hplot}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M07_CatchRecon}
% Use only one concept per line.
