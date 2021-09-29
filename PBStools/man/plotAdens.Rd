\name{plotAdens}
\alias{plotAdens}
\title{
Plot Age Density Curves
}
\description{
Plot density of field (e.g. length) by age as waveforms (density curves).
}
\usage{
plotAdens(dat, xfld="len", yfld="age", strSpp="417", sd=3, 
   xlim=NULL, yspan=2, yrel=TRUE, dcol,
   png=FALSE, pngres=400, PIN=c(8, 8), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{\code{data.frame} -- data object created by a called to \code{'gfb_bio.sql'} and processed by \code{'processBio'}.}
  \item{xfld}{\code{character} -- field name in \code{'dat'} used to create densities by \code{'yfld'}.}
  \item{xfld}{\code{character} -- field name in \code{'dat'} used to group densities of \code{'xfld'}.}
  \item{strSpp}{\code{character} -- string species code, usually Hart code (page number as a string).}
  \item{sd}{\code{numeric} -- standard deviations to spread density across \code{'x'}.}
  \item{xlim}{\code{numeric} -- limits of the x-axis.}
  \item{yspan}{\code{numeric} -- vertical adjustment for contrast.}
  \item{yrel}{\code{logical} -- if \code{TRUE}, convert y-values relative to y-max.}
  \item{dcol}{\code{character} -- colours of the density curves (defaults to a blue colour ramp).}
  \item{png}{\code{logical} -- if \code{TRUE}, send the figure to a \code{'.png'} file.}
  \item{pngres}{\code{numeric} -- resolution of output figure (pixels per inch).}
  \item{PIN}{\code{numeric} -- width and height of output figure (inches).}
  \item{\dots}{additional values for the function \code{graphics::lines}.}
}
\details{
  Creates a waveform plot (preliminary at this point). 
  Can be used to visualise lengths-at-age, but could be adapted to follow cohorts by year.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2021-09-28}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{compAF}},
  \code{\link[PBStools]{compLen}},
  \code{\link[PBStools]{histTail}},
  \code{\link[PBStools]{plotAgeErr}},
  \code{\link[PBStools]{processBio}},
  \code{\link[PBStools]{quantAges}},
  \code{\link[PBStools]{weightBio}}.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{hplot}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M02_Biology}
% Use only one concept per line.

