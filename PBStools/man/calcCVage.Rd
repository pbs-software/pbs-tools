\name{calcCVage}
\alias{calcCVage}
\title{
Calculate CVs at Age
}
\description{
Calculate CVs for each age based on (i) observed lengths-at-age, or (ii) otolith readers' reported precision ranges.
}
\usage{
calcCVage(dat, read_prim=2, read_prec=NULL, cvtype="age",
   Amax=50, min.ages=1:4, min.cv=0.05, interp=FALSE, smooth=TRUE,
   tabs=TRUE, plot=TRUE, png=FALSE, pngres=400, PIN=c(8,6), outnam,
   lang=c("e","f"))
}
\arguments{
  \item{dat}{\code{data.frame} -- species-specific results from a query to \code{'gfb_vbgf.sql'}}
  \item{read_prim}{\code{numeric} -- primary otolith reader code}
  \item{read_prec}{\code{character} -- precision otolith reader code}
  \item{cvtype}{\code{character} -- type of data from which to calculate CVs
    (\code{'age'} for otolith readers precision estimates;
    \code{'len'} for observed lengths at-age)}
  \item{Amax}{\code{numeric} -- maximum age for calculating CVs-at-age}
  \item{min.ages}{\code{numeric} -- minimum ages for disregarding the data and 
    adopting a minimum CV (\code{'min.cv'})}
  \item{min.cv}{\code{numeric} -- CV used for minimum ages; CVs for missing ages 
    greater than \code{'min.ages'} are interpolated from adjacent CVs}
  \item{interp}{\code{logical} -- if \code{TRUE}, linearly interpolate missing values}
  \item{smooth}{\code{logical} -- if \code{TRUE}, run a loess-smoother through the calculated standard deviations}
  \item{tabs}{\code{logical} -- if \code{TRUE}, write results tables to \code{.csv} files}
  \item{plot}{\code{logical} -- if \code{TRUE}, plot the results as a histogram}
  \item{png}{\code{logical} -- if \code{TRUE}, send the figure to a \code{'.png'} file}
  \item{pngres}{\code{numeric} -- resolution of output figure (pixels per inch)}
  \item{PIN}{\code{numeric} -- width and height of output figure (inches)}
  \item{outnam}{\code{character} -- optional output name to override an internally generated one}
  \item{lang}{\code{character} -- a letter that denotes the language for output:
    currently only \code{'e'} (english) or \code{'f'} (french)}
}
\details{
  Used to generate ageing error vectors and/or matrices.
}
\value{
  A matrix where each rows represents an age and four column with names \code{'N'}
  (number observations), \code{'MU'} (mean age/len),\code{'SD'} (standard deviation
  of mean age/len), and \code{'CV'} (coefficient of variation).
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-12-19}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{extractAges}},
  \code{\link[PBStools]{makeAgeErr}},
  \code{\link[PBStools]{plotAgeErr}},
  \code{\link[PBStools]{quantAges}},
  \code{\link[PBStools]{requestAges}},
  \code{\link[PBStools]{runCCA}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{hplot}
\keyword{manip}
% For non-standard keywords, use \concept instead of \keyword:
\concept{M06_Assessment}
% Use only one concept per line.
