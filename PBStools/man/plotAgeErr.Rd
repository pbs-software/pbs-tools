\name{plotAgeErr}
\alias{plotAgeErr}
\title{
  Plot Ageing Error
}
\description{
  Plot ageing precision data, comparing ages from a primary reader to 
  those from secondary ones (precision readers).
}
\usage{
plotAgeErr(dat, nsamp, xlim=NULL, ylim=NULL, jitter=0.25, seed=42,
   png=FALSE, pngres=400, PIN=c(8,8), lang=c("e","f"))
}
\arguments{
  \item{dat}{\code{data.frame} -- data frame created by call to \code{gfb_age_precision.sql}.}
  \item{nsamp}{\code{numeric}  -- number of fish to randomly sample for plot.}
  \item{xlim}{\code{numeric}   -- limits for the x-axis (probably in years).}
  \item{ylim}{\code{numeric}   -- limits for the y-axis (probably in years).}
  \item{jitter}{\code{numeric} -- maximum value used to randomly sample from a uniform
    distribution for visualization (done separately for x and y); same jitter values are
    used for the ages estimated by primary and precision (secondary) readers for the same fish.}
  \item{seed}{\code{numeric}   -- seed value: if numeric value, set the random seed so
    that the same rows are sampled each time and the same jitter values are generated;
    if \code{NULL}, different fish will be sampled each time the function is run.}
  \item{png}{\code{logical}    -- if \code{TRUE}, send the figure to a \code{.png} output file.}
  \item{pngres}{\code{numeric} -- resolution (pixels/inch) for the \code{.png} figure.}
  \item{PIN}{\code{numeric} -- two-element vector specifying output size (inches) 
    as width and height for the \code{.png} figure.}
  \item{lang}{\code{character} -- a vector of letters that denote the language for output:
    currently only \code{"e"} (english) and \code{"f"} (french).}
}
\details{
  Plot is used to demonstrate the ageing error (or precision) for a species.
  The age ranges come from 
  \href{http://www.pac.dfo-mpo.gc.ca/science/species-especes/agelab-scalimetrie/index-eng.html}{SCL} 
  personnel's estimated minimum and maximum ages for each reading based on expert judgement.
}
\value{
  Data frame used to plot the jittered output.
  The processed data file \code{ragout} is store in the \code{PBStools}
  environment; use \code{ttget{ragout}} to retrieve it.
}
\references{
  Modified from code by \href{https://github.com/pbs-assess/gfplot/blob/master/R/ageing-precision.R}{Sean Anderson}.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-27}
}
\seealso{
  \code{\link[PBStools]{calcVB}},
  \code{\link[PBStools]{compAF}},
  \code{\link[PBStools]{crossTab}},
  \code{\link[PBStools]{estOgive}},
  \code{\link[PBStools]{getData}},
  \code{\link[PBStools]{plotProp}},
  \code{\link[PBStools]{quantAges}},
  \code{\link[PBStools]{requestAges}},
  \code{\link[PBStools]{ttget}},
  \code{\link[PBStools]{weightBio}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}% use one of  RShowDoc("KEYWORDS")
\keyword{manip}% __ONLY ONE__ keyword per line
\concept{M06_Assessment}
