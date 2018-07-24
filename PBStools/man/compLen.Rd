\name{compLen}
\alias{compLen}
\title{
  Compare Lengths Among Surveys
}
\description{
  Compare annual quantiles of fish length (or age) among synoptic surveys.
  Used to detect consistent size differencs along the BC coast to suggest stock delineation.
}
\usage{
compLen(dat, fld="len", ssid=c(16,1,4), sex=c(2,1), strat=FALSE, 
   boot=FALSE, R=10, lbin=1, bxpsep=0.2, bxpcol="black", ylim=NULL, 
   png=FALSE, PIN=c(8, 8), lang=c("e","f"))
}
\arguments{
  \item{dat}{\code{data.frame} -- usually a biological object created by \code{gfb_bio.sql}.}
  \item{fld}{\code{character} -- field name containing metric (e.g., \code{len}) for comparison.}
  \item{ssid}{\code{numeric} -- survey series ID number.}
  \item{sex}{\code{numeric} -- sex code in \code{GFBioSQL}, where 1=males and 2=females.}
  \item{strat}{\code{logical} -- if \code{TRUE}, stratify the metric by the survey strata.}
  \item{boot}{\code{logical} -- if \code{TRUE}, bootstrap the stratified data.}
  \item{R}{\code{numeric} -- number of bootstrap replicates.}
  \item{lbin}{\code{numeric} -- size of each length bin (cm).}
  \item{bxpsep}{\code{numeric} -- space between mulitiple survey series boxes in any given year.}
  \item{bxpcol}{\code{character} -- outline colour of boxes.}
  \item{ylim}{\code{numeric} -- limits of the y-axis.}
  \item{png}{\code{logical} -- if \code{TRUE}, send the figure to a \code{.png} file.}
  \item{PIN}{\code{numeric} -- two-element vector specifying the width and height (both in inches) of the \code{.png} file.}
  \item{lang}{\code{character} -- vector of letters specifying language in figures (\code{"e"}= english, \code{"f"}= french.}
}
\details{
  The default quantile plots show the distribution of the raw data.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus: Institute of Ocean Sciences (IOS), Sidney BC}\cr
  Last modified \code{Rd: 2018-07-17}
}
\note{
  Developed to demonstrate and support stock separation for Redstripe Rockfish.
}
\seealso{
\code{\link[PBStools]{calcStockArea}},
\code{\link[PBStools]{calcVB}},
\code{\link[PBStools]{compAF}},
\code{\link[PBStools]{estOgive}},
\code{\link[PBStools]{getData}},
\code{\link[PBStools]{histMetric}},
\code{\link[PBStools]{makeSSID}},
\code{\link[PBStools]{plotEO}},
\code{\link[PBStools]{plotLocal}},
\code{\link[PBStools]{preferDepth}},
\code{\link[PBStools]{quantBox}},
\code{\link[PBStools]{SQLcode}},
\code{\link[PBStools]{surveyCatch}},
\code{\link[PBStools]{weightBio}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
