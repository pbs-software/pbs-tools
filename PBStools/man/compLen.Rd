\name{compLen}
\alias{compLen}
\title{
  Compare Lengths Among Groups
}
\description{
  Compare annual quantiles of fish length (or age) among groups (e.g., synoptic surveys, commercial gear types).
  Used to detect consistent size differencs along the BC coast to suggest stock delineation.
}
\usage{
compLen(dat, strSpp, fld="len", lbin=1, sex=c(2,1), 
   gfld="SSID", gval=list(16,1,4), yrs, ttype, stype, scat, exlax,
   strat=FALSE, boot=FALSE, R=10, bxpsep=0.2, bxpcol="black",
   ylim=NULL, png=FALSE, PIN=c(8,8), lang=c("e","f"))
}
\arguments{
  \item{dat}{\code{data.frame} -- usually a biological object created by \code{gfb_bio.sql}.}
  \item{strSpp}{\code{character} -- alpha-numeric species code (usually Hart code).}
  \item{fld}{\code{character} -- field name containing metric (e.g., \code{"len"}) for comparison.}
  \item{lbin}{\code{numeric} -- size of each length bin (cm).}
  \item{sex}{\code{numeric} -- sex code in \code{GFBioSQL}, where 1=males and 2=females.}
  \item{gfld}{\code{character} -- field name containing groups for annual side-by-side comparisons (e.g., \code{"SSID"}).}
  \item{gval}{\code{numeric} -- list of values in \code{gfld} to use (e.g., 1=QCS synoptic, 4=WCVI synoptic, 16=WCHG synoptic)}
  \item{yrs}{\code{numeric} -- vector of years to qualify data.}
  \item{ttype}{\code{numeric} -- vector of trip type codes to qualify data.}
  \item{stype}{\code{numeric} -- vector of sample type codes to qualify data.}
  \item{scat}{\code{numeric} -- vector of species category codes (e.g. 1=unsorted) to qualify data.}
  \item{exlax}{\code{character} -- extra text to use in figure file name.}
  \item{strat}{\code{logical} -- if \code{TRUE}, stratify the metric by the survey strata.}
  \item{boot}{\code{logical} -- if \code{TRUE}, bootstrap the stratified data.}
  \item{R}{\code{numeric} -- number of bootstrap replicates.}
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
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2018-12-21}
}
\note{
  Developed to demonstrate and support stock separation for Redstripe Rockfish.
  Functionality expanded during data exploration of Widow Rockfish to compare lengths
  between commercial bottom and midwater trawl gear.
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
