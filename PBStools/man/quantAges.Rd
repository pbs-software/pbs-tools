\name{quantAges}
\alias{quantAges}
\title{ Plot Ages as Quantiles }
\description{
  Plot quantile boxes of age by year and/or area, including mean age over time.
}
\usage{
quantAges(bioDat, dfld="age", afld="major", tfld="year",
   type="time", major=3:9, years=1990:2018, ylim=c(0,35), strSpp="RSR",
   outnam="Quant-Age", png=FALSE, pngres=400, PIN=c(8,8),
   scat, lang=c("e","f")) 
}
\arguments{
  \item{bioDat}{\code{data.frame} -- biological data from query \code{gfb_bio.sql}.}
  \item{dfld}{\code{character} -- name of data field in \code{bioDat} (usually \code{"age"}).}
  \item{afld}{\code{character} -- name of area field in \code{bioDat} (usually \code{"major"}).}
  \item{tfld}{\code{character} -- name of time field in \code{bioDat} (usually \code{"year"}).}
  \item{type}{\code{character} -- type of plot, either \code{"area"} or \code{"time"}.}
  \item{major}{\code{numeric} -- codes for major PMFC areas.}
  \item{years}{\code{numeric} -- vector of years.}
  \item{ylim}{\code{numeric} -- limits of the y-axis.}
  \item{strSpp}{\code{character} -- three-letter code for the fish species of interest.}
  \item{outnam}{\code{character} -- output name for figure plot.}
  \item{png}{\code{logical} -- if \code{TRUE}, send figure to a portable network graphics (\code{.png}) file.}
  \item{pngres}{\code{numeric} -- resolution for output file (pixesl/inch).}
  \item{PIN}{\code{numeric} -- vector of (width, height) in inches for output file.}
  \item{scat}{\code{numeric list} -- list of species category codes (\code{GFBio}'s \code{"SPECIES_CATEGORY"} table),
    e.g. \code{list(Unsorted=1, Keepers=3)}.}
  \item{lang}{\code{character} -- a vector of letters that denote the language for output:
    currently only \code{"e"} (english) and \code{"f"} (french).}
}
\details{
  The quantile plots display age-distribution boxes by sex (side-by-side), and separate commercial and survey samples.\cr
  If \code{type=="time"}, multiple panels separate the samples by area and display annual quantiles of age;
  addtionally, a line running through consecutive mean ages is added.\cr
  If \code{type=="area"}, quantile plots show age distributions by area after pooling all years.
}
\value{
  List object of ages placed into lists that are used to construct the quantile plots.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-27}
}
\note{
  Suggested by PJS to detect changes in age by area and over time.
}
\seealso{
  \code{\link[PBStools]{compBmsy}},
  \code{\link[PBStools]{crossTab}},
  \code{\link[PBStools]{plotLocal}},
  \code{\link[PBStools]{plotProp}},
  \code{\link[PBStools]{quantBox}},
  \code{\link[PBStools]{weightBio}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the R documentation directory.
\keyword{hplot}% use one of  RShowDoc("KEYWORDS")
\concept{M06_Assessment}
