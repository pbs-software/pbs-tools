\name{quantAges}
\alias{quantAges}
\title{ Plot Ages as Quantiles }
\description{
  Plot quantile boxes of age by year and/or area, including mean age over time.
}
\usage{
quantAges(bioDat, dfld="age", afld="major", tfld="year",
   type="time", outnam="Quant-Age", png=FALSE)
}
\arguments{
  \item{bioDat}{\code{data.frame} -- Biological data from query \code{gfb_bio.sql}.}
  \item{dfld}{\code{character} -- Name of data field in \code{bioDat} (usually \code{"age"}).}
  \item{afld}{\code{character} -- Name of area field in \code{bioDat} (usually \code{"major"}).}
  \item{tfld}{\code{character} -- Name of time field in \code{bioDat} (usually \code{"year"}).}
  \item{type}{\code{character} -- Type of plot, either \code{"area"} or \code{"time"}.}
  \item{outnam}{\code{character} -- Output name for figure plot.}
  \item{png}{\code{logical} -- if \code{TRUE}, send figure to a portable network graphics (\code{.png}) file.}
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
  Rowan Haigh, Research Biologist\cr
  Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
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

