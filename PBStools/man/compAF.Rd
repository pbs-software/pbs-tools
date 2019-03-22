\name{compAF}
\alias{compAF}
\title{
  Compare Age Frequencies
}
\description{
  Compare age frequencies between (or among) categories in a user-defined
  group (e.g., group by gear type -- bottom trawl vs. midwater trawl)
  using plots of discrete or cumulative frequency.
}
\usage{
compAF(x, year=2003, sex=2, amax=40, pfld="wp",
   png=FALSE, pngres=400, PIN=c(10,7.5),
   outnam, clrs=c("red","black"), ltys=1, 
   type="cumul", lang=c("e","f"))
}
\arguments{
  \item{x}{\code{list} -- list of arrays by group category containing
    age frequency data by age, year, sex, and stats (e.g., number of
    B&B otoliths by age, annual proportion by age, etc.).}
  \item{year}{\code{numeric} -- vector of years, one per panel.}
  \item{sex}{\code{numeric} -- vector of GFBioSQL sex codes (e.g., 1=males, 2=females).}
  \item{amax}{\code{numeric} -- maximum age to use as a plus class.}
  \item{pfld}{\code{character} -- name of field in AF arrays that contains the data you wish to compare.}
  \item{png}{\code{logical} -- if \code{TRUE}, send the figure to a \code{png} device.}
  \item{pngres}{\code{numeric} -- resolution in pixels per inch for the png file.}
  \item{PIN}{\code{numeric} -- vector specifying width and height for output graphics file.}
  \item{outnam}{\code{character} -- name for the output file.}
  \item{clrs}{\code{character} -- vector of colour names of the lines by category.}
  \item{ltys}{\code{numeric} -- vector of line types by category.}
  \item{type}{\code{character} -- type of line plot: \code{"cumul"} 
    for cumulative or \code{"discr"} for discrete frequency distributuon.}
  \item{lang}{\code{character} -- a vector of letters that denote the language for output:
    currently only \code{"e"} (english) and \code{"f"} (french).}
}
\details{
  Plot annual age frequencies by group category to compare the
  distribution of ages (or some other discrete variable) between 
  (or among) categories in a group -- e.g., bottom trawl vs. midwater
  trawl gear, unsorted vs. sorted samples.
  Each panel shows one year; some panels may be missing data for a particular
  category in the group.
}
\value{
  No value returned currently.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2019-03-21}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{estOgive}},
  \code{\link[PBStools]{genPa}},
  \code{\link[PBStools]{plotProp}},
  \code{\link[PBStools]{quantAges}},
  \code{\link[PBStools]{requestAges}},
  \code{\link[PBStools]{weightBio}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}% use one of  RShowDoc("KEYWORDS")

