\name{plotMW}
\alias{plotMW}
\title{
  Plot Mean Weight
}
\description{
  Plot annual mean weights by stock and add annual mean weights 
  by individual areas that occur in the bigger stock areas.
}
\usage{
plotMW(dat, xlim, ylim, outnam="Mean-Weight-Compare",
   outcast, png=FALSE, lang=c("e","f"))
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{\code{data.frame} -- mean weight data table where first
    column contains the year and subsequent columns give annual mean
    weights by stock area and PMFC area.}
  \item{xlim}{\code{numeric} -- limits of the x-axis (years);
    determined automatically if not specified.}
  \item{ylim}{\code{numeric} -- limits of the y-axis (weight);
    determined automatically if not specified.}
  \item{outnam}{\code{character} -- name of the output \code{.png} file.}
  \item{outcast}{\code{list} -- various controls for the plot: \cr
    \code{"scol", "spch", "slty", "slwd", "scex", "sord"}.}
  \item{png}{\code{logical} -- if \code{TRUE}, send the figure to a \code{.png} file.}
  \item{lang}{\code{character} -- vector of letters specifying language in figures
    (\code{"e"}= english, \code{"f"}= french.}
}
\details{
  Designed to highlight the difference in annual commerical mean
  weight between stocks, and to show how mean weight trends in 
  individual PMFC areas compare to those of the stock areas.
}
\value{
  Returns nothing at present.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-27}
}
\note{
  The code is fixed at the moment for Redstripe Rockfish (RSR)
  but this can be modified over time as needed.
}
\seealso{
\code{\link[PBStools]{calcStockArea}},
\code{\link[PBStools]{compLen}},
\code{\link[PBStools]{getFile}},
\code{\link[PBStools]{plotLocal}}

\code{\link[PBSdata]{pmfc}} in \code{PBSdata}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
\concept{M06_Assessment}
