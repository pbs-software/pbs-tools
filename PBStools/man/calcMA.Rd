\name{calcMA}
\alias{calcMA}
\title{
  Calculate Moving Average of a Series
}
\description{
  Calculate a moving average of a series using a fixed
  period occurring every number of specified base units (usually days).
}
\usage{
calcMA(x, y, y2, period = 270, every = 10)
}
\arguments{
  \item{x}{x-values that define the units over which the series is observed 
    (e.g., dates, years, months, etc.). Must be one of the following classes:
    \code{numeric}, \code{integer}, \code{ts}, \code{POSIXt}, \code{POSIXct},
    \code{POSIXlt}, or \code{Date}.}
  \item{y}{y-values that measure the values to be smoothed by a moving average.}
  \item{y2}{optional y-values that can be used should any \code{y} be missing.}
  \item{period}{period in base units of x (usually days) over which each 
    average in the moving series is calculated.}
  \item{every}{base units of x (usually days) to move the \code{period} forward 
    for successive calculations of the average \code{y}.}
}
\details{
  The moving average is accumulated backwards from the last observation.

  If your series does not depend on any particular \code{x} (e.g., date), set \code{x=1:nrow(y)}.
}
\value{
  A data frame that represents the moving average series. This object comprises
  three columns: \cr
  \code{x} = the end x-value of each period of the moving average; \cr
  \code{y} = the average y-value for each period; \cr
  \code{z} = the numeric value of x (above) taken from the cumulative sum 
  of the input x-units starting at 0 (see below).
  
  Additionally, the moving average data frame above has an attribute called 
  \code{dat} that contains the original input data as a data frame comprising
  three columns: \cr
  \code{x} = the x-value of the input series; \cr
  \code{y} = the y-value of the input series; \cr
  \code{z} = the numeric cumulative sum of the x-units starting at 0.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2025-03-19}\cr
}
\seealso{
  \code{\link[PBStools]{glimmer}}, 
  \code{\link[PBStools]{simBGtrend}}
}
\keyword{manip}
\keyword{smooth}
\concept{M06_Assessment}

