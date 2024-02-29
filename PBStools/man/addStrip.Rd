\name{addStrip}
\alias{addStrip}
\title{
  Add Vertical Colour Strip to Plot
}
\description{
  Add a vertical colour strip as a legend, where each box is labelled.
  Place the strip in a plot using relative (0:1) coordinates.
}
\usage{
addStrip(x, y, col, lab, xwidth = 0.01, yheight = 0.3, ...)
}
\arguments{
  \item{x}{x-axis coordinate of strip's left side in the range (0:1).}
  \item{y}{y-axis coordinate of strip's top side in the range (0:1).}
  \item{col}{Set of colours that make the strip.}
  \item{lab}{Set of labels to place beside each coloured rectangle in the strip.}
  \item{xwidth}{Width of the vertical strip specified in the range (0:1).}
  \item{yheight}{Height of the vertical strip specified in the range (0:1).}
  \item{\dots}{additional arguments (not currently used).}
}
\details{
  This function was created specifically to label barplots, but can be used in any situation.
  The \code{legend} function is too persnickety when trying to close gaps between filled boxes.
  The \code{image} function is also persnickety and provides an overkill solution.
}
\value{
  Nada, rien, zilcho
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2021-06-15}\cr
}
\note{
  Persnickety: placing too much emphasis on trivial or minor details; fussy.
}
\seealso{
  \code{\link[PBStools]{plotRecon}}, \code{\link[PBSmodelling]{addLabel}}, 
  \code{\link[PBSmodelling]{addLegend}}
}
\keyword{aplot}
\concept{M01_Utility}

