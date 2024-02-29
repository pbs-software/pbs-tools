\name{flagIt}
\alias{flagIt}
\title{
  Flag a Coordinate
}
\description{
  Take a coordinate \code{(a,b)} and label it using a diagonal line 
  of radius \code{r} and angle \code{A}.
}
\usage{
flagIt(a, b, A=45, r=0.2, n=1, lab, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a,b}{\code{numeric} -- midpoint coordinate of a circle}
  \item{A}{\code{numeric} -- angle (degrees) to radiate out from coordinate}
  \item{r}{\code{numeric} -- radius of the circle}
  \item{n}{\code{numeric} -- number of times to decrement the radius (max=5)}
  \item{lab}{\code{character} -- optional label to precede the flagged coordinate}
  \item{\dots}{\code{dots} -- additional \code{par} parameters for the \code{text} function}
}
\details{
  A diagonal dotted line in light grey radiates out from a central coordinate
  and the coordinate values are used to label the coordinate itself.
  The function adjusts for the aspect ratio created by different x- and y-limits,
  and for different x- and y-dimensions of the plot. This ensures that an angle looks
  correct in the plotting space.
}
\value{
  Invisibly returns a list of the vectors (a,x) and (b,y), the angle in 
  radians, the original x-value calculated in a square Cartesian system, 
  and the final x- and y-coordinates on the periphery of a circle.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2018-12-10}
}
\seealso{
\code{\link[PBStools]{estOgive}}, \code{\link[PBStools]{scaleVec}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun = function() {
  plot(0,0,type="n",xlim=c(0,20),ylim=c(0,1))
  points(10,0.5,pch=20,col="blue")
  for (i in seq(10,360,10))
    flagIt(a=10, b=0.5, r=0.25, A=i,col="blue",cex=0.7)
}; pbsfun()
})
}
\keyword{aplot}
\concept{M01_Utility}

