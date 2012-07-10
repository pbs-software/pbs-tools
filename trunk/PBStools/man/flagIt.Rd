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
flagIt(a, b, A = 45, r = 0.2, n = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a,b}{midpoint coordinate of a circle}
  \item{A}{angle (degrees) to radiate out from coordinate}
  \item{r}{radius of the circle}
  \item{n}{number of times to decrement the radius (max=5)}
  \item{\dots}{additional \code{par} parameters for the \code{text} function.}
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
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
\code{\link[PBStools]{estOgive}}, \code{\link[PBStools]{scaleVec}}
}
\examples{
pbsfun = function() {
  plot(0,0,type="n",xlim=c(0,20),ylim=c(0,1))
  points(10,0.5,pch=20,col="blue")
  for (i in seq(10,360,10))
    flagIt(a=10, b=0.5, r=0.25, A=i,col="blue",cex=0.7)
}; pbsfun()
}
\keyword{aplot}

