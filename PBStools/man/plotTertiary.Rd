\name{plotTertiary}
\alias{plotTertiary}
\title{
  Plot a Compositional Diagram Within a Polygonal Space
}
\description{
  Plot a compositional diagram using data classified into \eqn{n} categories.
}
\usage{
plotTertiary(x=c(100,5,25,10,50), pC=c(0.5,0.5), r=0.5,
   diag=FALSE, eps=FALSE, wmf=FALSE)
}
\arguments{
  \item{x}{compositional data: single \eqn{n}-element vector or a matrix/data
    frame with \eqn{n} columns. The latter can have row and column names
    which will be used in the plot.}
  \item{pC}{\eqn{(x,y)} coordinates of the polygon centroid.}
  \item{r}{radial arm length from the centroid to each vertex.}
  \item{diag}{logical: if \code{TRUE}, add some diagnostic vectors to the polygon (mostly for debugging).}
  \item{eps}{logical: if \code{TRUE}, the plot is sent to a postrcript \code{.eps} file.}
  \item{wmf}{logical: if \code{TRUE}, the plot is sent to a Windows metafile \code{.wmf}.}
}
\details{
  This function seeks to extend the ternary diagram from \eqn{3} to \eqn{n}
  proportions. However, the geometry of mutiple vertices (squares, pentagons,
  hexagons, etc.) does not translate into deterministic solutions as it does
  for triangles (see Aitchison 1986,  Schnute and Haigh 2007).

  When only one proportion set (vector or single-row matrix) is supplied,
  the function shows lines that connect the mode to the vertices and to each
  side of the polygon. When multiple proportion sets are supplied, the modes
  are connected together to form a trace diagram.
}
\value{
  Invsibly returns the modal point(s).

  Additionally, a number of calculated values within the function are written
  to the list object \code{PBStool}  located in the \code{.PBStoolEnv}
  environment. Use \code{ttget(PBStool)} to transfer the object to your local
  working environment, or use \code{ttprint(PBStool)} to print the contents
  on the command console.
}
\references{
  Aitchison, J. (1986, reprinted in 2003). 
  The Statistical Analysis of Compositional Data. 
  The Blackburn Press, Caldwell, NJ. 416 p.

  Schnute, J.T., and Haigh, R. (2007).
  Compositional analysis of catch curve data, with an application to \emph{Sebastes maliger}.
  \emph{ICES Journal of Marine Science} \bold{64}: 218--233.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-02-27}
}
\seealso{
  \code{\link[PBStools]{plotTernary}}, \code{\link[PBStools]{ttget}}
}
\examples{
local(envir=.PBStoolEnv,expr={
  pbsfun=function(){
    plotTertiary(c(1,1,2,2,5,5))
    invisible() }
  pbsfun()
})
}
\keyword{hplot}
\concept{M05_Spatial}
