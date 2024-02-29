\name{plotTernary}
\alias{plotTernary}
\title{
  Plot a Ternary Diagram
}
\description{
  Plot a ternary diagram using compositional data classified
  into three categories.
}
\usage{
plotTernary(x=c(3,2,1), connect=FALSE, show.geometry=TRUE, 
   bw=FALSE, eps=FALSE, wmf=FALSE)
}
\arguments{
  \item{x}{compositional data: single three-element vector or a matrix/data
    frame with three columns. The latter can have row and column names
    which will be used in the plot.}
  \item{connect}{logical: if \code{TRUE}, multiple events (rows) will be 
    connected together in the ternary diagram.}
  \item{show.geometry}{logical: if \code{TRUE}, the vectors \eqn{p_1}, \eqn{p_2},
    and \eqn{p_3} will be drawn, as well as lines connecting \eqn{(x,y)} to each
    vertex.}
  \item{bw}{logical: if \code{TRUE}, the plot is rendered without colour.}
  \item{eps}{logical: if \code{TRUE}, the plot is sent to a postrcript \code{.eps} file.}
  \item{wmf}{logical: if \code{TRUE}, the plot is sent to a Windows metafile \code{.wmf}.}
}
\details{
  When population components are amalgamated into \eqn{g=3} groups, vectors of 
  proportions can be portrayed in a graph called a ternary diagram 
  (Aitchison, 1986, p. 5). The diagram begins with an equilateral triangle
  that has vertices labelled \dQuote{1}, \dQuote{2}, and \dQuote{3}. 
  A vector \bold{p} \eqn{(p_1, p_2, p_3)} of proportions can then be 
  represented as a point within this triangle, where the perpendicular
  distance to the side opposite vertex \dQuote{i} is proportional to \eqn{p_i}.
}
\value{
  Nada at present.
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
  \code{\link[PBStools]{runCCA}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun=function(){
  plotTernary(c(1/2,1/3,1/6))
  invisible() }
pbsfun()
})
}
\keyword{hplot}
\concept{M05_Spatial}
